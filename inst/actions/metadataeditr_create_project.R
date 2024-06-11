function(action, entity, config){
  
  if(!requireNamespace("metadataeditr", quietly = TRUE)){
    stop("The 'metadataeditr-create-project' action requires the 'metadataeditr' package")
  }
  
  #fetch software
  MD_EDITOR = config$software$output$metadataeditr
  if(is.null(MD_EDITOR)){
    stop("A 'metadataeditr' software must be configured to use this action")
  }
  MD_EDITOR_CONFIG = config$software$output$metadataeditr_config
  collection_names = list()
  if(!is.null(MD_EDITOR_CONFIG$properties$collection_names)){
    collection_names = as.list(strsplit(MD_EDITOR_CONFIG$properties$collection_names, ",")[[1]])
  }
  
  #basic function to map a geoflow_contact to a metadata editor contact
  produce_md_contact = function(x){
    
    md_contact = list()
    
    if(is.null(x$firstName)) x$firstName = NA
    if(is.null(x$lastName)) x$lastName = NA
    if(!is.na(x$firstName) && !is.na(x$lastName)) md_contact$individualName = paste(x$firstName, x$lastName)
    if(!is.na(x$organizationName)) md_contact$organisationName = x$organizationName
    if(!is.na(x$positionName)) md_contact$positionName = x$positionName
    if(!is.na(x$role)) md_contact$role = x$role
    
    md_contact$contactInfo = list()
    md_contact$contactInfo$address = list()
    if(!is.na(x$email)) md_contact$contactInfo$address$electronicMailAddress = x$email
    if(!is.na(x$postalAddress)) md_contact$contactInfo$address$deliveryPoint = x$postalAddress
    if(!is.na(x$city)) md_contact$contactInfo$address$city = x$city
    if(!is.na(x$postalCode)) md_contact$contactInfo$address$postalCode = x$postalCode
    if(!is.na(x$country)) md_contact$contactInfo$address$country = x$country
    md_contact$contactInfo$phone = list()
    if(!is.na(x$voice)) md_contact$contactInfo$phone$voice = x$voice
    if(!is.na(x$facsimile)) md_contact$contactInfo$phone$facsimile = x$facsimile
    md_contact$contactInfo$onlineResource = list()
    if(!is.na(x$websiteUrl)) md_contact$contactInfo$onlineResource$linkage = x$websiteUrl
    if(!is.na(x$websiteName)) md_contact$contactInfo$onlineResource$name = x$websiteName
    
    return(md_contact)
  }
  
  metadata_maintainers = entity$contacts[sapply(entity$contacts, function(x){tolower(x$role) %in% c("metadata")})]
  producers = entity$contacts[sapply(entity$contacts, function(x){tolower(x$role) %in% c("owner","originator")})]
  poc = entity$contacts[sapply(entity$contacts, function(x){!tolower(x$role) %in% c("metadata", "processor")})]
  distributors = entity$contacts[sapply(entity$contacts, function(x){!tolower(x$role) %in% c("distributor")})]
  
  thumbnails = entity$relations[sapply(entity$relations, function(x){x$key == "thumbnail"})]
  #thumbnail management
  if(length(thumbnails)>0){
    dir.create("thumbnails")
    
    thumbnails = lapply(1:length(thumbnails), function(i){
      thumbnail = thumbnails[[i]]
      if(startsWith(thumbnail$link, "http")){
        req_head = httr::HEAD(thumbnail$link)
        if(httr::status_code(req_head) == 200){
          fileext = unlist(strsplit(httr::headers(req_head)[["content-type"]], "image/"))[2]
          filename = paste0("thumbnail_", i, ".", fileext)
          download.file(thumbnail$link, destfile = file.path(getwd(), "thumbnails", filename), mode = "wb")
          thumbnail$link = filename
        }
      }else{
        file.copy(from = thumbnail$link, to = file.path(getwd(), "thumbnails", basename(thumbnail$link)))
        thumbnail$link = basename(thumbnail$link)
      }
      return(thumbnail)
    })
  }
  
  project <- list()
  
  production_date = Sys.Date()
  
  #metadata_information
  project$metadata_information = list(
    title = entity$titles[["title"]],
    producers = lapply(producers, function(x){ 
      contact = produce_md_contact(x)
      name = contact$organisationName
      if(!is.null(contact$individualName)) name = contact$individualName
      list(name = name) 
    }),
    production_date = production_date,
    version = entity$descriptions$edition
  )
  
  #description (~ ISO 19115)
  #description/metadata
  project$description = list(
    idno = entity$identifiers[["id"]],
    language = entity$language,
    characterSet = list(codeListValue = "utf8"),
    hierarchyLevel = entity$types[["generic"]],
    contact = lapply(metadata_maintainers, produce_md_contact),
    dateStamp = production_date,
    metadataStandardName = "ISO 19115:2003/19139"
  )
  
  #description/spatialRepresentationInfo
  project$description$spatialRepresentationInfo = list()
  #spatial representation
  if(!is.null(entity$data)) {
    spatialRepresentationType <- entity$data$spatialRepresentationType
    if(!is.null(spatialRepresentationType)){
      if(spatialRepresentationType=="vector"){
        features = entity$data$features
        if(!is.null(features)){
          #support vector spatial representation
          if(is(features, "sf")){
            geomtypes <- as.list(table(sf::st_geometry_type(features)))
            geomtypes <- geomtypes[geomtypes > 0]
            if(length(geomtypes)>0){
              #spatialRepresentationType <- "vector"
              for(geomtype in names(geomtypes)){
                vsr = list()
                geomLevel <- "geometryOnly"
                if(geomtype == "TIN") geomLevel = "planarGraph"
                if(geomLevel == "geometryOnly"){
                  isoGeomType <- switch(geomtype,
                                        "GEOMETRY" = "composite", "GEOMETRYCOLLECTION" = "composite",
                                        "POINT" = "point", "MULTIPOINT" = "point", 
                                        "LINESTRING" = "curve", "CIRCULARSTRING" = "curve", "MULTILINESTRING" = "curve", "CURVE" = "curve", "COMPOUNDCURVE" = "curve",
                                        "POLYGON" = "surface", "MULTIPOLYGON" = "surface", "TRIANGLE" = "surface",
                                        "CURVEPOLYGON" = "surface", "SURFACE" = "surface", "MULTISURFACE" = "surface",
                                        "POLYHEDRALSURFACE" = "solid"
                  )
                  
                  vsr = list(
                    topologyLevel = geomLevel,
                    geometricObjects = list(
                      list(
                        geometricObjectType = isoGeomType,
                        geometricObjectCount = nrow(features[sf::st_geometry_type(features)==geomtype,])
                      )
                    )
                  )
                }
                project$description$spatialRepresentationInfo[[1]] = list(
                  vectorSpatialRepresentation = vsr
                )
              }
            }else{
              spatialRepresentationType <- "textTable"
            }
          }
        }
      }
      
      if(spatialRepresentationType=="grid"){
        gsr = list()
        gsr$numberOfDimensions = length(entity$data$dimensions)
        for(dimension in names(entity$data$dimensions)){
          dimObject <- list()
          dimObject$dimensionName = dimension
          dimObject$dimensionSize = entity$data$dimensions[[dimension]]$size
          resolution<-entity$data$dimensions[[dimension]]$resolution
          if(!is.null(resolution$value)){
            dimObject$resolution = resolution$value
          }
          gsr$axisDimensionproperties[[length(gsr$axisDimensionproperties)+1]] = dimObject
        }
        gsr$cellGeometry = "area"
        project$description$spatialRepresentationInfo = list(
          gridSpatialRepresentation = gsr
        )
      }
    }
  }
  
  #description/referenceSystemInfo
  project$description$referenceSystemInfo = list()
  if(!is.null(entity$srid)){
    project$description$referenceSystemInfo[[1]] = list(
      code = as.character(entity$srid), 
      codeSpace = "EPSG"
    )
    if(entity$srid == 4326){
      #we add also the WGS one
      project$description$referenceSystemInfo[[2]] = list(
        code = "WGS 84",
        codeSpace = "World Geodetic System (WGS)"
      )
    }
  }
  
  #description/identificationInfo
  project$description$identificationInfo = list(
    citation = list(
      title = entity$titles[["title"]],
      alternateTitle = if(!is.null(entity$titles[["alternative"]])) entity$titles[["alternative"]] else "",
      date = lapply(entity$dates, function(x){
        list(date = x$value, type = x$key)
      }),
      edition = entity$descriptions$edition,
      editionDate = if(any(sapply(entity$dates, function(x){x$key == "edition"}))){
        entity$dates[sapply(entity$dates, function(x){x$key == "edition"})][[1]]$value
      }else "",
      identifier = list(authority = "WB-DECDG", code = entity$identifiers[["id"]]),
      #otherCitationDetails = ""
      citedResponsibleParty = lapply(producers, produce_md_contact)
    ),
    abstract = entity$descriptions$abstract,
    purpose = if(!is.null(entity$descriptions$purpose)) entity$descriptions$purpose else "",
    credit = if(!is.null(entity$descriptions$credit)) entity$descriptions$credit else "",
    status = if(!is.null(entity$descriptions$status)) entity$descriptions$status else "",
    pointOfContact = lapply(poc, produce_md_contact),
    resourceMaintenance = list(
      list(maintenanceOrUpdateFrequency = "asNeeded")
    ),
    graphicOverview= if(length(thumbnails)>0) lapply(1:length(thumbnails), function(i){
      thumbnail = thumbnails[[i]]
      th = list(fileName = thumbnail$link)
      if(!is.null(thumbnail$description)) th$fileDescription = thumbnail$description
      return(th)
    }) else list(),
    resourceFormat = lapply(entity$formats[sapply(entity$formats, function(x){x$key == "resource"})], function(resourceFormat){
      rf = list(name = resourceFormat$name)
      if(!is.null(resourceFormat$description)) rf$specification = resourceFormat$description
      return(rf)
    }),
    descriptiveKeywords = do.call(c, lapply(entity$subjects[sapply(entity$subjects, function(x){return(x$key != "topic")})], function(subject){
      lapply(subject$keywords, function(kwd){
        out_kwd = list(type = subject$key, keyword = kwd$name)
        if(!is.null(subject$name)) out_kwd$thesaurusName = subject$name
        return(out_kwd)
      })
    })),
    resourceConstraints = list(
      list(
        legalConstraints = list(
          useLimitation = lapply(entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "uselimitation"})], function(cons){
            cons$values[[1]]
          }),
          accessConstraints = lapply(entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "accessconstraint"})], function(cons){
            cons$values[[1]]
          }),
          useConstraints = lapply(entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "useconstraint"})], function(cons){
            cons$values[[1]]
          })
        )
      )
    ),
    #resourceSpecificUsage
    #aggregationInfo
    extent = list(
      geographicElement = list(
        list(
          geographicBoundingBox = list(
            southBoundLatitude = entity$spatial_bbox$ymin,
            westBoundLongitude = entity$spatial_bbox$xmin,
            northBoundLatitude = entity$spatial_bbox$ymax,
            eastBoundLongitude = entity$spatial_bbox$xmax
          )
        )
      ),
      temporalElement = list(
        if(!is.null(entity$temporal_extent$instant)){
          list(timePosition = entity$temporal_extent$instant)
        }else if(!is.null(entity$temporal_extent$start) & !is.null(entity$temporal_extent$end)){
          list(beginPosition = entity$temporal_extent$start, endPosition = entity$temporal_extent$end)
        }
      )
    ),
    spatialRepresentationType = entity$data$spatialRepresentationType,
    language = list(entity$language),
    characterSet = list(
      list(codeListValue = "utf8")
    ),
    supplementalInformation = if(!is.null(entity$descriptions$info)) entity$descriptions$info else ""
  )

  #description/distributionInfo
  project$description$distributionInfo = list(
    distributionFormat = lapply(entity$formats[sapply(entity$formats, function(x){x$key == "distribution"})], function(distFormat){
      df = list(name = distFormat$name)
      if(!is.null(distFormat$description)) df$specification = distFormat$description
      return(df)
    }),
    distributor = lapply(distributors, produce_md_contact)
  )
  
  #description/dataQualityInfo
  project$description$dataQualityInfo = list()
  if(!is.null(entity$provenance)){
    project$description$dataQualityInfo = list(
      list(
        lineage = list(
          statement = entity$provenance$statement,
          processStep = lapply(entity$provenance$processes, function(process){
            list(
              description = process$description,
              rationale = process$rationale,
              processor = lapply(process$processors, produce_md_contact)
            )
          })
        )
      )
    ) 
  }
  
  #description/metadataMaintenance
  project$description$metadataMaintenance = list(maintenanceAndUpdateFrequency = "asNeeded")

  #description/contentInfo
  #description/feature_catalogue (common to all metadata standards?)
  
  #creation
  output = metadataeditr::create_project(
    type = "geospatial",
    idno = entity$identifiers[["id"]],
    metadata = project,
    collection_names = collection_names,
    thumbnail = if(length(thumbnails)>0) file.path(getwd(), "thumbnails", thumbnails[[1]]$link) else NULL, #TODO
    overwrite = TRUE
  )
  
  if(output$response$status == "success"){
    config$logger.info(sprintf("Project '%s' successfully submitted to metadata editor", entity$identifiers$id))
  }
  
  #add resources
  #first remove existing resources
  reslist = metadataeditr::resources_list(entity$identifiers[["id"]])
  if(reslist$status_code==200){
    existing_resources = reslist$response$resources
    if(length(existing_resources)>0) for(i in 1:nrow(existing_resources)){
      metadataeditr::resources_delete(idno = entity$identifiers[["id"]], resource_id = existing_resources[i,]$id)
    }
  }
  
  #thumbnails
  if(length(thumbnails)>0){
    for(thumbnail in thumbnails){
      metadataeditr::resources_add(
        idno = entity$identifiers[["id"]],
        dctype = "pic",
        title = thumbnail$name,
        file_path = file.path(getwd(), "thumbnails", thumbnail$link)
      )   
    }
  }
}