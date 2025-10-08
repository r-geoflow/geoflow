function(action, entity, config){
  
  skipEnrichWithData = if(!is.null(config$profile$options$skipEnrichWithData)) config$profile$options$skipEnrichWithData else FALSE
  
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
  
  #options
  #feature catalog related options
  fc <- action$getOption("fc")
  fc_exclude_attributes <- action$getOption("fc_exclude_attributes")
  fc_exclude_attributes_not_in_dictionary <- action$getOption("fc_exclude_attributes_not_in_dictionary")
  fc_exclude_values_for_attributes <- action$getOption("fc_exclude_values_for_attributes")
  fc_extra_attributes <- action$getOption("fc_extra_attributes")
  fc_default_min_occurs <- action$getOption("fc_default_min_occurs")
  fc_default_max_occurs <- action$getOption("fc_default_max_occurs")
  #file upload related options
  depositWithFiles <- action$getOption("depositWithFiles")
  depositDataPattern <- action$getOption("depositDataPattern")
  depositMetadataPattern <- action$getOption("depositMetadataPattern")
  
  
  #features if any
  build_catalog_from_features = TRUE
  if(fc){
    #manage multiple sources (supposes a common data structure to expose as FC)
    data_objects <- list()
    if(is.null(entity$data$dir)){
      data_objects <- list(entity$data)
    }else{
      data_objects <- entity$data$getData()
    }
    features = do.call("rbind", lapply(data_objects, function(data_object){data_object$features}))
    if(is.null(features)){
      if(!skipEnrichWithData){
        warnMsg <- sprintf("No data features associated to entity '%s' and global option 'skipEnrichWithData' is false. Skip feature catalogue creation", entity$identifiers[["id"]])
        config$logger$WARN(warnMsg)
        fc = FALSE
      }else{
        fto <- entity$data$featureTypeObj
        if(!is.null(fto)){
          infoMsg <- "Global option 'skipEnrichWithData' is true. Feature catalogue will be created based on the dictionary only"
          config$logger$INFO(infoMsg)
          build_catalog_from_features = FALSE
        }else{
          warnMsg <- "Global option 'skipEnrichWithData' is true, but no dictionary available. Skip feature catalogue creation"
          config$logger$WARN(warnMsg)
          fc = FALSE
        }
      }
    }
  }
  
  #basic function to map a geoflow_contact to a metadata editor contact
  produce_md_contact = function(x){
    
    md_contact = list()
    
    if(is.null(x$firstName)) x$firstName = NA
    if(is.null(x$lastName)) x$lastName = NA
    if(!is.na(x$firstName) && !is.na(x$lastName)) md_contact$individualName = paste(x$firstName, x$lastName)
    if(!is.null(x$organizationName)) if(!is.na(x$organizationName)) md_contact$organisationName = x$organizationName
    if(!is.null(x$positionName)) if(!is.na(x$positionName)) md_contact$positionName = x$positionName
    if(!is.null(x$role)) if(!is.na(x$role)) md_contact$role = x$role
    
    md_contact$contactInfo = list()
    md_contact$contactInfo$address = list()
    if(!is.null(x$email)) if(!is.na(x$email)) md_contact$contactInfo$address$electronicMailAddress = x$email
    if(!is.null(x$postalAddress)) if(!is.na(x$postalAddress)) md_contact$contactInfo$address$deliveryPoint = x$postalAddress
    if(!is.null(x$city)) if(!is.na(x$city)) md_contact$contactInfo$address$city = x$city
    if(!is.null(x$postalCode)) if(!is.na(x$postalCode)) md_contact$contactInfo$address$postalCode = x$postalCode
    if(!is.null(x$country)) if(!is.na(x$country)) md_contact$contactInfo$address$country = x$country
    md_contact$contactInfo$phone = list()
    if(!is.null(x$country)) if(!is.na(x$voice)) md_contact$contactInfo$phone$voice = x$voice
    if(!is.null(x$facsimile)) if(!is.na(x$facsimile)) md_contact$contactInfo$phone$facsimile = x$facsimile
    md_contact$contactInfo$onlineResource = list()
    if(!is.null(x$websiteUrl)) if(!is.na(x$websiteUrl)) md_contact$contactInfo$onlineResource$linkage = x$websiteUrl
    if(!is.null(x$websiteName)) if(!is.na(x$websiteName)) md_contact$contactInfo$onlineResource$name = x$websiteName
    
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
  edition = ""
  if(!is.null(entity$descriptions$edition)) if(!is.na(entity$descriptions$edition)){
    edition = entity$descriptions$edition
  }
  project$metadata_information = list(
    title = entity$titles[["title"]],
    producers = lapply(producers, function(x){ 
      contact = produce_md_contact(x)
      name = contact$organisationName
      if(!is.null(contact$individualName)) name = contact$individualName
      list(name = name) 
    }),
    production_date = production_date,
    version = edition
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
      edition = edition,
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
    resourceFormat = if(length(entity$formats)>0){
      lapply(entity$formats[sapply(entity$formats, function(x){x$key == "resource"})], function(resourceFormat){
        rf = list(name = resourceFormat$name)
        if(!is.null(resourceFormat$description)) rf$specification = resourceFormat$description
        return(rf)
      })
    }else{ list() },
    descriptiveKeywords = if(length(entity$subjects)>0){
      do.call(c, lapply(entity$subjects[sapply(entity$subjects, function(x){return(x$key != "topic")})], function(subject){
        lapply(subject$keywords, function(kwd){
          out_kwd = list(type = subject$key, keyword = kwd$name)
          if(!is.null(subject$name)) out_kwd$thesaurusName = subject$name
          return(out_kwd)
        })
      }))
    }else{list()},
    resourceConstraints = list(
      list(
        legalConstraints = list(
          useLimitation = if(length(entity$rights)>0){
            lapply(entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "uselimitation"})], function(cons){
              cons$values[[1]]
            })
          }else{list()},
          accessConstraints = if(length(entity$rights)>0){
            lapply(entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "accessconstraint"})], function(cons){
              cons$values[[1]]
            })
          }else{list()},
          useConstraints = if(length(entity$rights)>0){
            lapply(entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "useconstraint"})], function(cons){
              cons$values[[1]]
            })
          }else{list()}
        )
      )
    ),
    #resourceSpecificUsage
    #aggregationInfo
    extent = list(
      geographicElement = list(
        list(
          geographicBoundingBox = list(
            southBoundLatitude = entity$geo_bbox$ymin,
            westBoundLongitude = entity$geo_bbox$xmin,
            northBoundLatitude = entity$geo_bbox$ymax,
            eastBoundLongitude = entity$geo_bbox$xmax
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
    spatialRepresentationType = if(!is.null(entity$data$spatialRepresentationType)){
      entity$data$spatialRepresentationType
    }else{
      
    },
    language = list(entity$language),
    characterSet = list(
      list(codeListValue = "utf8")
    ),
    supplementalInformation = if(!is.null(entity$descriptions$info)) entity$descriptions$info else ""
  )
  
  #description/distributionInfo
  project$description$distributionInfo = list(
    distributionFormat = if(length(entity$formats)>0){
      lapply(entity$formats[sapply(entity$formats, function(x){x$key == "distribution"})], function(distFormat){
        df = list(name = distFormat$name)
        if(!is.null(distFormat$description)) df$specification = distFormat$description
        return(df)
      })
    }else{list()},
    distributor = lapply(distributors, produce_md_contact)
  )
  
  #description/dataQualityInfo
  project$description$dataQualityInfo = list()
  if(!is.null(entity$provenance)){
    project$description$dataQualityInfo = list(
      list(
        lineage = list(
          statement = if(!is.null(entity$provenance$statement)) entity$provenance$statement else "",
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
  if(fc){
    project$description$feature_catalogue = list()
    project$description$feature_catalogue$name = paste0(entity$titles[["title"]], " - Feature Catalogue")
    project$description$feature_catalogue$fieldOfApplication = list("FAIR") #to map
    versionDate <- as.POSIXct(Sys.time())
    project$description$feature_catalogue$versionNumber <- format(versionDate, "%Y%m%dT%H%M%S")
    project$description$feature_catalogue$versionDate = list(
      date = versionDate,
      type = "publication"
    )
    project$description$feature_catalogue$producer = produce_md_contact(producers[[1]])
    project$description$feature_catalogue$functionalLanguage = entity$language
    #featuretype
    ft = list(
      typeName = entity$identifiers$id,
      definition = entity$titles[["title"]],
      code = entity$identifiers$id,
      isAbstract = FALSE,
      carrierOfCharacteristics = list()
    )
    
    columns <- if(build_catalog_from_features){
      #from data features
      c(colnames(features), unlist(fc_extra_attributes))
    }else{
      #from dictionary
      fto <- entity$data$featureTypeObj
      sapply(fto$getMembers(), function(x){x$id})
    }
    for(featureAttrName in columns){
      
      if(featureAttrName %in% fc_exclude_attributes){
        config$logger$WARN(sprintf("Feature Attribute '%s' is listed in 'fc_exclude_attributes'. Discarding it...", featureAttrName)) 
        next
      }
      
      fat_attr_register <- NULL
      
      #create attribute
      fat <- list()
      #default name (from data)
      memberName <- featureAttrName
      
      fat_attr <- NULL
      fat_attr_desc <- NULL
      fto <- entity$data$featureTypeObj
      if(!is.null(fto)) fat_attr <- fto$getMemberById(featureAttrName)
      if(!is.null(fat_attr)){
        fat_attr_desc <- fat_attr$name
        registerId <- fat_attr$registerId
        if(!is.null(registerId)) if(!is.na(registerId)){
          registers <- config$registers
          if(length(registers)>0) registers <- registers[sapply(registers, function(x){x$id == registerId})]
          if(length(registers)==0){
            warnMsg <- sprintf("Unknown register '%s'. Ignored for creating feature catalogue", registerId)
            config$logger$WARN(warnMsg)
          }else{
            fat_attr_register <- registers[[1]]
          }
        }
        if(!is.null(fat_attr_desc)) memberName <- fat_attr_desc
      }else{
        if(fc_exclude_attributes_not_in_dictionary){
          config$logger$WARN(sprintf("Feature Attribute '%s' not referenced in dictionary and 'fc_exclude_attributes_not_in_dictionary' option is enabled. Discarding it...", featureAttrName)) 
          next
        }
      }
      fat$memberName = memberName
      fat$definition = fat_attr$def
      #cardinality
      minOccurs <- fc_default_min_occurs; if(!is.null(fat_attr)) minOccurs <- fat_attr$minOccurs
      maxOccurs <- fc_default_max_occurs; if(!is.null(fat_attr)) maxOccurs <- fat_attr$maxOccurs
      if(is.null(minOccurs)) minOccurs <- fc_default_min_occurs
      if(is.null(maxOccurs)) maxOccurs <- fc_default_max_occurs
      if(maxOccurs == "Inf") maxOccurs <- Inf
      fat$cardinality = list(lower = minOccurs, upper = maxOccurs)
      #code
      fat$code = featureAttrName
      #uom
      fat$valueMeasurementUnit = fat_attr$uom
      #add listed values
      featureAttrValues <- fat_attr_register$data$code
      if(build_catalog_from_features) if(featureAttrName %in% colnames(features)){
        featureAttrValues <- switch(class(features)[1],
                                    "sf" = features[,featureAttrName][[1]],
                                    "data.frame" = features[,featureAttrName]
        )
      }
      addValues <- TRUE
      if(is(featureAttrValues, "sfc")){
        addValues <- FALSE
      }else if(featureAttrName %in% fc_exclude_values_for_attributes){
        addValues <- FALSE
      }else{
        if(is.null(fat_attr)){
          addValues <- FALSE
        }else{
          if(fat_attr$type == "variable") addValues <- FALSE
        }
      }
      fat$listedValue = list()
      if(!is.null(featureAttrValues) & addValues){
        config$logger$INFO("Listing values for feature Attribute '%s'...", featureAttrName)
        featureAttrValues <- unique(featureAttrValues)
        featureAttrValues <- featureAttrValues[order(featureAttrValues)]
        for(featureAttrValue in featureAttrValues){
          if(!is.na(featureAttrValue)){
            val <- list(label = "", code = "", definition = "")
            if(!is(featureAttrValue, "character")) featureAttrValue <- as(featureAttrValue, "character")
            val$code = featureAttrValue
            if(!is.null(fat_attr_register)){
              reg_item <- fat_attr_register$data[fat_attr_register$data$code == featureAttrValue,]
              if(nrow(reg_item)>0){
                val$code = featureAttrValue
                val$label = if(!is.na(reg_item[1L,"label"])) reg_item[1L,"label"] else ""
                val$definition = if(!is.na(reg_item[1L, "definition"])) reg_item[1L, "definition"] else ""
              }
            }
            fat$listedValue[[length(fat$listedValue)+1]] = val
          }
        }
      }else{
        config$logger$WARN(sprintf("Skip listing values for feature Attribute '%s'...", featureAttrName))
      }
      
      #add primitive type + data type (attribute or variable) as valueType
      fat_type <- if(build_catalog_from_features & !is.null(featureAttrValues[1])){
        switch(class(featureAttrValues[1])[1],
               "integer" = "xsd:int",
               "numeric" = "xsd:decimal",
               "character" = "xsd:string",
               "logical" = "xsd:boolean",
               "Date" = "xsd:date",
               "POSIXct" = "xsd:datetime",
               "sfc_POINT" = "gml:PointPropertyType",
               "sfc_MULTIPOINT" = "gml:MultiPointPropertyType",
               "sfc_LINESTRING" = "gml:LineStringPropertyType",
               "sfc_MULTILINESTRING" = "gml:MultiLineStringPropertyType",
               "sfc_POLYGON" = "gml:PolygonPropertyType",
               "sfc_MULTIPOLYGON" = "gml:MultiPolygonPropertyType"
        )
      }else{
        type = if(!is.null(fto)) fto$getMemberById(featureAttrName)$type else "attribute"
        switch(type,
               "attribute" = "xsd:string",
               "variable" = "xsd:decimal",
               type
        )
      }
      config$logger$INFO("Set primitive type '%s' for feature Attribute '%s'...", fat_type, featureAttrName)
      fat_generic_type <- if(build_catalog_from_features){
        switch(class(featureAttrValues[1])[1],
               "integer" = "variable",
               "numeric" = "variable",
               "attribute"
        )
      }else{
        if(!is.null(fto)) fto$getMemberById(featureAttrName)$type else "attribute"
      }
      config$logger$INFO("Feature member generic type for '%s': %s", featureAttrName, fat_generic_type)
      if(!is.null(fat_attr)) fat_generic_type <- fat_attr$type
      fat_type_anchor <- fat_type #ISOAnchor$new(name = fat_type, href = fat_generic_type)
      fat$valueType = fat_type_anchor
      
      #add feature attribute as carrierOfCharacteristic
      config$logger$INFO("Add carrier of characteristics for feature Attribute '%s'...", featureAttrName)
      ft$carrierOfCharacteristics[[length(ft$carrierOfCharacteristics)+1]] = fat
    }
    
    project$description$feature_catalogue$featureType = list(ft)
  }
  
  #creation
  #-----------------------------------------------------------------------------
  output = metadataeditr::create_project(
    type = "geospatial",
    idno = entity$identifiers[["id"]],
    metadata = project,
    collection_names = collection_names,
    thumbnail = if(length(thumbnails)>0) file.path(getwd(), "thumbnails", thumbnails[[1]]$link) else NULL, #TODO
    overwrite = TRUE
  )
  
  if(output$response$status == "success"){
    config$logger$INFO("Project '%s' successfully submitted to metadata editor", entity$identifiers$id)
  }
  
  #add resources
  #-----------------------------------------------------------------------------
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
  
  #DOI resource
  if(!is.null(entity$identifiers[["doi"]])){
    metadataeditr::resources_add(
      idno = entity$identifiers[["id"]],
      dctype = "web",
      title = "DOI",
      description = "Digital Object Identifier",
      file_path = paste0("http://dx.doi.org/", entity$identifiers[["doi"]])
    )
  }
  
  #entity HTTP(S) resources
  if(length(entity$relations)>0){
    http_relations <- entity$relations[sapply(entity$relations, function(x){
      x$key %in% c("ftp","http", "download") | any(startsWith(x$key, c("wfs", "wms", "wcs", "csw")))
    })]
    for(http_relation in http_relations){
      metadataeditr::resources_add(
        idno = entity$identifiers[["id"]],
        dctype = "web",
        title = http_relation$name,
        description = http_relation$description,
        file_path = http_relation$link
      )
    }
  }
  
  #files
  if(depositWithFiles){
    #data
    data_files <- list.files(file.path(getwd(),"data"), pattern = depositDataPattern)
    for(data_file in data_files){
      config$logger$INFO("Upload data file '%s'", data_file)
      metadataeditr::resources_add(
        idno = entity$identifiers[["id"]],
        dctype = "dat",
        title = data_file,
        file_path = file.path(getwd(), "data", data_file)
      )
    }
    #metadata
    # metadata_files <- list.files(file.path(getwd(),"metadata"), pattern = depositMetadataPattern)
    # for(metadata_file in metadata_files){
    #   config$logger$INFO("Upload metadata file '%s'", metadata_file)
    #   metadataeditr::resources_add(
    #     idno = entity$identifiers[["id"]],
    #     dctype = "dat",
    #     title = metadata_file,
    #     file_path = file.path(getwd(), "metadata", metadata_file)
    #   )
    # }
  }
}
