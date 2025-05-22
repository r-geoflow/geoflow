function(action, entity, config){
  
  if(!requireNamespace("geometa", quietly = TRUE)){
    stop("The 'geometa-create-iso-19115' action requires the 'geometa' package")
  }
  
  ISOMetadataNamespace$GML$uri <- "http://www.opengis.net/gml/3.2"
  
  #features if any
  features <- entity$data$features
  
  #options
  use_uuid <- action$getOption("use_uuid")
  inspire <- action$getOption("inspire")
  logo <- action$getOption("logo")
  doi <- action$getOption("doi")
  doi_thumbnail <- action$getOption("doi_thumbnail")
  addfeatures <- action$getOption("addfeatures")
  featureid <- action$getOption("featureid")
  if(is.na(featureid)) if(!is.null(features)) featureid = colnames(features)[1]
  geographySubject <- action$getOption("subject_geography")
  include_service_identification <- action$getOption("include_service_identification")
  include_coverage_data_dimension_values <- action$getOption("include_coverage_data_dimension_values")
  include_coverage_service_dimension_values <- action$getOption("include_coverage_service_dimension_values")
  include_object_identification_ids = action$getOption("include_object_identification_ids")
  
  #check inspire metadata validator configuration
  INSPIRE_VALIDATOR <- NULL
  if(inspire){
    INSPIRE_VALIDATOR <- config$software$output$inspire
    if(is.null(INSPIRE_VALIDATOR)){
      errMsg <- "This action requires a INSPIRE metadata validator software to be declared in the configuration"
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }
  
  createResponsibleParty = function(x, role = NULL, roleId = NULL){
    if(is.null(role)) role <- x$role 
    if(is.null(roleId)) roleId = role
    rp <- ISOResponsibleParty$new()
    if(is.null(x$firstName)) x$firstName = NA
    if(is.null(x$lastName)) x$lastName = NA
    indName = ""
    if(!is.na(x$firstName)) indName = x$firstName
    if(!is.na(x$lastName)){
      if(!nzchar(indName)){
        indName = x$lastName
      }else{
        indName = paste(indName, x$lastName)
      }
    }
    if(nzchar(indName)) rp$setIndividualName(indName)
    rp$setOrganisationName(x$organizationName)
    rp$setPositionName(x$positionName)
    rp$setRole(role)
    contact <- ISOContact$new()
    phone <- ISOTelephone$new()
    phone$setVoice(x$voice)
    phone$setFacsimile(x$facsimile)
    contact$setPhone(phone)
    address <- ISOAddress$new()
    address$setDeliveryPoint(x$postalAddress)
    address$setCity(x$city)
    address$setPostalCode(x$postalCode)
    address$setCountry(x$country)
    address$setEmail(x$email)
    contact$setAddress(address)
    if(!is.null(x$websiteUrl)){
      res <- ISOOnlineResource$new()
      res$setLinkage(x$websiteUrl)
      res$setName(x$websiteName)
      contact$setOnlineResource(res)
    }
    rp$setContactInfo(contact)
    
    #check existence of ORCID
    orcid = x$identifiers[["orcid"]]
    if(!is.null(orcid)){
      rp$parentAttrs[["xlink:href"]] <- paste0("https://orcid.org/", orcid)
    }
    
    if(include_object_identification_ids){
      rp_id = paste(roleId, tolower(x$email), sep = "_")
      rp$setAttr("id", create_object_identification_id("party", rp_id))
    }
    return(rp)
  }
  
  #metadata creation
  #-----------------------------------------------------------------------------------------------------
  #create geometa object
  # if(!is.null(entity$data)) {
  #   md<-switch(entity$data$spatialRepresentationType,
  #                                     "vector" = ISOMetadata$new(),
  #                                     "grid" = ISOImageryMetadata$new()
  #   )
  # }else{
  #   md <- ISOMetadata$new()
  # }
  md <- ISOMetadata$new()
  mdId <- entity$identifiers[["id"]]
  if(use_uuid) mdId <- entity$identifiers[["uuid"]]
  md$setFileIdentifier(mdId)
  
  the_doi <- entity$identifiers[["doi"]]
  if(is.null(the_doi)) {
    #no DOI set in initial entity, let's look for a reserved DOI
    if(any(regexpr("zenodo", entity$identifiers)>0)) {
      the_doi <- entity$identifiers[["zenodo_conceptdoi_to_save"]]
      if(is.null(the_doi)) the_doi <- entity$identifiers[["zenodo_doi_to_save"]]
    } else if(any(regexpr("dataverse", entity$identifiers)>0)){
      the_doi <- entity$identifiers[["dataverse_conceptdoi_to_save"]]
      if(is.null(the_doi)) the_doi <- entity$identifiers[["dataverse_doi_to_save"]]
    }
  }
  
  if(length(entity$relations)>0){
    parent_rels <- entity$relations[sapply(entity$relations, function(x){x$key == "parent"})]
    if(length(parent_rels)>0){
      parent <- parent_rels[[1]]
      parentId <- parent$name
      if(!is.null(parent$link)) parentId <- ISOAnchor$new(name = parent$name, href = parent$link)
      md$setParentIdentifier(parentId)
    }
  }
  md$setCharacterSet("utf8")
  md$setLanguage(entity$language)
  
  md_date = Sys.time()
  if(length(entity$dates)>0){
    md_dates = entity$dates[sapply(entity$dates, function(x){x$key == "metadata"})]
    if(length(md_dates)>0){
      md_date = md_dates[[1]]$value
    }
  }
  md$setDateStamp(md_date)
  
  #locales (i18n/i10n support)
  if(length(entity$locales)>0){
    ref_locales = utils::read.csv(system.file("extdata/codelists", "ISO-639-2_utf-8.txt", package = "geometa"),sep="|", stringsAsFactors = FALSE)
    for(locale in entity$locales){
      a_locale <- ISOLocale$new()
      a_locale$setId(locale)
      language = ref_locales[ref_locales$alpha2 == tolower(locale),]$alpha3[1]
      a_locale$setLanguage(language)
      a_locale$setCharacterSet("utf8")
      md$addLocale(a_locale)
    }
  }
  
  if(!is.null(entity$data)) {
    md$setMetadataStandardName(switch(entity$data$spatialRepresentationType,
                                      "vector" = "ISO 19115:2003 Geographic information - Metadata",
                                      "grid" = "ISO 19115-2 Geographic Information - Metadata Part 2 Extensions for imagery and gridded data"
    ))
    md$setMetadataStandardVersion(switch(entity$data$spatialRepresentationType,
                                         "vector" = "ISO 19115:2003",
                                         "grid" = "ISO 19115-2:2009"
    ))
  }else{
    md$setMetadataStandardName("ISO 19115:2003 Geographic information - Metadata")
    md$setMetadataStandardVersion("ISO 19115:2003")
  }
  
  md$setDataSetURI(md$fileIdentifier)
  
  dctype <- entity$types[["generic"]]
  dctype_idx = which(tolower(ISOScopeCode$values()) == tolower(dctype))
  dctype_iso = ISOScopeCode$values()[dctype_idx]
  if(length(dctype_iso)==0) dctype_iso = "dataset"
  md$addHierarchyLevel(dctype_iso)
  
  #add contacts
  # if(length(entity$contacts)>0){
  #   metadata_contacts <- entity$contacts[sapply(entity$contacts, function(x){tolower(x$role) == "metadata"})]
  #   if(is.null(metadata_contacts)) metadata_contacts<-entity$contacts[sapply(entity$contacts, function(x){tolower(x$role) == "owner"})]
  #     for(metadata_contact in metadata$contacts){
  #       metadata_contact$setRole("metadata")
  #       rp<-createResponsibleParty(metadata_contact,"pointOfContact") 
  #       md$addContact(rp)
  #     }
  # }
  
  if(length(entity$contacts)>0)for(entity_contact in entity$contacts){
    if(tolower(entity_contact$role) == "metadata"){
      rp<-createResponsibleParty(entity_contact,role = "pointOfContact", roleId = "metadata") 
      md$addContact(rp)
    } 
  }
  
  if(length(md$contact)==0) md$contact <- ISOAttributes$new("gco:nilReason" = "missing")   
  
  
  #spatial representation
  if(!is.null(entity$data)) {
    spatialRepresentationType <- entity$data$spatialRepresentationType
    if(!is.null(spatialRepresentationType)){
      if(spatialRepresentationType=="vector"){
        if(!is.null(features)){
          #support vector spatial representation
          if(is(features, "sf")){
            geomtypes <- as.list(table(sf::st_geometry_type(features)))
            geomtypes <- geomtypes[geomtypes > 0]
            if(length(geomtypes)>0){
              #spatialRepresentationType <- "vector"
              for(geomtype in names(geomtypes)){
                vsr <- ISOVectorSpatialRepresentation$new()
                geomLevel <- "geometryOnly"
                if(geomtype == "TIN") geomLevel = "planarGraph"
                vsr$setTopologyLevel(geomLevel)
                if(geomLevel == "geometryOnly"){
                  geomObject <- ISOGeometricObjects$new()
                  isoGeomType <- switch(geomtype,
                                        "GEOMETRY" = "composite", "GEOMETRYCOLLECTION" = "composite",
                                        "POINT" = "point", "MULTIPOINT" = "point", 
                                        "LINESTRING" = "curve", "CIRCULARSTRING" = "curve", "MULTILINESTRING" = "curve", "CURVE" = "curve", "COMPOUNDCURVE" = "curve",
                                        "POLYGON" = "surface", "MULTIPOLYGON" = "surface", "TRIANGLE" = "surface",
                                        "CURVEPOLYGON" = "surface", "SURFACE" = "surface", "MULTISURFACE" = "surface",
                                        "POLYHEDRALSURFACE" = "solid"
                  )
                  geomObject$setGeometricObjectType(isoGeomType)
                  geomObject$setGeometricObjectCount(nrow(features[sf::st_geometry_type(features)==geomtype,]))
                  vsr$addGeometricObjects(geomObject)
                }
                md$addSpatialRepresentationInfo(vsr)
              }
            }else{
              spatialRepresentationType <- "textTable"
            }
          }
        }
      }
      
      if(spatialRepresentationType=="grid"){
        gsr <- ISOGridSpatialRepresentation$new()
        gsr$setNumberOfDimensions(length(entity$data$dimensions))
        for(dimension in names(entity$data$dimensions)){
          dimObject <- ISODimension$new()
          dimObject$setName(dimension)
          dimObject$setSize(entity$data$dimensions[[dimension]]$size)
          resolution<-entity$data$dimensions[[dimension]]$resolution
          if(is.null(resolution$value)){
            dimObject$resolution <- ISOAttributes$new("gco:nilReason" = "missing")  
          }else{
            dimObject$setResolution(ISOMeasure$new(value=resolution$value,uom=resolution$uom))
          }
          gsr$addDimension(dimObject)
        }
        gsr$setCellGeometry("area")
        md$addSpatialRepresentationInfo(gsr)
      }
    }
  }
  
  #spatial reference system
  if(!is.null(entity$srid)){
    rs <- ISOReferenceSystem$new()
    rsId <- ISOReferenceIdentifier$new(code = as.character(entity$srid), codeSpace = "EPSG")
    rs$setReferenceSystemIdentifier(rsId)
    md$addReferenceSystemInfo(rs)
  }
  
  #Data identification
  ident <- ISODataIdentification$new()
  ident$setAbstract(entity$descriptions[["abstract"]], locales = geoflow::get_locales_from(entity$descriptions[["abstract"]]))
  ident$setPurpose(entity$descriptions[["purpose"]], locales = geoflow::get_locales_from(entity$descriptions[["purpose"]]))
  ident$addCredit(entity$descriptions[["credit"]], locales = geoflow::get_locales_from(entity$descriptions[["credit"]]))
  ident$addStatus(entity$descriptions[["status"]])
  ident$addLanguage(entity$language)
  ident$addCharacterSet("utf8")
  #topic categories
  topics <- list()
  if(length(entity$subjects)>0) topics <- entity$subjects[sapply(entity$subjects, function(x){return(tolower(x$key) == "topic")})]
  if(length(topics)>0){
    for(topic in topics){
      for(topicCategory in topic$keywords) ident$addTopicCategory(topicCategory$name)
    }
  }
  
  #adding contacts
  if(length(entity$contacts)>0)for(entity_contact in entity$contacts){
    if(tolower(entity_contact$role) != "metadata" && !startsWith(entity_contact$role, "processor")){
      rp<-createResponsibleParty(entity_contact, roleId = entity_contact$role) 
      ident$addPointOfContact(rp)
    }
  }
  
  #citation
  now <- Sys.time()
  ct <- ISOCitation$new()
  ct$setTitle(entity$titles[["title"]], locales = geoflow::get_locales_from(entity$titles[["title"]]))
  if("alternative" %in% names(entity$titles)){
    ct$addAlternateTitle(entity$titles[["alternative"]])
  }
  for(date in entity$dates){
    if(date$key != "edition"){
      d <- ISODate$new()
      d$setDate(date$value)
      d$setDateType(date$key)
      ct$addDate(d)
    }
  }
  #edition date
  editionDates = list()
  if(length(entity$dates)>0) entity$dates[sapply(entity$dates, function(x){x$key == "edition"})]
  if(length(editionDates)>0){
    editionDate = editionDates[[1]]$value
    ct$setEditionDate(editionDate)
  }
  
  #edition
  if(!is.null(entity$descriptions[["edition"]])){
    edition = entity$descriptions[["edition"]]
    ct$setEdition(edition, locales = geoflow::get_locales_from(edition))
  }
  
  #set metadata identifier
  ct$addIdentifier(ISOMetaIdentifier$new(code = mdId))
  if(doi){
    #methodology to set DOI inspired by NOAA wiki
    #https://geo-ide.noaa.gov/wiki/index.php?title=DOI_Minting_Procedure#Third.2C_Include_the_DOI_and_citation_text_in_the_ISO_Metadata_Record
    if(!is.null(the_doi)){
      mdIdentifier <- ISOAnchor$new(
        name = paste0("doi:", the_doi), 
        href = paste0("http://dx.doi.org/", the_doi)
      )
      mdIdentifier$setAttr("xlink:title", "DOI")
      mdIdentifier$setAttr("xlink:actuate", "onRequest")
      ct$addIdentifier(ISOMetaIdentifier$new(code = mdIdentifier))
    }
  }
  
  ct$addPresentationForm("mapDigital") #TODO to map with gsheet
  
  #adding responsible party (search for owner, otherwise take first contact)
  if(length(entity$contacts)>0){
    owners <- entity$contacts[sapply(entity$contacts, function(x){x$role == "owner"})]
    if(length(owners)==0){
      owner = entity$contacts[[1]]$clone(deep = T)
      owner$setRole("owner")
      owners <- list(owner)
    }
    for(owner_entity in owners){
      rp<-createResponsibleParty(owner_entity, roleId = "responsible_party") 
      ct$citedResponsibleParty <- c(ct$citedResponsibleParty, rp)
    }
  }
  ident$setCitation(ct)
  
  #graphic overviews
  if(length(entity$relations)>0){
    thumbnails <- entity$relations[sapply(entity$relations, function(x){x$key == "thumbnail"})]
    for(thumbnail in thumbnails){
      go <- ISOBrowseGraphic$new(
        fileName = thumbnail$link,
        fileDescription = thumbnail$description
      )
      thumbnail_id = paste(tolower(entity$identifiers[["id"]]), "thumbnail", tolower(thumbnail$link),sep="_")
      if(include_object_identification_ids) go$setAttr("id", create_object_identification_id("browsegraphic", thumbnail_id))
      ident$addGraphicOverview(go)
    }
  }
  #resource formats
  if(length(entity$formats)>0){
    resourceFormats = entity$formats[sapply(entity$formats, function(x){x$key == "resource"})]
    if(length(resourceFormats)>0) for(resourceFormat in resourceFormats){
      format = ISOFormat$new()
      format_name = resourceFormat$name
      if(!is.null(resourceFormat$uri)){
        format_name <- ISOAnchor$new(name = resourceFormat$name, href = resourceFormat$uri)
      }
      format$setName(format_name)
      if(!is.null(ISOFormat$buildFrom)) format = ISOFormat$buildFrom(resourceFormat$name)
      format$setVersion(NA)
      format$setSpecification(resourceFormat$description)
      ident$addFormat(format)
    }
  }
  #option to add doi thumbnail
  if(doi && doi_thumbnail) if(!is.null(the_doi)) {
    doiThumbnail <- ISOBrowseGraphic$new(
      fileName = sprintf("https://img.shields.io/badge/DOI-%s-informational.svg",the_doi),
      fileDescription =  ISOAnchor$new(
        name = the_doi, 
        href = paste0("http://dx.doi.org/", the_doi)
      )
    )
    ident$addGraphicOverview(doiThumbnail)
  }
  #option to add logo as thumbnail
  if(logo && !is.null(config$profile$logos)){
    for(logo in config$profile$logos){
      logoThumbnail <- ISOBrowseGraphic$new(fileName = logo, fileDescription = "Logo")
      ident$addGraphicOverview(logoThumbnail)
    }
  }
  
  #maintenance information
  default_maintenance = "asNeeded"
  maint <- ISOMaintenanceInformation$new()
  maint$setMaintenanceFrequency(if(!is.null(entity$descriptions[["maintenance"]])) entity$descriptions[["maintenance"]] else default_maintenance)
  ident$addResourceMaintenance(maint)
  
  #legal constraints
  if(length(entity$rights)>0){
    legal_constraints <- ISOLegalConstraints$new()
    #license
    licenses <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "license"})]
    if(length(licenses)>0){
      legal_constraints$addUseConstraint("license")
      for(license in licenses){
        for(value in license$values){
          license_info = zen4R::ZenodoManager$new()$getLicenseById(value)
          if(!is.null(license_info)){
            value = ISOAnchor$new(name = license_info$title[[1]], href = license_info$props$url)
            legal_constraints$useLimitation = c(legal_constraints$useLimitation, value)
          }else{
            legal_constraints$addUseLimitation(value, locales = geoflow::get_locales_from(value))
          }
        }
      }
    }
    #use limitation
    uses <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) %in% c("use","uselimitation","termsofuse", "disclaimer", "citation")})]
    if(length(uses)>0){
      for(use in uses){
        for(value in use$values){
          legal_constraints$addUseLimitation(value, locales = geoflow::get_locales_from(value))
        }
      }
    }
    #use constraints
    useConstraints <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "useconstraint"})]
    if(length(useConstraints)>0){
      for(useConstraint in useConstraints){
        for(value in useConstraint$values){
          legal_constraints$addUseConstraint(value) 
        }
      }
    }
    #access constraints
    accessConstraints <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "accessconstraint"})]
    if(length(accessConstraints)>0){
      for(accessConstraint in accessConstraints){
        for(value in accessConstraint$values){
          legal_constraints$addAccessConstraint(value)
        }
      }
    }
    #other constraints
    otherConstraints <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "otherconstraint"})]
    if(length(otherConstraints)>0){
      for(otherConstraint in otherConstraints){
        for(value in otherConstraint$values){
          legal_constraints$addOtherConstraint(value, locales = geoflow::get_locales_from(value))
        }
      }
    }
    ident$addResourceConstraints(legal_constraints)
  }
  
  #extents
  extent <- ISOExtent$new()
  #geographic extent
  if(!is.null(entity$geo_bbox)){
    sf_bbox <- entity$geo_bbox
    bbox <- ISOGeographicBoundingBox$new(minx = sf_bbox$xmin, miny = sf_bbox$ymin, maxx = sf_bbox$xmax, maxy = sf_bbox$ymax)
    extent$addGeographicElement(bbox)
  }
  #bounding polygons from spatial coverage
  #(applies to spatial coverage set-up from wkt)
  if(is(entity$spatial_extent, "sfc")){
    bbox_sfc <- sf::st_as_sfc(sf::st_bbox(entity$spatial_extent))
    #if bbox (as geometry) is different from the spatial extent
    #then we have more complex geometries
    if(bbox_sfc != entity$spatial_extent){
      sbp <- ISOBoundingPolygon$new()
      geom <- GMLAbstractGeometry$fromSimpleFeatureGeometry(entity$spatial_extent[[1]])
      sbp$addPolygon(geom)
      extent$addGeographicElement(sbp)
    }
  }
  #bounding polygons from data (if any features & 'addfeatures' option is enabled)
  if(!is.null(features) && addfeatures){
    bp <- ISOBoundingPolygon$new()
    geom_field = colnames(features)[sapply(colnames(features), function(x){is(features[[x]],"sfc")})][1]
    for(i in 1:nrow(features)){
      geom <- GMLAbstractGeometry$fromSimpleFeatureGeometry(features[i,][geom_field][[1]])
      geom$attrs["gml:id"] <- paste0("fid.",as.character(features[i,][featureid])[1])
      bp$polygon <- c(bp$polygon, geom)
    }
    extent$addGeographicElement(bp)
  }
  #geographic identifiers
  geothesauri <- list()
  if(length(entity$subjects)>0) geothesauri <- entity$subjects[sapply(entity$subjects, function(x){return(tolower(x$key) == geographySubject)})]
  if(length(geothesauri)>0){
    for(geothesaurus in geothesauri){
      for(geokwd in geothesaurus$keywords){
        iso_kwd <- geokwd$name
        if(!is.null(geokwd$uri)){
          iso_kwd <- ISOAnchor$new(name = geokwd$name, href = geokwd$uri)
        }
        geodesc <- ISOGeographicDescription$new()
        geodesc$setGeographicIdentifier(ISOMetaIdentifier$new(code = iso_kwd))
        extent$addGeographicElement(geodesc)
      }
    }
  }
  
  
  #temporal extent
  if(!is.null(entity$temporal_extent)){
    time <- ISOTemporalExtent$new()
    if(!is.null(entity$temporal_extent$instant)){
      gmltimeinstant <- GMLTimeInstant$new()
      instant = entity$temporal_extent$instant
      gmltimeinstant$setTimePosition(timePosition = if(is.na(instant)) NULL else instant,
                                    frame = attr(instant, "frame"),
                                    calendarEraName = attr(instant, "calendarEraName"),
                                    indeterminatePosition = attr(instant, "indeterminatePosition"))
      time$setTimeInstant(gmltimeinstant)
    }
    if(!is.null(entity$temporal_extent$start) & !is.null(entity$temporal_extent$end)){
      gmltimeperiod <- GMLTimePeriod$new()
      start = entity$temporal_extent$start
      gmltimeperiod$setBeginPosition(beginPosition = if(is.na(start)) NULL else start,
                                     frame = attr(start, "frame"),
                                     calendarEraName = attr(start, "calendarEraName"),
                                     indeterminatePosition = attr(start, "indeterminatePosition"))
      end = entity$temporal_extent$end
      gmltimeperiod$setEndPosition(endPosition = if(is.na(end)) NULL else end,
                                   frame = attr(start, "frame"),
                                   calendarEraName = attr(end, "calendarEraName"),
                                   indeterminatePosition = attr(end, "indeterminatePosition"))
      time$setTimePeriod(gmltimeperiod)
    }
    extent$addTemporalElement(time)
  }
  ident$addExtent(extent)
  
  #thesaurus/keywords
  subjects <- entity$subjects
  if(length(subjects)>0) subjects <- subjects[sapply(subjects, function(x){return(x$key != "topic")})]
  if(length(subjects)>0) for(subject in subjects){
    #add keywords
    kwds <- ISOKeywords$new()
    for(kwd in subject$keywords){
      iso_kwd <- kwd$name
      iso_kwd_locales <- geoflow::get_locales_from(kwd$name)
      iso_kwd_locales_codes = names(iso_kwd_locales)
      if(!is.null(kwd$uri)){
        iso_kwd <- ISOAnchor$new(name = kwd$name, href = kwd$uri)
        iso_kwd_locales_uris <- geoflow::get_locales_from(kwd$uri)
        if(length(iso_kwd_locales_uris)>0){
          iso_kwd_locales <- lapply(iso_kwd_locales_codes, function(locale){
            iso_kwd_locale <- iso_kwd_locales[[locale]]
            attr(iso_kwd_locale, "uri") <- iso_kwd_locales_uris[[locale]]
            return(iso_kwd_locale)
          })
          names(iso_kwd_locales) <- iso_kwd_locales_codes
        }
      }
      kwds$addKeyword(iso_kwd, locales = iso_kwd_locales)
    }
    kwds$setKeywordType(subject$key)
    #theausurus
    if(!is.null(subject$name)){
      th <- ISOCitation$new()
      title <- subject$name
      title_locales <- geoflow::get_locales_from(subject$name)
      title_locales_codes <- names(title_locales)
      
      if(!is.null(subject$uri)){
        title <- ISOAnchor$new(name = subject$name, href = subject$uri)
        title_locales_uris <- geoflow::get_locales_from(subject$uri)
        if(length(title_locales_uris)>0){
          title_locales <- lapply(title_locales_codes, function(locale){
            title_locale <- title_locales[[locale]]
            attr(title_locale, "uri") <- title_locales_uris[[locale]]
            return(title_locale)
          })
          names(title_locales) <- title_locales_codes
        }
      }
      th$setTitle(title, locales = title_locales)
      
      if(length(subject$dates)>0){
        for(subj_datetype in names(subject$dates)){
          subj_date <- ISODate$new()
          subj_date$setDate(subject$dates[[subj_datetype]])
          subj_date$setDateType(subj_datetype)
          th$addDate(subj_date) 
        }
      }else{
        #TODO thesaurus date (likely to be different that current date). Required for ISO validity
        #this is a limitation of tabular approach to fill metadata
        d <- ISODate$new()
        d$setDate(Sys.Date())
        d$setDateType("lastRevision")
        th$addDate(d) 
      }
      kwds$setThesaurusName(th)
    }
    ident$addKeywords(kwds)
  }
  
  ident$setSupplementalInformation(entity$descriptions[["info"]], locales = geoflow::get_locales_from(entity$descriptions[["info"]]))
  if(!is.null(entity$data)) ident$addSpatialRepresentationType(entity$data$spatialRepresentationType)
  md$identificationInfo = c(md$identificationInfo,ident)
  
  #service information
  if(length(entity$relations)>0){
    #WMS
    wms<-entity$relations[sapply(entity$relations, function(x){startsWith(x$key,"wms")})]
    if(include_service_identification) if(length(wms)>0){
      wms <- wms[[1]]
      wms_link <- gsub("service=WMS","",wms$link)
      wms_version <- switch(wms$key,
                            "wms" = "1.1.0",
                            "wms110" = "1.1.0",
                            "wms111" = "1.1.1",
                            "wms130" = "1.3.0")
      config$logger.info(sprintf("Configuring WMS client on '%s' (version = '%s')", wms_link, wms_version))
      
      if(!requireNamespace("ows4R", quietly = TRUE)){
        stop("The 'geometa-create-iso-19115' action requires the 'ows4R' package")
      }
      
      WMS<-ows4R::WMSClient$new(url=wms_link,serviceVersion=wms_version,logger="DEBUG")
      if(!is.null(wms)){
        #SRVServiceIdentification
        si <- ISOSRVServiceIdentification$new()
        si$setAttr("id","OGC-WMS")
        #citation
        si$citation <- ISOAttributes$new("gco:nilReason" = "missing")
        #abstract
        si$setAbstract(WMS$getCapabilities()$getServiceIdentification()$getAbstract())
        #extent
        si$addExtent(extent)
        #descriptiveKeywords
        si$descriptiveKeywords <- ISOAttributes$new("gco:nilReason" = "missing")
        #resourceConstraints
        si$resourceConstraints <- ISOAttributes$new("gco:nilReason" = "missing")
        #aggregationInfo
        si$aggregationInfo <- ISOAttributes$new("gco:nilReason" = "missing")
        #servicetype
        si$setServiceType("OGC:WMS")
        #Fees
        orderProcess <- ISOStandardOrderProcess$new()
        orderProcess$setFees(WMS$getCapabilities()$getServiceIdentification()$getFees())
        si$setAccessProperties(orderProcess)
        #coupling type
        
        if(!is.null(entity$data)) {
          switch(entity$data$spatialRepresentationType,
                 "vector" = si$setCouplingType("mixed"),
                 "grid" = si$setCouplingType("tight")
          )
        }
        
        for(request in WMS$getCapabilities()$getRequestNames()){
          #add operation metadata 
          wmsOp <- ISOOperationMetadata$new()
          wmsOp$addDCP("WebServices")
          wmsOp$setOperationName(request)
          wmsOp$setOperationDescription(request)
          wmsOp$setInvocationName(request)
          
          if(request=="GetCapabilities"){
            or1 <- ISOOnlineResource$new()
            or1$setLinkage(paste0(wms$link,"&version=",switch(wms$key,
                                                              "wms" = "1.1.0",
                                                              "wms110" = "1.1.0",
                                                              "wms111" = "1.1.1",
                                                              "wms130" = "1.3.0"),"&request=GetCapabilities"))
            or1$setName("OGC:WMS")
            or1$setDescription("Open Geospatial Consortium Web Map Service (WMS)")
            or1$setProtocol("OGC:WMS")
            wmsOp$addConnectPoint(or1)
          }
          
          if(request=="GetMap"){
            #GetMap
            if(length(entity$data$ogc_dimensions)>0) for(ogc_dimension in names(entity$data$ogc_dimensions)){
              param <- ISOParameter$new()
              param$setName(toupper(ogc_dimension), "xs:string")
              param$setDirection("in")
              param$setOptionality(FALSE)
              param$setRepeatability(FALSE)
              param$setValueType("xs:string")
              wmsOp$parameters=c(wmsOp$parameters,param)  
            }
            if(!is.null(entity$data)) if(entity$data$uploadType == "dbquery" & length(entity$data$parameters)>0){
              param <- ISOParameter$new()
              param$setName("VIEWPARAMS", "xs:string")
              param$setDirection("in")
              param$setOptionality(FALSE)
              param$setRepeatability(FALSE)
              param$setValueType("xs:string")
              wmsOp$parameters=c(wmsOp$parameters,param)    
            }
          }
          
          if(length(wmsOp$parameters)==0) wmsOp$parameters <- ISOAttributes$new("gco:nilReason" = "missing")
          if(length(wmsOp$connectPoint)==0) wmsOp$connectPoint <- ISOAttributes$new("gco:nilReason" = "missing")
          
          si$containsOperations = c(si$containsOperations, wmsOp)
        }
      }
      md$identificationInfo = c(md$identificationInfo,si)
    }
  }
  
  #contentInfo
  #coverage description
  if(!is.null(entity$data)) if(entity$data$spatialRepresentationType=="grid"){
    if(!is.null(entity$data$variables)){
      #create coverage description
      cov <- ISOImageryCoverageDescription$new()
      cov$setAttributeDescription("data")
      cov$setContentType("physicalMeasurement")
      
      #adding dimensions
      for(variable in entity$data$variables){
        print(variable)
        band <- ISOBand$new()
        mn <- ISOMemberName$new(aName = variable,attributeType ="float")
        band$sequenceIdentifier<-mn
        band$descriptor<-attr(variable,"description")
        
        # unit<-attr(variable,"units")
        # if(length(unit)>0){
        #   gml<-GMLUnitDefinition$buildFrom(unit)
        #   if(is.null(gml)) invisible(capture.output(gml<-GMLUnitDefinition$buildFrom(unit,"name_singular"),type="message"))
        #   if(is.null(gml)) invisible(capture.output(gml<-GMLUnitDefinition$buildFrom(unit,"name_plural"),type="message"))
        #   if(!is.null(gml)) band$units<-gml
        # }
        band$units<-NA
        cov$dimension = c(cov$dimension, band)
      }
      md$contentInfo = c(md$contentInfo,cov)
    }
    if(!is.null(entity$data$dimensions)){
      #create coverage description
      cov <- ISOImageryCoverageDescription$new()
      cov$setAttributeDescription("data")
      cov$setContentType("coordinate")
      
      #adding dimensions
      
      for(dimension in names(entity$data$dimensions)){
        dim_name<-dimension
        dimension<-entity$data$dimensions[[dimension]]
        
        band <- ISOBand$new()
        
        mn <- ISOMemberName$new(aName = dim_name, attributeType = "float")
        band$sequenceIdentifier<-mn
        band$descriptor<-dimension$longName
        band$maxValue<-dimension$minValue
        band$minValue<-dimension$maxValue
        #unit
        # unit<-dimension$resolution$uom
        # if(length(unit)>0){
        #   invisible(capture.output(gml<-try(GMLUnitDefinition$buildFrom(unit)),type="message"))
        #   if(is.null(gml) | class(gml)=="try-error") invisible(capture.output(gml<-try(GMLUnitDefinition$buildFrom(unit,"name_singular")),type="message"))
        #   if(is.null(gml) | class(gml)=="try-error") invisible(capture.output(gml<-try(GMLUnitDefinition$buildFrom(unit,"name_plural")),type="message"))
        #   if(!is.null(gml) & class(gml)!="try-error") band$units<-gml
        # }
        band$units<-NA
        cov$dimension = c(cov$dimension, band)
        
        if(include_coverage_data_dimension_values){
          des <- ISOImageryRangeElementDescription$new()
          des$name<-dim_name
          des$definition<-dimension$longName
          des$rangeElement <- sapply(unique(dimension$values), function(x){ ISORecord$new(value = x)})
          cov$rangeElementDescription = c(cov$rangeElementDescription,des)
        }
      }
      md$contentInfo = c(md$contentInfo,cov)
    }
    if(!is.null(entity$data$ogc_dimensions)){
      #create coverage description
      cov <- ISOImageryCoverageDescription$new()
      cov$setAttributeDescription("service")
      cov$setContentType("coordinate")
      #adding dimensions
      for(ogc_dimension in names(entity$data$ogc_dimensions)){
        ogc_dim_name<-toupper(ogc_dimension)
        ogc_dimension<-entity$data$ogc_dimensions[[ogc_dimension]]
        band <- ISOBand$new()
        
        mn <- switch(ogc_dim_name,
                     "TIME"  = ISOMemberName$new(aName = ogc_dim_name, attributeType = "xsd:datetime"),
                     "ELEVATION" = ISOMemberName$new(aName = ogc_dim_name, attributeType = "xsd:decimal")
        )
        
        band$sequenceIdentifier<-mn
        #band$setUnits(gml)
        cov$dimension = c(cov$dimension, band)
        
        if(include_coverage_service_dimension_values){
          des <- ISOImageryRangeElementDescription$new()
          des$name<-ogc_dim_name
          des$definition<-""
          des$rangeElement <- sapply(ogc_dimension$values, function(x){ ISORecord$new(value = x)})
          cov$rangeElementDescription = c(cov$rangeElementDescription,des)
        }
      }
      md$contentInfo = c(md$contentInfo,cov)
    }
  }
  
  #distribution
  distrib <- ISODistribution$new()
  
  #distribution/contact
  if(length(entity$contacts)>0){
    distributors <- entity$contacts[sapply(entity$contacts, function(x){x$role == "distributor"})]
    if(length(distributors)==0) distributors <- list(entity$contacts[[1]])
    for(distributor_entity in distributors){
      dist_ent = ISODistributor$new()
      dist_rp<-createResponsibleParty(distributor_entity, roleId = "distributor") 
      dist_ent$setContact(dist_rp)
      distrib$addDistributor(dist_ent)
    }
  }
  
  #distribution/transfer options
  dto <- ISODigitalTransferOptions$new()
  
  #add online resource for DOI if existing
  if(!is.null(the_doi) & doi){
    doi_or <- ISOOnlineResource$new()
    doi_or$setLinkage(paste0("http://dx.doi.org/", the_doi))
    doi_or$setName("DOI")
    doi_desc = set_i18n(term_key = "doi")
    doi_or$setDescription(doi_desc, locales = geoflow::get_locales_from(doi_desc))
    doi_or$setProtocol("WWW:LINK-1.0-http--link")
    if(include_object_identification_ids) doi_or$setAttr("id", create_object_identification_id("onlineresource", the_doi))
    dto$addOnlineResource(doi_or)
  }
  
  #add distribution formats
  if(length(entity$formats)>0){
    distFormats = entity$formats[sapply(entity$formats, function(x){x$key == "distribution"})]
    if(length(distFormats)>0) for(distFormat in distFormats){
      format = ISOFormat$new()
      format_name = distFormat$name
      if(!is.null(distFormat$uri)){
        format_name <- ISOAnchor$new(name = distFormat$name, href = distFormat$uri)
      }
      format$setName(format_name)
      if(!is.null(ISOFormat$buildFrom)) format = ISOFormat$buildFrom(distFormat$name)
      format$setVersion(NA)
      format$setSpecification(distFormat$description)
      distrib$addFormat(format)
    }
  }
  
  #add online resource for each relation
  if(length(entity$relations)>0){
    http_relations <- entity$relations[sapply(entity$relations, function(x){
      x$key %in% c("ftp","http", "download", "wfs", "wms", "wms110", "wms111", "wms130", "wcs", "csw")
    })]
    for(http_relation in http_relations){
      or <- ISOOnlineResource$new()
      or$setLinkage(http_relation$link)
      
      mimeType <- http_relation$mimeType
      if(is.null(mimeType) && any(sapply(c("wms", "wfs", "wcs"), function(x){startsWith(http_relation$key, x)}))){
        mimeType <- "application/xml"
      }
      name = http_relation$name
      if(http_relation$key == "download") if(!is.null(mimeType)){
        name = ISOMimeFileType$buildFrom(mimeType)
        if(is.null(name)) name = ISOMimeFileType$new(type = mimeType, name = http_relation$name)
        name$setName(http_relation$name)
      }
      or$setName(name)
      or$setDescription(http_relation$description, locales = geoflow::get_locales_from(http_relation$description))
      protocol <- switch(http_relation$key,
                         "http" = "WWW:LINK-1.0-http--link",
                         "download" = "WWW:DOWNLOAD-1.0-http--download",
                         "wms" = "OGC:WMS", #defaut
                         "wms110" = "OGC:WMS-1.1.0-http-get-map",
                         "wms111" = "OGC:WMS-1.1.1-http-get-map",
                         "wms130" = "OGC:WMS-1.3.0-http-get-map",
                         "wfs" = "OGC:WFS",
                         "wfs100" = "OGC:WFS-1.0.0-http-get-feature",
                         "wfs110" = "OGC:WFS-1.1.0-http-get-feature",
                         "wfs200" = "OGC:WFS-2.0.0-http-get-feature",
                         "wcs" = "OGC:WCS",
                         "wcs100" = "OGC:WCS-1.0.0-http-get-coverage",
                         "wcs11"  = "OGC:WCS-1.1-http-get-coverage",
                         "wcs110" = "OGC:WCS-1.1.0-http-get-coverage",
                         "wcs111" = "OGC:WCS-1.1.1-http-get-coverage",
                         "wcs201" = "OGC:WCS-2.0.1-http-get-coverage",
                         "wcs210" = "OGC:WCS-2.1.0-http-get-coverage", 
                         "WWW:LINK-1.0-http--link"
      )
      or$setProtocol(protocol)
      
      if(include_object_identification_ids) if(any(sapply(c("wms", "wfs", "wcs","download"), function(x){startsWith(http_relation$key, x)}))) {
        resource_id = paste(tolower(entity$identifiers[["id"]]), http_relation$key, if(!is.null(mimeType)) mimeType else "", tolower(http_relation$name),sep="_")
        or$setAttr("id", create_object_identification_id("onlineresource", resource_id))
      }
      
      dto$onLine = c(dto$onLine,or)
    }
  }
  distrib$addDigitalTransferOptions(dto)
  md$setDistributionInfo(distrib)
  
  #data quality - provenance / lineage
  if(!is.null(entity$provenance)){
    dq_lineage <- ISODataQuality$new()
    dq_lineage_scope <- ISODataQualityScope$new()
    dq_lineage_scope$setLevel(dctype_iso)
    dq_lineage$setScope(dq_lineage_scope)
    lineage <- ISOLineage$new()
    lineage$setStatement(entity$provenance$statement, locales = geoflow::get_locales_from(entity$provenance$statement))
    processes <- entity$provenance$processes
    if(length(processes)>0){
      for(process in processes){
        processStep <- ISOProcessStep$new()
        processStep$setRationale(process$rationale, locales = geoflow::get_locales_from(process$rationale))
        processStep$setDescription(process$description, locales = geoflow::get_locales_from(process$description))
        
        #processor as responsability party
        for(processor in process$processors){
          rpp<-createResponsibleParty(processor, roleId = "processor") 
          processStep$addProcessor(rpp)
        }
        lineage$addProcessStep(processStep)
      }
    }
    dq_lineage$setLineage(lineage)
    md$addDataQualityInfo(dq_lineage)
  }
  
  #data quality other than lineage
  if(inspire){
    dq2 <- ISODataQuality$new()
    scope2 <- ISODataQualityScope$new()
    scope2$setLevel(dctype_iso)
    dq2$setScope(scope2)
    
    #INSPIRE - interoperability of spatial data sets and services
    dc_inspire1 <- ISODomainConsistency$new()
    cr_inspire1 <- ISOConformanceResult$new()
    cr_inspire_spec1 <- ISOCitation$new()
    inspire_sds = set_i18n("inspire_spatial_data_services")
    cr_inspire_spec1$setTitle(inspire_sds, locales = geoflow::get_locales_from(inspire_sds))
    cr_inspire1$setExplanation(NA)
    cr_inspire_date1 <- ISODate$new()
    cr_inspire_date1$setDate(as.Date(ISOdate(2010,12,8)))
    cr_inspire_date1$setDateType("publication")
    cr_inspire_spec1$addDate(cr_inspire_date1)
    cr_inspire1$setSpecification(cr_inspire_spec1)
    cr_inspire1$setPass(TRUE)
    dc_inspire1$addResult(cr_inspire1)
    dq2$addReport(dc_inspire1)
    #INSPIRE - metadata
    dc_inspire2 <- ISODomainConsistency$new()
    cr_inspire2 <- ISOConformanceResult$new()
    cr_inspire_spec2 <- ISOCitation$new()
    inspire_md = set_i18n("inspire_metadata")
    cr_inspire_spec2$setTitle(inspire_md, locales = geoflow::get_locales_from(inspire_md))
    cr_inspire2$setExplanation(NA)
    cr_inspire_date2 <- ISODate$new()
    cr_inspire_date2$setDate(as.Date(ISOdate(2008,12,4)))
    cr_inspire_date2$setDateType("publication")
    cr_inspire_spec2$addDate(cr_inspire_date2)
    cr_inspire2$setSpecification(cr_inspire_spec2)
    cr_inspire2$setPass(TRUE)
    dc_inspire2$addResult(cr_inspire2)
    dq2$addReport(dc_inspire2)
    md$addDataQualityInfo(dq2)
  }
  
  #content information --> Feature Catalogue description (if data handling)
  fc_action <- NULL
  actions <- config$actions[sapply(config$actions, function(x){x$id=="geometa-create-iso-19110"})]
  if(length(actions)>0) fc_action <- actions[[1]]
  if(!is.null(fc_action)){
    fcIdentifier <- paste0(entity$identifiers[["id"]],"_dsd")
    config$logger.info("Adding content information (feature catalogue description) to ISO 19115")
    fcd <- ISOFeatureCatalogueDescription$new()
    fcd$setComplianceCode(TRUE)
    fcd$addLanguage(entity$language)
    fcd$setIncludedWithDataset(FALSE)
    fcd$featureCatalogueCitation <- list(ISOAttributes$new(uuidref = fcIdentifier))
    md$addContentInfo(fcd)
  }
  
  #we save the metadata
  #saveRDS(md, file.path(getwd(), "metadata", paste0(entity$identifiers[["id"]], ".rds")))
  md$save(file.path(getwd(), "metadata", paste0(entity$getEntityJobDirname(), "_ISO-19115.xml")), 
          inspire = inspire, inspireValidator = INSPIRE_VALIDATOR)
  rm(md)

}
