geometa_create_iso_19115 <- function(entity, config, options){
  
  ISOMetadataNamespace$GML$uri <- "http://www.opengis.net/gml/3.2"
  
  #features if any
  features <- entity$data$features
  
  #options
  inspire <- if(!is.null(options$inspire)) options$inspire else FALSE
  logo <- if(!is.null(options$logo)) options$logo else FALSE
  doi <- if(!is.null(options$doi)) options$doi else FALSE
  addfeatures <- if(!is.null(options$addfeatures)) options$addfeatures else FALSE
  featureid <- if(!is.null(options$featureid)){ options$featureid } else { if(!is.null(features)) colnames(features)[1] else NULL} 
  
  #metadata creation
  #-----------------------------------------------------------------------------------------------------
  #create geometa object
  md <- ISOMetadata$new()
  mdId <- entity$identifiers[["id"]]
  md$setFileIdentifier(mdId)
  
  the_doi <- entity$identifiers[["doi"]]
  if(is.null(the_doi)) the_doi <- entity$identifiers[["conceptdoi_to_save"]]
  if(is.null(the_doi)) the_doi <- entity$identifiers[["doi_to_save"]]
  
  if(length(entity$relations)>0){
    parent_rels <- entity$relations[sapply(entity$relations, function(x){x$key == "parent"})]
    if(length(parent_rels)>0){
      parent <- parent_rels[[1]]
      parentId <- parent$label
      if(!is.null(parent$link)) parentId <- ISOAnchor$new(name = parent$label, href = parent$link)
      md$setParentIdentifier(parentId)
    }
  }
  md$setCharacterSet("utf8")
  md$setLanguage(entity$language)
  md$setDateStamp(Sys.time())
  md$setMetadataStandardName("ISO 19115:2003/19139")
  md$setMetadataStandardVersion("1.0")
  md$setDataSetURI(md$fileIdentifier)
  md$setHierarchyLevel(entity$types[["generic"]])
  
  #add contacts
  for(entity_contact in entity$contacts){
    if(tolower(entity_contact$role) == "metadata"){
      rp <- ISOResponsibleParty$new()
      rp$setIndividualName(paste(entity_contact$firstName, entity_contact$lastName))
      rp$setOrganisationName(entity_contact$organizationName)
      rp$setPositionName(entity_contact$positionName)
      rp$setRole("pointOfContact")
      contact <- ISOContact$new()
      phone <- ISOTelephone$new()
      phone$setVoice(entity_contact$voice)
      phone$setFacsimile(entity_contact$facsimile)
      contact$setPhone(phone)
      address <- ISOAddress$new()
      address$setDeliveryPoint(entity_contact$postalAddress)
      address$setCity(entity_contact$city)
      address$setPostalCode(entity_contact$postalCode)
      address$setCountry(entity_contact$country)
      address$setEmail(entity_contact$email)
      contact$setAddress(address)
      res <- ISOOnlineResource$new()
      res$setLinkage(entity_contact$websiteUrl)
      res$setName(entity_contact$websiteName)
      contact$setOnlineResource(res)
      rp$setContactInfo(contact)
      md$addContact(rp)
    }
  }
  
  #spatial representation
  spatialRepresentationType <- NULL
  if(!is.null(features)){
    #support vector spatial representation
    if(is(features, "sf")){
      geomtypes <- as.list(table(st_geometry_type(features)))
      geomtypes <- geomtypes[geomtypes > 0]
      if(length(geomtypes)>0){
        spatialRepresentationType <- "vector"
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
            geomObject$setGeometricObjectCount(nrow(features[st_geometry_type(features)==geomtype,]))
            vsr$setGeometricObjects(geomObject)
          }
          md$addSpatialRepresentationInfo(vsr)
        }
      }else{
        spatialRepresentationType <- "textTable"
      }
    }
  }
  
  #spatial reference system
  if(!is.null(entity$srid)){
    rs <- ISOReferenceSystem$new()
    rsId <- ISOReferenceIdentifier$new(code = as.character(entity$srid), codeSpace = "EPSG")
    rs$setReferenceSystemIdentifier(rsId)
    md$setReferenceSystemInfo(rs)
  }
  
  #Data identification
  ident <- ISODataIdentification$new()
  ident$setAbstract(entity$descriptions[["abstract"]])
  ident$setPurpose(entity$descriptions[["purpose"]])
  ident$setCredit(entity$descriptions[["credit"]])
  #TODO status (N)
  ident$setLanguage(entity$language)
  ident$setCharacterSet("utf8")
  #topic categories
  topics <- list()
  if(length(entity$subjects)>0) topics <- entity$subjects[sapply(entity$subjects, function(x){return(tolower(x$name) == "topic")})]
  if(length(topics)>0){
    for(topic in topics){
      for(topicCategory in topic$keywords) ident$addTopicCategory(topicCategory$name)
    }
  }
  
  #adding contacts
  for(entity_contact in entity$contacts){
    if(tolower(entity_contact$role) != "metadata"){
      rp <- ISOResponsibleParty$new()
      rp$setIndividualName(paste(entity_contact$firstName, entity_contact$lastName))
      rp$setOrganisationName(entity_contact$organizationName)
      rp$setPositionName(entity_contact$positionName)
      rp$setRole(entity_contact$role)
      contact <- ISOContact$new()
      phone <- ISOTelephone$new()
      phone$setVoice(entity_contact$voice)
      phone$setFacsimile(entity_contact$facsimile)
      contact$setPhone(phone)
      address <- ISOAddress$new()
      address$setDeliveryPoint(entity_contact$postalAddress)
      address$setCity(entity_contact$city)
      address$setPostalCode(entity_contact$postalCode)
      address$setCountry(entity_contact$country)
      address$setEmail(entity_contact$email)
      contact$setAddress(address)
      res <- ISOOnlineResource$new()
      res$setLinkage(entity_contact$websiteUrl)
      res$setName(entity_contact$websiteName)
      contact$setOnlineResource(res)
      rp$setContactInfo(contact)
      ident$addPointOfContact(rp)
    }
  }

  #citation
  ct <- ISOCitation$new()
  ct$setTitle(entity$title)
  d <- ISODate$new()
  d$setDate(Sys.Date())
  d$setDateType("publication")
  ct$addDate(d)
  ct$setEdition("1.0") #TODO
  editionDate <- if(!is.null(entity$date)) entity$date else Sys.Date()
  ct$setEditionDate(editionDate)
  
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
  main_entity <- NULL
  owners <- entity$contacts[sapply(entity$contacts, function(x){x$role == "owner"})]
  if(length(owners)==0) main_entity <- entity$contacts[[1]] else main_entity <- owners[[1]]
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName(paste(main_entity$firstName, main_entity$lastName))
  rp$setOrganisationName(main_entity$organizationName)
  rp$setPositionName(main_entity$positionName)
  rp$setRole(main_entity$role)
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice(main_entity$voice)
  phone$setFacsimile(main_entity$facsimile)
  contact$setPhone(phone)
  address <- ISOAddress$new()
  address$setDeliveryPoint(main_entity$postalAddress)
  address$setCity(main_entity$city)
  address$setPostalCode(main_entity$postalCode)
  address$setCountry(main_entity$country)
  address$setEmail(main_entity$email)
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage(main_entity$websiteUrl)
  res$setName(main_entity$websiteName)
  contact$setOnlineResource(res)
  rp$setContactInfo(contact) 
  ct$addCitedResponsibleParty(rp)
  ident$setCitation(ct)
 
  #graphic overviews
  if(length(entity$relations)>0){
    thumbnails <- entity$relations[sapply(entity$relations, function(x){x$key == "thumbnail"})]
    for(thumbnail in thumbnails){
      go <- ISOBrowseGraphic$new(
        fileName = thumbnail$link,
        fileDescription = thumbnail$name
      )
      ident$addGraphicOverview(go)
    }
  }
  #option to add logo as thumbnail
  if(logo && !is.null(config$profile$logos)){
    for(logo in config$profile$logos){
      logoThumbnail <- ISOBrowseGraphic$new(fileName = logo, fileDescription = "Logo")
      ident$addGraphicOverview(logoThumbnail)
    }
  }
  
  #maintenance information
  maint <- ISOMaintenanceInformation$new()
  maint$setMaintenanceFrequency("asNeeded")
  ident$addResourceMaintenance(maint)
  
  #legal constraints
  if(length(entity$rights)>0){
    legal_constraints <- ISOLegalConstraints$new()
    #use limitation
    uses <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "use"})]
    if(length(uses)>0){
      for(use in uses) legal_constraints$addUseLimitation(use$value)
    }
    #use constraints
    useConstraints <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "useconstraint"})]
    if(length(useConstraints)>0){
      for(useConstraint in useConstraints) legal_constraints$addUseConstraint(useConstraint$value)
    }
    #access constraints
    accessConstraints <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "accessconstraint"})]
    if(length(accessConstraints)>0){
      for(accessConstraint in accessConstraints) legal_constraints$addAccessConstraint(accessConstraint$value)
    }
    #other constraints
    otherConstraints <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "otherconstraint"})]
    if(length(otherConstraints)>0){
      for(otherConstraint in otherConstraints) legal_constraints$addOtherConstraint(otherConstraint$value)
    }
    ident$addResourceConstraints(legal_constraints)
  }
  
  #extents
  extent <- ISOExtent$new()
  #geographic extent
  if(!is.null(entity$spatial_bbox)){
    sf_bbox <- entity$spatial_bbox
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
    for(i in 1:nrow(features)){
      geom <- GMLAbstractGeometry$fromSimpleFeatureGeometry(features[i,]$geometry[[1]])
      geom$attrs["gml:id"] <- paste0("fid.",as.character(features[i,][featureid])[1])
      bp$polygon <- c(bp$polygon, geom)
    }
    extent$addGeographicElement(bp)
  }
  #temporal extent
  if(!is.null(entity$temporal_extent)){
    time <- ISOTemporalExtent$new()
    if(!is.null(entity$temporal_extent$instant)){
      gmltimeinstant <- GMLTimeInstant$new(timePosition = entity$temporal_extent$instant)
      time$setTimeInstant(gmltimeinstant)
    }
    if(!is.null(entity$temporal_extent$start) & !is.null(entity$temporal_extent$end)){
      gmltimeperiod <- GMLTimePeriod$new(beginPosition = entity$temporal_extent$start, endPosition = entity$temporal_extent$end)
      time$setTimePeriod(gmltimeperiod)
    }
    extent$addTemporalElement(time)
  }
  ident$setExtent(extent)
  
  #thesaurus/keywords
  subjects <- entity$subjects
  if(length(subjects)>0) subjects <- subjects[sapply(subjects, function(x){return(x$name != "topic")})]
  if(length(subjects)>0) for(subject in subjects){
    #add keywords
    kwds <- ISOKeywords$new()
    for(kwd in subject$keywords){
      iso_kwd <- kwd$name
      if(!is.null(kwd$uri)){
        iso_kwd <- ISOAnchor$new(name = kwd$name, href = kwd$uri)
      }
      kwds$addKeyword(iso_kwd)
    }
    kwds$setKeywordType("theme") #TODO need to handle keywordType in thesaurus table definition...
    th <- ISOCitation$new()
    title <- subject$name
    if(!is.null(subject$uri)){
      title <- ISOAnchor$new(name = subject$name, href = subject$uri)
    }
    th$setTitle(title)
    
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
    ident$addKeywords(kwds)
  }
  
  ident$setSupplementalInformation(entity$descriptions[["info"]])
  if(!is.null(spatialRepresentationType)) ident$setSpatialRepresentationType(spatialRepresentationType)
  md$addIdentificationInfo(ident)
  
  #distribution
  distrib <- ISODistribution$new()
  dto <- ISODigitalTransferOptions$new()
  
  #add online resource for DOI if existing
  if(!is.null(the_doi) & doi){
    doi_or <- ISOOnlineResource$new()
    doi_or$setLinkage(paste0("http://dx.doi.org/", the_doi))
    doi_or$setName("DOI")
    doi_or$setDescription("Digital Object Identifier")
    doi_or$setProtocol("WWW:LINK-1.0-http--link")
    dto$addOnlineResource(doi_or)
  }
  
  #add online resource for each relation
  if(length(entity$relations)>0){
    http_relations <- entity$relations[sapply(entity$relations, function(x){
      x$key %in% c("ftp","http", "wfs", "wms", "wcs", "csw")
    })]
    for(http_relation in http_relations){
      or <- ISOOnlineResource$new()
      or$setLinkage(http_relation$link)
      or$setName(http_relation$name)
      or$setDescription(http_relation$description)
      protocol <- switch(http_relation$key,
        "http" = "WWW:LINK-1.0-http--link",
        "wms" = "OGC:WMS-1.1.0-http-get-map",
        "WWW:LINK-1.0-http--link"
      )
      or$setProtocol(protocol)
      dto$addOnlineResource(or)
    }
  }
  distrib$setDigitalTransferOptions(dto)
  md$setDistributionInfo(distrib)
  
  #data quality - provenance / lineage
  if(!is.null(entity$provenance)){
    dq_lineage <- ISODataQuality$new()
    dq_lineage_scope <- ISOScope$new()
    dq_lineage_scope$setLevel(entity$types[["generic"]])
    dq_lineage$setScope(dq_lineage_scope)
    lineage <- ISOLineage$new()
    lineage$setStatement(entity$provenance$statement)
    processes <- entity$provenance$processes
    if(length(processes)>0){
      for(process in processes){
        processStep <- ISOProcessStep$new()
        processStep$setRationale(process$rationale)
        processStep$setDescription(process$description)
        
        #processor as responsability party
        processor <- process$processor
        rpp <- ISOResponsibleParty$new()
        rpp$setIndividualName(paste(processor$firstName, processor$lastName))
        rpp$setOrganisationName(processor$organizationName)
        rpp$setPositionName(processor$positionName)
        rpp$setRole(processor$role)
        contact <- ISOContact$new()
        phone <- ISOTelephone$new()
        phone$setVoice(processor$voice)
        phone$setFacsimile(processor$facsimile)
        contact$setPhone(phone)
        address <- ISOAddress$new()
        address$setDeliveryPoint(processor$postalAddress)
        address$setCity(processor$city)
        address$setPostalCode(processor$postalCode)
        address$setCountry(processor$country)
        address$setEmail(processor$email)
        contact$setAddress(address)
        res <- ISOOnlineResource$new()
        res$setLinkage(processor$websiteUrl)
        res$setName(processor$websiteName)
        contact$setOnlineResource(res)
        rpp$setContactInfo(contact) 
        processStep$addProcessor(rpp)
        lineage$addProcessStep(processStep)
      }
    }
    dq_lineage$setLineage(lineage)
    md$addDataQualityInfo(dq_lineage)
  }
  
  #data quality other than lineage
  if(inspire){
    dq2 <- ISODataQuality$new()
    scope2 <- ISOScope$new()
    scope2$setLevel(entity$types[["generic"]])
    dq2$setScope(scope2)
    
    #INSPIRE - interoperability of spatial data sets and services
    dc_inspire1 <- ISODomainConsistency$new()
    cr_inspire1 <- ISOConformanceResult$new()
    cr_inspire_spec1 <- ISOCitation$new()
    cr_inspire_spec1$setTitle("Commission Regulation (EU) No 1089/2010 of 23 November 2010 implementing Directive 2007/2/EC of the European Parliament and of the Council as regards interoperability of spatial data sets and services")
    cr_inspire1$setExplanation("See the referenced specification")
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
    cr_inspire_spec2$setTitle("COMMISSION REGULATION (EC) No 1205/2008 of 3 December 2008 implementing Directive 2007/2/EC of the European Parliament and of the Council as regards metadata")
    cr_inspire2$setExplanation("See the referenced specification")
    cr_inspire_date2 <- ISODate$new()
    cr_inspire_date2$setDate(as.Date(ISOdate(2008,12,4)))
    cr_inspire_date2$setDateType("publication")
    cr_inspire_spec2$addDate(cr_inspire_date2)
    cr_inspire2$setSpecification(cr_inspire_spec2)
    cr_inspire2$setPass(TRUE)
    dc_inspire2$addResult(cr_inspire2)
    dq2$addReport(dc_inspire2)
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
  saveRDS(md, file.path(getwd(), "metadata", paste0(entity$identifiers[["id"]], ".rds")))
  md$save(file.path(getwd(), "metadata", paste0(entity$identifiers[["id"]], "_ISO-19115.xml")), inspire = inspire)
  
  return(md)
}