geometa_create_iso_19115 <- function(entity, config, options){
  
  ISOMetadataNamespace$GML$uri <- "http://www.opengis.net/gml/3.2"
  
  #features if any
  features <- entity$data$features
  
  #options
  inspire <- if(!is.null(options$inspire)) options$inspire else FALSE
  logo <- if(!is.null(options$logo)) options$logo else FALSE
  doi <- if(!is.null(options$doi)) options$doi else FALSE
  doi_thumbnail <- if(!is.null(options$doi_thumbnail)) options$doi_thumbnail else FALSE
  addfeatures <- if(!is.null(options$addfeatures)) options$addfeatures else FALSE
  featureid <- if(!is.null(options$featureid)){ options$featureid } else { if(!is.null(features)) colnames(features)[1] else NULL} 
  geographySubject <- if(!is.null(options$subject_geography)) options$subject_geography else "geography"
  
  createResponsibleParty = function(x, role = NULL){
    if(is.null(role)) role <- x$role 
    rp <- ISOResponsibleParty$new()
    if(is.null(x$firstName)) x$firstName = NA
    if(is.null(x$lastName)) x$lastName = NA
    if(!is.na(x$firstName) && !is.na(x$lastName)) rp$setIndividualName(paste(x$firstName, x$lastName))
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
    
    return(rp)
  }
  
  #metadata creation
  #-----------------------------------------------------------------------------------------------------
  #create geometa object
  md <- ISOMetadata$new()
  mdId <- entity$identifiers[["id"]]
  md$setFileIdentifier(mdId)
  
  the_doi <- entity$identifiers[["doi"]]
  if(is.null(the_doi)) {
    #no DOI set in initial entity, let's look for a reserved DOI
    if(any(regexpr("zenodo", entity$identifiers)>0)) {
      the_doi <- entity$identifiers[["zenodo_conceptdoi_to_save"]]
      the_doi <- entity$identifiers[["zenodo_doi_to_save"]]
    } else if(any(regexpr("dataverse", entity$identifiers)>0)){
      the_doi <- entity$identifiers[["dataverse_conceptdoi_to_save"]]
      the_doi <- entity$identifiers[["dataverse_doi_to_save"]]
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
  md$setDateStamp(Sys.time())
  
  if(!is.null(entity$data)) {
    md$setMetadataStandardName(switch(entity$data$spatialRepresentationType,
                                      "vector" = "ISO 19115:2003 Geographic information — Metadata",
                                      "grid" = "ISO 19115-2 Geographic Information - Metadata Part 2 Extensions for imagery and gridded data"
    ))
    md$setMetadataStandardVersion(switch(entity$data$spatialRepresentationType,
                                         "vector" = "ISO 19115:2003",
                                         "grid" = "ISO 19115-2:2009"
    ))
  }else{
    md$setMetadataStandardName("ISO 19115:2003 Geographic information — Metadata")
    md$setMetadataStandardVersion("ISO 19115:2003")
  }
  
  md$setDataSetURI(md$fileIdentifier)
  
  dctype <- entity$types[["generic"]]
  dctype_idx = which(tolower(ISOHierarchyLevel$values()) == tolower(dctype))
  dctype_iso = ISOHierarchyLevel$values()[dctype_idx]
  if(length(dctype_iso)==0) dctype_iso = "dataset"
  md$setHierarchyLevel(dctype_iso)
  
  #add contacts
  if(length(entity$contacts)>0)for(entity_contact in entity$contacts){
    if(tolower(entity_contact$role) == "metadata"){
      rp<-createResponsibleParty(entity_contact,"pointOfContact") 
      md$addContact(rp)
    }
  }
  
  #spatial representation
  spatialRepresentationType <- entity$data$spatialRepresentationType
  if(spatialRepresentationType=="vector"){
    if(!is.null(features)){
      #support vector spatial representation
      if(is(features, "sf")){
        geomtypes <- as.list(table(st_geometry_type(features)))
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
  }
  
  if(spatialRepresentationType=="grid"){
    gsr <- ISOGridSpatialRepresentation$new()
    gsr$setNumberOfDimensions(length(entity$data$dimensions))
    for(dimension in entity$data$dimensions){
      dimObject <- ISODimension$new()
      dimObject$setName(names(dimension[[1]]))
      dimObject$setSize(dimension[[1]]$size)
      dim1$setResolution(ISOMeasure$new(value=dimension[[1]]$resolution$value,uom=dimension[[1]]$resolution$uom))
    grs$addDimensions(dimObject)
    }
    grs$dimObject$setCellGeometry("area")
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
  ident$addCredit(entity$descriptions[["credit"]])
  ident$addStatus(entity$descriptions[["status"]])
  ident$setLanguage(entity$language)
  ident$setCharacterSet("utf8")
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
    if(tolower(entity_contact$role) != "metadata"){
      rp<-createResponsibleParty(entity_contact) 
      ident$addPointOfContact(rp)
    }
  }

  #citation
  now <- Sys.time()
  ct <- ISOCitation$new()
  ct$setTitle(entity$titles[["title"]])
  if("alternative" %in% names(entity$titles)){
    ct$setAlternateTitle(entity$titles[["alternative"]])
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
  editionDates = entity$dates[sapply(entity$dates, function(x){x$key == "edition"})]
  if(length(editionDates)>0){
    editionDate = editionDates[[1]]$value
    ct$setEditionDate(editionDate)
  }
  
  #edition
  if(!is.null(entity$descriptions[["edition"]])){
    edition = entity$descriptions[["edition"]]
    ct$setEdition(edition)
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
    if(length(owners)==0) owners <- list(entity$contacts[[1]])
    for(owner_entity in owners){
      rp<-createResponsibleParty(owner_entity) 
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
        fileDescription = thumbnail$name
      )
      ident$addGraphicOverview(go)
    }
  }
  #resource formats
  if(length(entity$formats)>0){
    resourceFormats = entity$formats[sapply(entity$formats, function(x){x$key == "resource"})]
    for(resourceFormat in resourceFormats){
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
  if(length(subjects)>0) subjects <- subjects[sapply(subjects, function(x){return(x$key != "topic")})]
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
    kwds$setKeywordType(subject$key)
    #theausurus
    if(!is.null(subject$name)){
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
    }
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
  
  #add distribution formats
  if(length(entity$formats)>0){
    distFormats = entity$formats[sapply(entity$formats, function(x){x$key == "distribution"})]
    for(distFormat in distFormats){
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
      x$key %in% c("ftp","http", "wfs", "wms", "wms110", "wms111", "wms130", "wcs", "csw")
    })]
    for(http_relation in http_relations){
      or <- ISOOnlineResource$new()
      or$setLinkage(http_relation$link)
      or$setName(http_relation$name)
      or$setDescription(http_relation$description)
      protocol <- switch(http_relation$key,
        "http" = "WWW:LINK-1.0-http--link",
        "wms" = "OGC:WMS-1.1.0-http-get-map", #defaut
        "wms110" = "OGC:WMS-1.1.0-http-get-map",
        "wms111" = "OGC:WMS-1.1.1-http-get-map",
        "wms130" = "OGC:WMS-1.3.0-http-get-map",
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
    dq_lineage_scope$setLevel(dctype_iso)
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
        rpp<-createResponsibleParty(processor) 
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
    scope2$setLevel(dctype_iso)
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
  #saveRDS(md, file.path(getwd(), "metadata", paste0(entity$identifiers[["id"]], ".rds")))
  md$save(file.path(getwd(), "metadata", paste0(entity$getEntityJobDirname(), "_ISO-19115.xml")), inspire = inspire)
  rm(md)
}
