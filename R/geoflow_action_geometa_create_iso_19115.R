geometa_create_iso_19115 <- function(entity, config, options){
  
  ISOMetadataNamespace$GML$uri <- "http://www.opengis.net/gml/3.2"
  
  if(!require("geometa")){
    stop("This action requires the 'geometa' package")
  }
  
  #options
  inspire <- if(!is.null(options$inspire)) options$inspire else FALSE
  logo <- if(!is.null(options$logo)) options$logo else FALSE
  doi <- if(!is.null(options$doi)) options$doi else FALSE
  
  #metadata creation
  #-----------------------------------------------------------------------------------------------------
  #create geometa object
  md <- ISOMetadata$new()
  mdId <- entity$identifiers[["id"]]
  md$setFileIdentifier(mdId)
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
    md$addContact(rp)
  }
  
  #TODO spatial representation
  
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
  #TODO credit (N)
  #TODO status (N)
  ident$setLanguage(entity$language)
  ident$setCharacterSet("utf8")
  #TODO TopicCategory (N)
  
  #adding contacts
  for(entity_contact in entity$contacts){
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
  #methodology to set DOI inspired by NOAA wiki
  #https://geo-ide.noaa.gov/wiki/index.php?title=DOI_Minting_Procedure#Third.2C_Include_the_DOI_and_citation_text_in_the_ISO_Metadata_Record
  the_doi <- entity$identifiers[["doi"]]
  if(is.null(the_doi)) the_doi <- entity$identifiers[["conceptdoi_to_save"]]
  if(is.null(the_doi)) the_doi <- entity$identifiers[["doi_to_save"]]
  if(!is.null(the_doi) & doi){
    mdIdentifier <- ISOAnchor$new(
      name = paste0("doi:", the_doi), 
      href = paste0("http://dx.doi.org/", the_doi)
    )
    mdIdentifier$setAttr("xlink:title", "DOI")
    mdIdentifier$setAttr("xlink:actuate", "onRequest")
    ct$addIdentifier(ISOMetaIdentifier$new(code = mdIdentifier))
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
  if(!is.null(entity$spatial_extent)){
    sf_bbox <- entity$spatial_extent
    bbox <- ISOGeographicBoundingBox$new(minx = sf_bbox$xmin, miny = sf_bbox$ymin, maxx = sf_bbox$xmax, maxy = sf_bbox$ymax)
    extent$setGeographicElement(bbox)
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
    extent$setTemporalElement(time)
  }
  ident$setExtent(extent)
  
  #thesaurus/keywords
  for(subject in entity$subjects){
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
  #TODO spatial representation type
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
        "wms" = "OGC:WMS-1.3.0-http-get-map",
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
  
  #TODO data quality other than lineage
  #e.g. INSPIRE conformance?
  
  #TODO content information --> Feature Catalogue description (if data handling)
  
  #we save the metadata
  saveRDS(md, file.path(getwd(), "metadata", paste0(entity$identifiers[["id"]], ".rds")))
  md$save(file.path(getwd(), "metadata", paste0(entity$identifiers[["id"]], "_ISO-19115.xml")), inspire = inspire)
  
  return(md)
}