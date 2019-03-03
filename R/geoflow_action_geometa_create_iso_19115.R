geometa_create_iso_19115 <- function(entity, config, options){
  
  if(!require("geometa")){
    stop("This action requires the 'geometa' package")
  }
  
  #options
  inspire <- FALSE
  if(!is.null(options$inspire)) inspire <- options$inspire
  
  #create geometa object
  md <- ISOMetadata$new()
  md$setFileIdentifier(entity$id)
  parent_rels <- entity$relations[sapply(entity$relations, function(x){x$key == "parent"})]
  if(length(parent_rels)>0){
    parent <- parent_rels[[1]]
    parentId <- parent$label
    if(!is.null(parent$link)) parentId <- ISOAnchor$new(name = parent$label, href = parent$link)
    md$setParentIdentifier(parentId)
  }
  md$setCharacterSet("utf8")
  md$setLanguage(entity$language)
  md$setDateStamp(Sys.time())
  md$setMetadataStandardName("ISO 19115:2003/19139")
  md$setMetadataStandardVersion("1.0")
  md$setDataSetURI(md$fileIdentifier)
  
  #add contacts
  for(entity_contact in entity$contacts){
    rp <- ISOResponsibleParty$new()
    rp$setIndividualName(entity_contact$individualName)
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
    res$setLinkage(entity_contact$website)
    res$setName("Website") #TODO writing convention in contacts table
    contact$setOnlineResource(res)
    rp$setContactInfo(contact)
    md$addContact(rp)
  }
  
  #TODO spatial representation
  
  #TODO spatial reference system
  
  #Data identification
  ident <- ISODataIdentification$new()
  ident$setAbstract(entity$abstract)
  #TODO purpose
  #TODO credit (N)
  #TODO status (N)
  ident$setLanguage(entity$language)
  ident$setCharacterSet("utf8")
  #TODO TopicCategory (N)
  
  #adding contacts
  for(entity_contact in entity$contacts){
    rp <- ISOResponsibleParty$new()
    rp$setIndividualName(entity_contact$individualName)
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
    res$setLinkage(entity_contact$website)
    res$setName("Website") #TODO writing convention in contacts table
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
  ct$setEditionDate(as.Date(ISOdate(2015, 1, 1, 1))) #TODO to map with gsheet
  ct$setIdentifier(ISOMetaIdentifier$new(code = entity$id))
  ct$setPresentationForm("mapDigital") #TODO to map with gsheet
  
  #adding responsible party (search for owner, otherwise take first contact)
  main_entity <- NULL
  owners <- entity$contacts[sapply(entity$contacts, function(x){x$role == "owner"})]
  if(length(owners)==0) main_entity <- entity$contacts[[1]] else main_entity <- owners[[1]]
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName(main_entity$individualName)
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
  res$setLinkage(main_entity$website)
  res$setName("Website") #TODO writing convention in contacts table
  contact$setOnlineResource(res)
  rp$setContactInfo(contact) 
  ct$setCitedResponsibleParty(rp)
  ident$setCitation(ct)
 
  #graphic overviews
  thumbnails <- entity$relations[sapply(entity$relations, function(x){x$key == "thumbnail"})]
  for(thumbnail in thumbnails){
    go <- ISOBrowseGraphic$new(
      fileName = thumbnail$link,
      fileDescription = thumbnail$label
    )
    ident$addGraphicOverview(go)
  }
  
  #TODO maintenance information?
  #TODO legal constraints?
  #TODO security constraints?
  #TODO geographic extent
  #TODO temporal extent
  #TODO thesaurus/keywords
  #TODO supplemental info?
  #TODO spatial representation type
  md$addIdentificationInfo(ident)
  
  #TODO distribution
  
  #TODO data quality
  
  #TODO content information --> Feature Catalogue description (if data handling)
  
  #we save the metadata
  md$save(file.path(getwd(), "metadata", paste0(entity$id, ".xml")), inspire = inspire)
  
  return(md)
}