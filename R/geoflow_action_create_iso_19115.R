geoflow_action_create_iso_19115 <- function(entity, config, options){
  
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
  
  #spatial representation
  
  
 
  #we save the metadata
  md$save(file.path(getwd(), "metadata", paste0(entity$id, ".xml")), inspire = inspire)
  
  return(md)
}