#handle_entities_csw
handle_entities_csw <- function(handler, source, config, handle = TRUE){
  
  if(!requireNamespace("ows4R", quietly = TRUE)){
    stop("The OGC CSW handler requires the 'ows4R' package")
  }
  if(!requireNamespace("sf", quietly = TRUE)){
    stop("The OGC CSW handler requires the 'sf' package")
  }
  if(!requireNamespace("geometa", quietly = TRUE)){
    stop("The OGC CSW handler requires the 'geometa' package")
  }
  
  CSW = config$software$input$csw
  CSW_CONFIG = config$software$input$csw_config
  if(is.null(CSW)){
    stop("There is no 'csw' input software configured to handle entities from an CSW service endpoint")
  }
  
  recs = if(is.null(source)){
    CSW$getRecords()
  }else{
    cons <- CSWConstraint$new(cqlText = source)
    query <- CSWQuery$new(constraint = cons)
    CSW$getRecords(query = query, outputSchema = "http://www.isotc211.org/2005/gmd")
  }
  
  createContactFromResponsibleParty = function(rp){
    contact = geoflow_contact$new()
    if(!is.null(rp$contactInfo$address$electronicMailAddress)) if(!is.na(rp$contactInfo$address$electronicMailAddress)) contact$identifiers[["id"]] = rp$contactInfo$address$electronicMailAddress
    if(!is.null(rp$organisationName)) if(!is.na(rp$organisationName)) contact$setOrganizationName(rp$organisationName)
    ind = rp$individualName
    if(!is.null(ind)) if(!is.na(ind)){
      ind_parts = unlist(strsplit(ind, " "))
      contact$setFirstName(ind_parts[1])
      contact$setLastName(ind_parts[2])
    }
    
    #we do a digest on contact to identify it, based on names, email eventually
    contact$setIdentifier(key = "digest", digest::digest(contact))
    
    if(!is.null(rp$positionName)) if(!is.na(rp$positionName)) contact$setPositionName(rp$positionName)
    
    if(length(rp$contactInfo$address)>0){
      address = rp$contactInfo$address[[1]]
      if(length(address$deliveryPoint)>0) if(!is.na(address$deliveryPoint)){
        dp = address$deliveryPoint
        if(is.list(dp)) dp = dp[[1]]
        contact$setPostalAddress(dp)
      }
      if(!is.null(address$postalCode)) if(!is.na(address$postalCode)) contact$setPostalCode(address$postalCode)
      if(!is.null(address$city)) if(!is.na(address$city)) contact$setCity(address$city)
      if(!is.null(address$country)) if(!is.na(address$country)) contact$setCountry(address$country)
      if(length(address$electronicMailAddress)>0) if(!is.na(address$electronicMailAddress)) {
        email = address$electronicMailAddress
        if(is.list(email)) email = email[[1]]
        contact$setEmail(email)
      }
    }
    if(length(rp$contactInfo$phone)>0){
      phone = rp$contactInfo$phone[[1]]
      if(!is.null(phone$voice)) if(!is.na(phone$voice)) contact$setVoice(phone$voice)
      if(!is.null(phone$facsimile)) if(!is.na(phone$facsimile)) contact$setFacsimile(phone$facsimile)
    }
    if(length(rp$contactInfo$onlineResource)>0){
      or = rp$contactInfo$onlineResource[[1]]
      if(!is.null(or$name)) if(!is.na(or$name)) contact$setWebsiteName(or$name)
      if(is(or$linkage, "ISOURL")){
        contact$setWebsiteUrl(or$linkage$value)
      }
    }
    contact$setRole(rp$role$attrs$codeListValue)
    return(contact$clone(deep = TRUE))
  }
  
  contacts = do.call("c", lapply(recs, function(rec){
    entity_contacts = list()
    #metadata contacts
    for(poc in rec$contact) if(length(poc)>1){
      contact_metadata = createContactFromResponsibleParty(poc)
      contact_metadata$setRole("metadata")
      entity_contacts = c(entity_contacts, contact_metadata)
    }
    #identificationInfo metadata fields
    if(length(rec$identificationInfo)>0){
      
      #pocs
      for (poc in rec$identificationInfo[[1]]$pointOfContact) if(length(poc)>1){
        contact_metadata = createContactFromResponsibleParty(poc)
        contact_metadata$setRole("pointOfContact")
        entity_contacts = c(entity_contacts, contact_metadata)
      }
      
      #cited responsible party
      rps = rec$identificationInfo[[1]]$citation$citedResponsibleParty
      for(rp in rps){
        rp_contact = createContactFromResponsibleParty(rp)
        entity_contacts = c(entity_contacts, rp_contact)
      }
    }
 
       #distributionInfo metadata fields
    if(length(rec$distributionInfo)>0){
      #distributors
      distributors = rec$distributionInfo$distributor
      for(distributor in distributors){
        dist_contact= createContactFromResponsibleParty(distributor$distributorContact)
        entity_contacts = c(entity_contacts, dist_contact)
      }
    }
    return(entity_contacts)
  }))
  
  if(any(duplicated(sapply(contacts, function(x){x$identifiers$digest})))){
    contacts = contacts[!duplicated(sapply(contacts, function(x){x$identifiers$digest}))]
  }
  
  return(contacts)
}