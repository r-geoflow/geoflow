eml_create_eml <- function(entity, config, options){
  
  if(!requireNamespace("EML", quietly = TRUE)){
    stop("The EML action requires the 'EML' package")
  }
  
  #options
  taxonomySubject <- if(!is.null(options$subject_taxonomy)) options$subject_taxonomy else "taxonomy"
  
  #init dataset
  dataset <- EML::eml$dataset()
  
  #get doi
  the_doi <- entity$identifiers[["doi"]]
  if(is.null(the_doi)) the_doi <- entity$identifiers[["conceptdoi_to_save"]]
  if(is.null(the_doi)) the_doi <- entity$identifiers[["doi_to_save"]]
  
  #identifiers
  dataset$id <- entity$identifiers[["id"]]
  if(!is.null(the_doi)){
    dataset$alternateIdentifier <- list(directory = "https://orcid.org", userId = the_doi)
  }
  
  #short name
  dataset$shortName <- entity$title
  
  #title
  dataset$title <- entity$title
  
  #contact IDs already added
  contact_ids <- list()
  
  #helper functions
  #contactToEML function
  contactToEML <- function(contact){
    person <- list(id = contact$id)
    if(!is.na(contact$firstName)) person$individualName$givenName <- contact$firstName
    if(!is.na(contact$lastName)) person$individualName$surName <- contact$lastName
    if(!is.na(contact$organizationName)) person$organizationName <- contact$organizationName
    if(!is.na(contact$positionName)) person$positionName <- contact$positionName
    #address
    address <- list()
    if(!is.na(contact$postalAddress)) address$deliveryPoint <- contact$postalAddress
    if(!is.na(contact$postalCode)) address$postalCode<- contact$postalCode
    if(!is.na(contact$city)) address$city <- contact$city
    if(!is.na(contact$country)) address$country <- contact$country
    if(length(address)>0) person$address <- address
    #contact resources
    if(!is.na(contact$voice)) person$phone <- contact$voice
    if(!is.na(contact$email)) person$lectronicMailAddress <- contact$email
    if(!is.na(contact$websiteUrl)) person$onlineUrl <- contact$websiteUrl
    if(length(contact$identifiers)>0){
      for(id in contact$identifiers){
        switch(id$key,
          "orcid" = {
            person$id <- sprintf("https://orcid.org/%s", id$value)
            person$userId <- list(directory = "https://orcid.org", userId = person$id)
          }       
        )
      }
    }
    return(person)
  }
  
  #contactsToEMLRole
  contactsToEML <- function(contacts){
    persons <- lapply(contacts, contactToEML)
    out_persons <- lapply(persons, function(person){
      if(person$id %in% contact_ids){
        list(references = person$id)
      }else{
        contact_ids <<- c(contact_ids, person$id)
        person
      }
    })
    return(out_persons)
  }

  #creator
  creators <- entity$getContacts()[sapply(entity$getContacts(), function(x){x$role=="owner"})]
  dataset$creator <- contactsToEML(creators)
  
  #metadataProvider
  md_providers <- entity$getContacts()[sapply(entity$getContacts(), function(x){x$role=="metadata"})]
  if(length(md_providers)>0) dataset$metadataProvider <- contactsToEML(md_providers)
  
  #pubDate
  dataset$pubDate <- Sys.time()
  pub_dates <- entity$dates[sapply(entity$dates, function(x){x$key == "publication"})]
  if(length(pub_dates)>0) dataset$pubDate <- pub_dates[[1]]$value
  
  #language
  dataset$language <- entity$language
  
  #abstract
  if(!is.null(entity$descriptions$abstract)) dataset$abstract <- list(
    section = list(),
    para = entity$descriptions$abstract
  )
  
  #keywords
  if(length(entity$subjects)>0){
    dataset$keywordSet <- lapply(entity$subjects, function(subject){
      list(
        keywordThesaurus = subject$name,
        keyword = lapply(subject$keywords, function(keyword){keyword$name})
      )
    })
  }
  
  #additionalInfo
  if(!is.null(entity$descriptions$info)) dataset$additionalInfo <- list(para = entity$descriptions$info)
  
  #TODO intellectualRights
  #TODO license
  #TODO distribution
  
  #coverage
  #geo
  if(!is.null(entity$spatial_bbox)){
    if(is.null(dataset$coverage)) dataset$coverage <- list()
    dataset$coverage$geographicCoverage <- list(
      boundingCoordinates = list(
        westBoundingCoordinate = entity$spatial_bbox$xmin,
        eastBoundingCoordinate = entity$spatial_bbox$xmax,
        southBoundingCoordinate = entity$spatial_bbox$ymin,
        northBoundingCoordinate = entity$spatial_bbox$ymax
      )
    )
  }
  #temporal
  if(!is.null(entity$temporal_extent)){
    if(is.null(dataset$coverage)) dataset$coverage <- list()
    dataset$coverage$temporalCoverage <- list(
      rangeOfDates = list(
        beginDate = list(calendarDate = as.character(as.Date(entity$temporal_extent$start))),
        endDate = list(calendarDate = as.character(as.Date(entity$temporal_extent$end)))
      )
    )
  }
  #taxonomic
  taxo <- entity$subjects[sapply(entity$subjects, function(x){x$name == taxonomySubject})]
  if(length(taxo)>0){
    taxo <- taxo[[1]]
    if(is.null(dataset$coverage)) dataset$coverage <- list()
    dataset$coverage$taxonomicCoverage = list(
      taxonomicClassification = lapply(taxo$keywords, function(keyword){
        kwd_name = keyword$name
        kwd_parts = unlist(strsplit(kwd_name, " "))
        taxocl <- list(
          taxonRankName = "Genus",
          taxonRankValue = kwd_parts[1]
        )
        if(length(kwd_parts)>1){
          str <- paste(kwd_parts[2:length(kwd_parts)], collapse = " ")
          taxocl$taxonomicClassification = list(
            taxonRankName = "Species",
            taxonRankValue = str
          )
        }
        return(taxocl)
      })
    )
  }
  
  #purpose
  if(!is.null(entity$descriptions$purpose)) dataset$purpose <- list(para = entity$descriptions$purpose)
  
  #contact
  pocs <- entity$getContacts()[sapply(entity$getContacts(), function(x){x$role=="pointOfContact"})]
  if(length(pocs)>0) dataset$contact <- contactsToEML(pocs)
  
  #publisher
  publishers <- entity$getContacts()[sapply(entity$getContacts(), function(x){x$role=="publisher"})]
  if(length(publishers)>0) dataset$publisher <- contactsToEML(publishers)
  
  #methods
  if(!is.null(entity$provenance)){
    if(length(entity$provenance$processes)>0){
      dataset$methodStep <- lapply(entity$provenance$processes, function(process){
        list(description = paste0(process$rationale, ": ", process$description))
      })
    }
  }
  
  #project
  project <- NULL
  if(!is.null(config$profile$project)) project <- config$profile$project
  if(!is.null(entity$descriptions$project)) project <- entity$descriptions$project
  
  #TODO spatialRaster
  #TODO spatialVector
  
  #wrapper
  eml_md <- EML::eml$eml(
    packageId = uuid::UUIDgenerate(),
    system = "uuid",
    dataset = dataset,
    lang = entity$language
  )
  
  #eml validation
  eml_valid <- EML::eml_validate(eml_md)
  if(eml_valid){
    config$logger.info("EML metadata produced is valid!")
  }else{
    eml_errors <- attr(eml_valid, "errors")
    config$logger.warn(sprintf("EML metadata has %s validation issues:", length(eml_errors)))
    for(eml_error in eml_errors){
      config$logger.warn(eml_error)
    }
  }
  
  #eml writing
  EML::write_eml(eml_md, file.path(getwd(), "metadata", paste0(entity$identifiers[["id"]], "_EML.xml")))
  
}