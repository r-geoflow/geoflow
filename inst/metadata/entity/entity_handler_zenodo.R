#handle_entities_zenodo
handle_entities_zenodo <- function(handler, source, config, handle = TRUE){
  
  if(!requireNamespace("zen4R", quietly = TRUE)){
    stop("The Dataverse handler requires the 'zen4R' package")
  }
  
  ZENODO <- config$software$input$zenodo
  if(is.null(ZENODO)){
    stop("There is no 'zenodo' input software configured to handle entities from Zenodo")
  }
  ZENODO_CONFIG <- config$software$input$zenodo_config
  
  resource_type <- handler$getOption("resource_type") #for this handler default is "deposit"
  source_type <- handler$getOption("source_type") #for this handler default is "query"
  
  #fetch results depending on resource and source types
  config$logger.info(sprintf("Zenodo entity handler: resource type '%s'", resource_type))
  config$logger.info(sprintf("Zenodo entity handler: source type '%s'", source_type))
  results <- switch(resource_type,
    "deposit" = {
      switch(source_type,
        "query" = {
          ZENODO$getDepositions(q = source)
        },
        "doi" = {
          recs = lapply(source, function(doi){
            rec = ZENODO$getDepositionByDOI(doi)
            if(is.null(rec)) rec = ZENODO$getDepositionByConceptDOI(doi)
            return(rec)
          })
          recs = recs[!sapply(recs, is.null)]
          recs
        },{
          config$logger.warn(sprintf("Unknown source type '%s' in Zenodo entity handler", source_type))
          list()
        }
      )
     },
    "record" = {
     switch(source_type,
      "query" = {
         ZENODO$getRecords(q = source) 
       },
       "doi" = {
         recs = lapply(source, function(doi){
           rec = ZENODO$getRecordByDOI(doi)
           if(is.null(rec)) rec = ZENODO$getRecordByConceptDOI(doi)
           return(rec)
         })
         recs = recs[!sapply(recs, is.null)]
         recs
       },{
         config$logger.warn(sprintf("Unknown source type '%s' in Zenodo entity handler", source_type))
         list()
       }
     )  
    },{
      config$logger.warn(sprintf("Unknown resource type '%s' in Zenodo entity handler. Valid values ['deposit','record']", resource_type))
      list()
    }
  )
  if(length(results)==1) if(is(results, "ZenodoRecord")) results = list(results)
  if(length(results)==0) return(list())
  
  entities <- lapply(1:length(results), function(i){
    result <- results[[i]]
    config$logger.info(sprintf("Creating entity (%s out of %s) from Zenodo deposit/record with DOI '%s'", i, length(results), result$metadata$doi))
    
    #create entity
    entity <- geoflow::geoflow_entity$new()
    #entity Identifier
    entity$setIdentifier("doi", result$metadata$doi)
    #semantic identifier
    identifiers = result$metadata$related_identifiers[sapply(result$metadata$related_identifiers, function(x){x$scheme == "urn" && x$relation == "isIdenticalTo"})]
    if(length(identifiers)>0){
      identifier = identifiers[[1]]$identifier
      entity$setIdentifier("id", substr(identifier, 5,nchar(identifier)))
    }else{
      entity$setIdentifier("id", gsub("/", "_", result$metadata$doi))
    }
    
    #entity Date
    if(!is.null(result$created)) entity$addDate(dateType = "creation", date = str_to_posix(result$created))
    if(!is.null(result$modified)) entity$addDate(dateType = "lastUpdate", date = str_to_posix(result$updated))
    if(!is.null(result$metadata$publication_date)) entity$addDate(dateType = "publication",   date = str_to_posix(result$metadata$publication_date))
    if(isTRUE(result$access$embargo$active) & !is.null(result$access$embargo$until)) entity$addDate(dateType = "embargo", date = str_to_posix(result$access$embargo$until))
    
    #entity common metadata
    #entity Type
    entity$setType(key = "generic", result$metadata$resource_type$id)
    #entity Title
    entity$setTitle("title", result$metadata$title)
    #entity Description
    entity$setDescription("abstract", result$metadata$description)
    #entity subjects (one single theme subject feed with keywords - freetext - and zenodo "subjects")
    subj <- geoflow::geoflow_subject$new()
    subj$setKey("theme")
    subj$setName("General")
    for(sbj in result$metadata$subjects){
      subj$addKeyword(sbj$subject)
    }
    entity$addSubject(subj)
  
    #entity Contacts/roles
    #creator(s)
    creators <- result$metadata$creators
    if(length(creators)>0) for(creator in creators){
      creator_poo = creator$person_or_org
      creatorNames <- unlist(strsplit(creator_poo$name, ", "))
      creator_c <- geoflow::geoflow_contact$new()
      creator_c$setFirstName(creatorNames[2])
      creator_c$setLastName(creatorNames[1])
      if(!is.null(creator_poo$affiliations)) creator_c$setOrganizationName(creator_poo$affiliations[[1]]$id)
      ids = creator_poo$identifiers
      names(ids) = sapply(ids, function(x){x$scheme})
      if(!is.null(ids$orcid)) creator_c$setIdentifier("orcid", ids$orcid$identifier)
      if(!is.null(ids$gnd)) creator_c$setIdentifier("gnd", ids$gnd$identifier)
      creator_c$setRole("creator")
      entity$addContact(creator_c)
    }
    #contributor(s)
    contribs <- result$metadata$contributors
    for(contrib in contribs){
      contrib_poo = contrib$person_or_org
      contribNames <- unlist(strsplit(contrib_poo$name, ", "))
      contrib_c <- geoflow::geoflow_contact$new()
      contrib_c$setFirstName(contribNames[2])
      contrib_c$setLastName(contribNames[1])
      if(!is.null(contrib_poo$affiliations)) contrib_c$setOrganizationName(contrib_poo$affiliations[[1]]$id)
      ids = contrib_poo$identifiers
      names(ids) = sapply(ids, function(x){x$scheme})
      if(!is.null(ids$orcid)) contrib_c$setIdentifier("orcid", ids$orcid$identifier)
      if(!is.null(ids$gnd)) contrib_c$setIdentifier("gnd", ids$gnd$identifier)
      contrib_c$setRole("contributor")
      entity$addContact(contrib_c)
    }
    #relations
    #references
    if(length(result$metadata$references)>0){
      for(reference in result$metadata$references){
        ref_rel = geoflow_relation$new()
        ref_rel$setKey("ref")
        ref_rel$setName(reference$reference)
        entity$addRelation(ref_rel)
      }
    }
    #grants
    if(length(result$metadata$grants)>0){
      for(grant in result$metadata$grants){
        grant_rel = geoflow_relation$new()
        grant_rel$setKey("grant")
        grant_rel$setName(grant$id)
        entity$addRelation(grant_rel)
      }
    }
    
    #license / terms of use
    rights <- geoflow::geoflow_right$new()
    if(!is.null(result$metadata$license)) {
      rights$setKey("license")
      rights$setValues(result$metadata$rights[[1]]$id)
      entity$addRight(rights)
    }
    if(result$access$files != "public"){
      access_rights = geoflow::geoflow_right$new()
      access_rights$setKey("accessRight")
      access_rights$setValues(result$access$files)
      entity$addRight(access_rights)
    }
    
    #entity SpatialCoverage
    #entity TemporalCoverage
    
    #data
    if(length(result$files)>0){
      data <- geoflow::geoflow_data$new()
      data$setAccess("zenodo")
      data$source <- lapply(1:length(result$files), function(i){
        z_file <- result$files[[i]]
        z_filename <- z_file$filename
        attr(z_filename, "uri") <- result$metadata$doi
        return(z_filename)
      })
      data$upload <- TRUE
      entity$setData(data)
    }
    return(entity)
  })
  
  
  return(entities)
}
