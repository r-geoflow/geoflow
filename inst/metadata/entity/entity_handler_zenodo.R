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
  
  results <- ZENODO$getDepositions(q = source)
  if(length(results)==0) return(list())
  
  entities <- lapply(1:length(results), function(i){
    result <- results[[i]]
    config$logger.info(sprintf("Creating entity (%s out of %s) from Zenodo deposit/record with DOI '%s'", i, length(results), result$doi))
    
    #create entity
    entity <- geoflow::geoflow_entity$new()
    #entity Identifier
    entity$setIdentifier("doi", result$doi)
    #semantic identifier
    identifiers = result$metadata$related_identifiers[sapply(result$metadata$related_identifiers, function(x){x$scheme == "urn" && x$relation == "isIdenticalTo"})]
    if(length(identifiers)>0){
      identifier = identifiers[[1]]$identifier
      entity$setIdentifier("id", substr(identifier, 5,nchar(identifier)))
    }
    
    #entity Date
    if(!is.null(result$created)) entity$addDate(dateType = "creation", date = str_to_posix(result$created))
    if(!is.null(result$modified)) entity$addDate(dateType = "lastUpdate", date = str_to_posix(result$modified))
    if(!is.null(result$metadata$publication_date)) entity$addDate(dateType = "publication",   date = str_to_posix(result$metadata$publication_date))
    if(!is.null(result$metadata$embargo_date)) entity$addDate(dateType = "embargo", date = str_to_posix(result$metadata$embargo_date))
    
    #entity common metadata
    #entity Type
    entity$setType(key = "generic", result$metadata$upload_type)
    #entity Title
    entity$setTitle("title", result$metadata$title)
    #entity Description
    entity$setDescription("abstract", result$metadata$description)
    #entity subjects (one single theme subject feed with keywords - freetext - and zenodo "subjects")
    subj <- geoflow::geoflow_subject$new()
    subj$setKey("theme")
    subj$setName("General")
    for(kwd in result$metadata$keywords){
      subj$addKeyword(kwd)
    }
    for(sbj in result$metadata$subjects){
      subj$addKeyword(sbj$term, uri = sbj$identifier)
    }
    entity$addSubject(subj)
  
    #entity Contacts/roles
    #creator(s)
    creators <- result$metadata$creators
    for(creator in creators){
      creatorNames <- unlist(strsplit(creator$name, ", "))
      creator_c <- geoflow::geoflow_contact$new()
      creator_c$setFirstName(creatorNames[2])
      creator_c$setLastName(creatorNames[1])
      if(!is.null(creator$affiliation)) creator_c$setOrganizationName(creator$affiliation)
      if(!is.null(creator$orcid)) creator_c$setIdentifier("orcid", creator$orcid)
      if(!is.null(creator$gnd)) creator_c$setIdentifier("gnd", creator$gnd)
      creator_c$setRole("creator")
      entity$addContact(creator_c)
    }
    #contributor(s)
    contribs <- result$metadata$contributors
    for(contrib in contribs){
      contribNames <- unlist(strsplit(contrib$name, ", "))
      contrib_c <- geoflow::geoflow_contact$new()
      contrib_c$setFirstName(contribNames[2])
      contrib_c$setLastName(contribNames[1])
      if(!is.null(contrib$affiliation)) contrib_c$setOrganizationName(contrib$affiliation)
      if(!is.null(contrib$orcid)) contrib_c$setIdentifier("orcid", contrib$orcid)
      if(!is.null(contrib$gnd)) contrib_c$setIdentifier("gnd", contrib$gnd)
      contrib_c$setRole("contributor")
      entity$addContact(contrib_c)
    }
    #relations
    #references
    if(length(result$metadata$references)>0){
      for(reference in result$metadata$references){
        ref_rel = geoflow_relation$new()
        ref_rel$setKey("ref")
        ref_rel$setName(reference)
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
      rights$setValues(result$metadata$license)
      entity$addRight(rights)
    }
    if(result$metadata$access_right != "open"){
      access_rights = geoflow::geoflow_right$new()
      access_rights$setKey("accessRight")
      access_rights$setValues(result$metadata$access_right)
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
        attr(z_filename, "uri") <- result$doi
        return(z_filename)
      })
      data$upload <- TRUE
      entity$setData(data)
    }
    return(entity)
  })
  
  
  return(entities)
}
