function(action, entity, config){
  
  if(!requireNamespace("atom4R", quietly = TRUE)){
    stop("The 'atom4R-dataverse-deposit-record' action requires the 'atom4R' package")
  }
  
  #global options
  skipFileDownload <- if(!is.null(config$profile$options$skipFileDownload)) config$profile$options$skipFileDownload else FALSE
  
  #options
  depositWithFiles <- action$getOption("depositWithFiles")
  publish <- action$getOption("publish")
  deleteOldFiles <- action$getOption("deleteOldFiles")
  update_metadata <- action$getOption("update_metadata")
  update_files <- action$getOption("update_files")
  
  #Sword API
  SWORD <- config$software$output$sword_for_dataverse
  SWORD_CONFIG <- config$software$output$sword_for_dataverse_config
  if(is.null(SWORD)){
    errMsg <- "This action requires a Dataverse SWORD software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  target_dataverse_id <- NULL
  if(is.null(SWORD_CONFIG$properties$dataverse)){
    errMsg <- "Missing target 'dataverse' in config. Search for entity-based 'dataverse'..."
    config$logger.warn(errMsg)
    if(is.null(entity$data$dataverse)){
      errMsg <- "No entity-based 'dataverse' defined"
      config$logger.warn(errMsg)
      errMsg <- "A 'dataverse' id is required to trigger the Dataverse SWORD deposit action"
      config$logger.error(errMsg)
      stop(errMsg)
    }else{
      target_dataverse_id <- entity$data$dataverse
    }
  }else{
    target_dataverse_id <- SWORD_CONFIG$properties$dataverse
  }
  
  #grab dataverse
  dataverse_exists <- any(sapply(SWORD$listCollections(), function(x){endsWith(x$href, target_dataverse_id)}))
  if(dataverse_exists){
    config$logger.info(sprintf("Successfully fetched dataverse for id '%s':", target_dataverse_id))
  }else{
    errMsg <- sprintf("Error while fetching dataverse for id '%s'. Unknown dataverse", target_dataverse_id)
    stop(errMsg)
  }
  
  #create DC entry
  dcentry <- atom4R::DCEntry$new()
  dcentry$setId(entity$identifiers[["id"]])
  #fill dc entry
  ##date
  creationDate <- entity$dates[sapply(entity$dates, function(x){x$key == "creation"})]
  if(length(creationDate)>0){
    dcentry$addDCDate(creationDate[[1]]$value)
  }
  ##title
  dcentry$addDCTitle(entity$titles[["title"]])
  ##type
  dctype <- entity$types[["generic"]]
  dctype_allowed <- atom4R::getDCMIVocabulary(id = "http://purl.org/dc/dcmitype/")$fetch()$label
  dctype_idx <- which(tolower(dctype_allowed) == tolower(dctype))
  dctype_dataverse <- dctype_allowed[dctype_idx]
  if(length(dctype_dataverse)==0) dctype_dataverse <- "Dataset"
  dcentry$addDCType(dctype_dataverse)
  ##subjects
  subjects <- entity$subjects
  if(length(subjects)>0) subjects <- subjects[sapply(subjects, function(x){return(x$key != "topic")})]
  if(length(subjects)>0) for(subject in subjects){
    for(kwd in subject$keywords){
      dcentry$addDCSubject(kwd$name)
    }
  }
  ##abstract
  dcentry$addDCDescription(entity$descriptions[["abstract"]])
  ##Creators
  owners <- entity$contacts[sapply(entity$contacts, function(x){x$role == "owner"})]
  if(length(owners)==0) owners <- list(entity$contacts[[1]])
  for(owner_entity in owners){
    creator <- NULL
    if(!is.na(owner_entity$lastName) && !is.na(owner_entity$firstName)){
      creator <-  sprintf("%s, %s", owner_entity$lastName, owner_entity$firstName)
    }else{
      creator <- owner_entity$organizationName
    }
    dc_creator <- atom4R::DCCreator$new(value = creator)
    dc_creator$attrs[["affiliation"]] <- owner_entity$organizationName
    dcentry$addDCCreator(dc_creator)
  }
  ##publisher
  publishers <- entity$contacts[sapply(entity$contacts, function(x){x$role == "publisher"})]
  if(length(publishers)==0) publishers <- list(entity$contacts[[1]])
  for(publisher_entity in publishers){
    dc_publisher <- NULL
    if(!is.na(publisher_entity$lastName) && !is.na(publisher_entity$firstName)){
      dc_publisher <- sprintf("%s, %s", publisher_entity$lastName, publisher_entity$firstName)
    }else{
      dc_publisher <- publisher_entity$organizationName
    }
    dcentry$addDCPublisher(dc_publisher)
  }
  ##contribs
  contribs <- entity$contacts[sapply(entity$contacts, function(x){!x$role %in% c("owner", "publisher")})]
  if(length(contribs)>0){
    for(contrib_entity in contribs){
      contrib <- NULL
      if(!is.na(contrib_entity$lastName) && !is.na(contrib_entity$firstName)){
        contrib <- sprintf("%s, %s", contrib_entity$lastName, contrib_entity$firstName)
      }else{
        contrib <- contrib_entity$organizationName
      }
      dc_contrib <- atom4R::DCContributor$new(value = contrib)
      dc_contrib$attrs[["type"]] <- contrib_entity$role #TODO mapping with controlled terms from Dataverse
      dcentry$addDCContributor(dc_contrib)
    }
  }
  
  ##relations
  if(length(entity$relations)>0){
    for(http_relation in entity$relations){
      dc_relation <- sprintf("%s: %s%s", 
                             http_relation$name, 
                             ifelse(!is.null(http_relation$description), paste0(http_relation$description, " - "), ""),
                             http_relation$link)
      dcentry$addDCRelation(dc_relation)
    }
  }
  ##coverage
  if(!is.null(entity$spatial_extent)){
    cov <- sf::st_as_text(entity$spatial_extent)
    if(!is.null(entity$srid)) cov <- paste0("SRID=", entity$srid, ";", cov)
    dcentry$addDCCoverage(cov)
  }
  
  ##sources
  #TODO any sources?
  #rights (license/right)
  if(length(entity$rights)>0){
    #licenses
    licenses <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "license"})]
    if(length(licenses)>0){
      #check if CCO is the license then we add it as DC License
      hasCCOLicense <- FALSE
      license <- licenses[[1]]$value
      if(license == "CCO"){
        hasCCOLicense <- TRUE
        dcentry$addDCLicense(license)
      }
      #if no CCO license we add DC rights
      if(!hasCCOLicense) {
        dcentry$addDCRights(license)
      }
    }
  }
  
  #save Dublin Core XML to metadata folder
  dcentry$save(file.path(getwd(), "metadata", paste0(entity$getEntityJobDirname(), "_DC.xml")))
  
  #action (create/update) on dataverse
  doi <- entity$identifiers[["doi"]]
  action <- ifelse(is.null(doi),"CREATE","UPDATE")
  update <- action == "UPDATE"
  out <- switch(action,
                "CREATE" = {
                  rec <- SWORD$createDataverseRecord(target_dataverse_id, dcentry)
                  doi <- unlist(strsplit(rec$id, "doi:"))[2] #we need the reserved doi to add files
                  rec
                },
                "UPDATE" = {
                  if(update_metadata){
                    config$logger.info(sprintf("Updating record for doi '%s'", doi))
                    SWORD$updateDataverseRecord(target_dataverse_id, dcentry, paste0("doi:", doi))
                  }else{
                    config$logger.info(sprintf("Skip updating record for doi '%s' (option 'update_metadata' is FALSE)", doi))
                    SWORD$getDataverseRecord(paste0("doi:", doi))
                  }
                }
  )
  
  #delete/add files
  if(depositWithFiles & (!update | (update & update_files))){
    if(deleteOldFiles & !skipFileDownload){
      config$logger.info("Dataverse: deleting old files...")
      deleted <- SWORD$deleteFilesFromDataverseRecord(paste0("doi:", doi))
      config$logger.info("Dataverse: files deletion status:")
      config$logger.info(deleted)
    }
    config$logger.info("Dataverse: adding files...")
    #upload data files, if any
    data_files <- list.files(file.path(getwd(),"data"), full.names = T)
    if(length(data_files)>0){
      data_files <- data_files[regexpr(entity$identifiers[["id"]],data_files)>0]
      if(length(data_files)>0) data_files <- data_files[!endsWith(data_files, ".rds")]
      if(length(data_files)>0){
        if(entity$data$upload){
          config$logger.info("Dataverse: uploading data files...")
          uploaded <- SWORD$addFilesToDataverseRecord(paste0("doi:", doi), files = data_files)
        }else{
          config$logger.warn("Dataverse: upload:false, skipping data files upload!")
        }
      }
    }
    #upload metadata files, if any
    metadata_files <- list.files(file.path(getwd(),"metadata"), full.names = T)
    if(length(metadata_files)>0){
      metadata_files <- metadata_files[regexpr(entity$identifiers[["id"]],metadata_files)>0]
      if(length(metadata_files)>0) metadata_files <- metadata_files[!endsWith(metadata_files, ".rds")]
      if(length(metadata_files)>0){
        config$logger.info("Dataverse: uploading metadata files...")
        uploaded <- SWORD$addFilesToDataverseRecord(paste0("doi:", doi), files = metadata_files)
      }
    }
  }else{
    config$logger.info("Skipping update of Dataverse record files (option 'update_files' and/or 'depositWithFiles FALSE)")
  }
  
  #publish record?
  if(publish){
    #2d verification for publish action, need to have the DOI specified in the entity table
    if(is.null(entity$identifiers[["doi"]])){
      config$logger.warn("No DOI specified in entity. Dataverse 'publish' action ignored!")
      publish <- FALSE
    }
    #3rd verification for publish action, need to check that DOI match the one prereserved
    if(!is.null(entity$identifiers[["doi"]])){
      if(doi != entity$identifiers[["doi"]]){ 
        config$logger.warn(sprintf("DOI specified (%s) in entity doesn't match Dataverse record Concept DOI (%s). Zenodo 'publish' action ignored!", 
                                   entity$identifiers[["doi"]], doi))
        publish <- FALSE
      }
    }
    
    #publish
    if(publish){
      config$logger.warn(sprintf("Dataverse: publishing record for DOI '%s'", doi))
      SWORD$publishDataverseRecord(paste0("doi:", doi))
    }
  }
  
  #output table of DOIs
  if(is(out, "AtomEntry") | is(out, "AtomFeed")){
    infoMsg <- switch(action,
                      "CREATE" = sprintf("Successfully created Dataverse dataset with id '%s'", 
                                         entity$identifiers[["id"]]),
                      "UPDATE" = sprintf("Successfully updated Dataverse dataset with id '%s' (doi: %s)", 
                                         entity$identifiers[["id"]], doi)
    )
    config$logger.info(infoMsg)
    
    #get the DOI assigned by Dataverse
    doi_to_save <- unlist(strsplit(out$id, "doi:"))[2]
    config$logger.info(sprintf("Setting DOI '%s' (inherited from Dataverse) to save and export for record", doi_to_save))
    for(i in 1:length(config$metadata$content$entities)){
      ent <- config$metadata$content$entities[[i]]
      if(ent$identifiers[["id"]]==entity$identifiers[["id"]]){
        config$metadata$content$entities[[i]]$identifiers[["dataverse_doi_to_save"]] <- doi_to_save
        config$metadata$content$entities[[i]]$identifiers[["dataverse_conceptdoi_to_save"]] <- doi_to_save
        config$metadata$content$entities[[i]]$setStatus("dataverse", ifelse(publish, "published", "draft"))
        break;
      }
    }
    entity$identifiers[["doi"]] <- doi_to_save
    entity$identifiers[["dataverse_doi_to_save"]] <- doi_to_save
    entity$identifiers[["dataverse_conceptdoi_to_save"]] <- doi_to_save
    entity$setStatus("dataverse", ifelse(publish, "published", "draft"))
  }
}