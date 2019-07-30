zen4R_deposit_record <- function(entity, config, options){
  
  if(!require("zen4R")){
    stop("This action requires the 'zen4R' package")
  }
  
  ZENODO <- config$software$output$zenodo
  
  if(is.null(ZENODO)){
    errMsg <- "This action requires the Zenodo API to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #global options
  skipFileDownload <- if(!is.null(config$options$skipFileDownload)) config$options$skipFileDownload else FALSE
  
  #options
  depositWithFiles <- if(!is.null(options$depositWithFiles)) options$depositWithFiles else FALSE
  publish <- if(!is.null(options$publish) & depositWithFiles) options$publish else FALSE
  deleteOldFiles <- if(!is.null(options$deleteOldFiles)) options$deleteOldFiles else TRUE
  update_metadata <- if(!is.null(options$update_metadata)) options$update_metadata else TRUE
  update_files <- if(!is.null(options$update_files)) options$update_files else TRUE
  communities <- if(!is.null(options$community)) options$community else NULL
  
  #create empty record
  #how to deal with existing records / new versions
  #this approach that the Zenodo record has a related identifier as URN
  #e.g. urn:my-metadata-identifier
  zenodo_metadata <- NULL
  deposits <- ZENODO$getDepositions(q = entity$identifiers[["id"]])
  if(length(deposits)>0){
    invisible(lapply(deposits, function(deposit){
     related_identifiers <- deposit$metadata$related_identifiers
     if(!is.null(related_identifiers)){
       for(related_identifier in related_identifiers){
         if(startsWith(related_identifier$identifier,"urn")){
           related_id <- unlist(strsplit(related_identifier$identifier, "urn:"))[2]
           if(related_id == entity$identifiers[["id"]] &
              related_identifier$relation == "isIdenticalTo"){
             zenodo_metadata <<- deposit
             break
           }
         }
       }
     }
    }))
  }
  
  update <- FALSE
  if(is.null(zenodo_metadata)){
    config$logger.info(sprintf("Zenodo: No existing Zenodo record with related identifier '%s'", paste0("urn:",entity$identifiers[["id"]])))
    config$logger.info("Zenodo: creating a new deposit empty record")
    zenodo_metadata <- ZENODO$createEmptyRecord()
    zenodo_metadata$addRelatedIdentifier("isIdenticalTo", paste("urn", entity$identifiers[["id"]], sep=":"))
  }else{
    config$logger.info(sprintf("Zenodo: Existing record with related identifier '%s'", paste0("urn:",entity$identifiers[["id"]])))
    update <- TRUE
  }
  
  doi <- zenodo_metadata$metadata$prereserve_doi$doi
  #if entity already comes with a DOI, we set it (this might be a preset DOI from Zenodo or elsewhere)
  if(!is.null(entity$identifiers[["doi"]])){
    doi <- entity$identifiers[["doi"]]
  }
  #if entity comes with a foreign DOI (not assigned by Zenodo)
  #we set the DOI (which set prereserve_doi to FALSE)
  if(regexpr("zenodo", doi)<0){
    config$logger.info("Zenodo: Existing foreign DOI (not assigned by Zenodo). Setting foreign DOI and prereserve_doi to 'FALSE'")
    zenodo_metadata$setDOI(doi)
  }
  
  if(!update | (update & update_metadata)){
    if(update){
      config$logger.info("Updating Zenodo record metadata properties")
    }else{
      config$logger.info("Setting Zenodo record metadata properties")
    }
    #basic record description
    zenodo_metadata$setTitle(entity$title)
    zenodo_metadata$setDescription(entity$descriptions[["abstract"]])
    #date
    zenodo_metadata$setPublicationDate(entity$date)
    #upload type
    #TODO think on how to map upload types between Dublin core, ISO/OGC metadata, Zenodo  
    zenodo_metadata$setUploadType(entity$type)
    #contacts
    #TODO think if correct handle all contacts (whatever roles) as creators (author/co-authors)
    contact_added <- list()
    zenodo_metadata$metadata$creators <- list()
    for(contact in entity$contacts){
      
      #manage orcid?
      orcid <- NULL
      contact_ids <- contact$identifiers
      if(any(sapply(contact_ids, function(x){x$key=="orcid"}))){
        contact_ids <- contact_ids[sapply(contact_ids, function(x){x$key=="orcid"})]
        if(length(contact_ids)>0) orcid <- contact_ids[[1]]$value
      }
      #add/update creators
      if(!(contact$id %in% contact_added)){
        zenodo_metadata$addCreator(
          firstname = contact$firstName, 
          lastname = contact$lastName, 
          affiliation = contact$organizationName,
          orcid = orcid
        )
        contact_added <- c(contact_added, contact$id)
      }
    }
    
    #TODO myrec$setLicense
    #TODO myrec$setAccessRight
    
    #communities
    if(!is.null(communities)){
      zenodo_metadata$metadata$communities <- list()
      for(community in communities) zenodo_metadata$addCommunity(community)
    }
  }else{
    config$logger.info("Skipping update of Zenodo metadata properties (option 'update_metadata' FALSE)")
  }
  
  #file uploads
  if(depositWithFiles & (!update | (update & update_files))){
    if(deleteOldFiles & !skipFileDownload){
      config$logger.info("Zenodo: deleting old files...")
      zen_files <- ZENODO$getFiles(zenodo_metadata$id)
      if(length(zen_files)>0){
        for(zen_file in zen_files){
          ZENODO$deleteFile(zenodo_metadata$id,zen_file$id)
        }
      }
    }
    config$logger.info("Zenodo: uploading files...")
    #upload data files, if any
    data_files <- list.files(file.path(getwd(),"data"))
    if(length(data_files)>0){
      data_files <- data_files[regexpr(entity$identifiers[["id"]],data_files)>0]
      if(length(data_files)>0) data_files <- data_files[!endsWith(data_files, ".rds")]
      if(length(data_files)>0){
        config$logger.info("Zenodo: uploading data files...")
        for(data_file in data_files){
          config$logger.info(sprintf("Zenodo: uploading data file '%s'", data_file))
          ZENODO$uploadFile(file.path(getwd(), "data", data_file), zenodo_metadata$id)
        }
      }
    }
    #upload metadata files, if any
    metadata_files <- list.files(file.path(getwd(),"metadata"))
    if(length(metadata_files)>0){
      metadata_files <- metadata_files[regexpr(entity$identifiers[["id"]],metadata_files)>0]
      if(length(metadata_files)>0) metadata_files <- metadata_files[!endsWith(metadata_files, ".rds")]
      if(length(metadata_files)>0){
        config$logger.info("Zenodo: uploading metadata files...")
        for(metadata_file in metadata_files){
          config$logger.info(sprintf("Zenodo: uploading metadata file '%s'", metadata_file))
          ZENODO$uploadFile(file.path(getwd(), "metadata",metadata_file), zenodo_metadata$id)
        }
      }
    }
  }else{
    config$logger.info("Skipping update of Zenodo metadata properties (option 'update_files' and/or 'depositWithFiles FALSE)")
  }

  #deposit (and publish, if specified in options)
  if(publish){
    #2d verification for publish action, need to have the DOI specified in the entity table
    if(is.null(entity$identifiers[["doi"]])){
      config$logger.warn("No DOI specified in entity. Zenodo 'publish' action ignored!")
      publish <- FALSE
    }
    #3rd verification for publish action, need to check that DOI match the one prereserved
    if(!is.null(entity$identifiers[["doi"]])){
      if(regexpr("zenodo", doi)>0) if(doi != zenodo_metadata$getConceptDOI()){ 
        config$logger.warn(sprintf("DOI specified (%s) in entity doesn't match Zenodo record Concept DOI (%s). Zenodo 'publish' action ignored!", 
                                   doi, zenodo_metadata$getConceptDOI()))
        publish <- FALSE
      }
    }
  }
  config$logger.info(sprintf("Deposit record with id '%s' - publish = %s", zenodo_metadata$id, tolower(as.character(publish))))
  out <- ZENODO$depositRecord(zenodo_metadata, publish = publish)
  if(!is(out,"ZenodoRecord")){
    errMsg <- sprintf("Zenodo: %s", out$errors[[1]]$message)
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #we set the (prereserved) doi to the entity in question
  config$logger.info(sprintf("Setting DOI '%s' to save and export for record",zenodo_metadata$metadata$prereserve_doi$doi))
  for(i in 1:length(config$metadata$content$entities)){
    ent <- config$metadata$content$entities[[i]]
    if(ent$identifiers[["id"]]==entity$identifiers[["id"]]){
      if(regexpr("zenodo", doi)>0){
        config$metadata$content$entities[[i]]$identifiers[["doi_to_save"]] <- zenodo_metadata$metadata$prereserve_doi$doi
        config$metadata$content$entities[[i]]$identifiers[["conceptdoi_to_save"]] <- zenodo_metadata$getConceptDOI()
        config$metadata$content$entities[[i]]$setStatus(ifelse(publish, "published", "draft"))
      }
      break;
    }
  }
  
  #if publish, we save all
  if(publish){
    config$logger.info(sprintf("Export record to Zenodo metadata formats", zenodo_metadata$getConceptDOI()))
    out$exportAsAllFormats(file.path(getwd(),"metadata",entity$identifiers[["id"]]))
  }
  
}
