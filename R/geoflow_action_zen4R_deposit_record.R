zen4R_deposit_record <- function(entity, config, options){
  
  if(!requireNamespace("zen4R", quietly = TRUE)){
    stop("The 'zen4R-deposit-record' action requires the 'zen4R' package")
  }
  
  ZENODO <- config$software$output$zenodo
  
  if(is.null(ZENODO)){
    errMsg <- "This action requires the Zenodo API to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #global options
  skipFileDownload <- if(!is.null(config$profile$options$skipFileDownload)) config$profile$options$skipFileDownload else FALSE
  
  #options
  depositWithFiles <- if(!is.null(options$depositWithFiles)) options$depositWithFiles else FALSE
  depositDataPattern <- if(!is.null(options$depositDataPattern)) options$depositDataPattern else ""
  depositMetadataPattern <- if(!is.null(options$depositMetadataPattern)) options$depositMetadataPattern else ""
  zipEachDataFile <- if(!is.null(options$zipEachDataFile)) options$zipEachDataFile else FALSE
  publish <- if(!is.null(options$publish) & depositWithFiles) options$publish else FALSE
  strategy <- if(!is.null(options$strategy)) options$strategy else "newversion"
  deleteOldFiles <- if(!is.null(options$deleteOldFiles)) options$deleteOldFiles else TRUE
  update_metadata <- if(!is.null(options$update_metadata)) options$update_metadata else TRUE
  update_files <- if(!is.null(options$update_files)) options$update_files else TRUE
  communities <- if(!is.null(options$community)) options$community else NA
  
  #zenodo object
  zenodo_metadata <- NULL
  
  #how to deal with existing records / new versions
  #we try first to use DOI if existing, assuming it's a concept DOI, if not a simple DOI
  #if nothing is found, we use the default get depositions by identifier, used if no DOI is specified
  #this approach is possible because the Zenodo record has a related identifier as URN, specified when
  #creating the record. For existing record we also check the presence of the URN as related identifier
  #e.g. urn:my-metadata-identifier
  deposits <- NULL
  if(!is.null(entity$identifiers[["doi"]])){
    #TODO review if getting deposit with concept DOI we don't inherit bucket link for doing file upload with new API
    deposit <- try(ZENODO$getDepositionByConceptDOI(entity$identifiers[["doi"]]), silent = TRUE) #try to get latest record with concept DOI
    if(is(deposit, "try-error")) deposit = NULL
    if(is.null(deposit)) deposit <- ZENODO$getDepositionByDOI(entity$identifiers[["doi"]]) #try to get record with specific DOI
    if(!is.null(deposit)) deposits <- list(deposit)
  }
  
  if(is.null(deposits)){
    deposit <- NULL
    if(!is.null(entity$identifiers[["zenodo_doi_to_save"]])){
      deposit <- ZENODO$getDepositionByDOI(entity$identifiers[["zenodo_doi_to_save"]]) #try to get record with specific DOI
    }
    if(!is.null(deposit)) deposits <- list(deposit)
  }
  
  if(is.null(deposits)){
    deposits <- ZENODO$getDepositions(q = entity$identifiers[["id"]], size = 1000L)
  }
  #check related identifier
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
  
  #doi
  doi <- NULL
  
  #action to perform: create empty record or update existing record
  update <- FALSE
  record_state <- NULL
  if(is.null(zenodo_metadata)){
    config$logger.info(sprintf("Zenodo: No existing Zenodo record with related identifier '%s'", paste0("urn:",entity$identifiers[["id"]])))
    config$logger.info("Zenodo: creating a new deposit empty record")
    zenodo_metadata <- ZENODO$createEmptyRecord()
    if("bucket" %in% names(zenodo_metadata$links)){
      entity$addResource("zenodo_bucket", zenodo_metadata$links$bucket) #attempt to keep bucket since zenodo seems not to retrieve always bucket link
    }
    zenodo_metadata$addRelatedIdentifier("isIdenticalTo", paste("urn", entity$identifiers[["id"]], sep=":"))
    record_state <- zenodo_metadata$state
  }else{
    config$logger.info(sprintf("Zenodo: Existing record with related identifier '%s'", paste0("urn:",entity$identifiers[["id"]])))
    update <- TRUE
    record_state <- zenodo_metadata$state
    
    #case of submitted records and 'edition' strategy, need to unlock record
    if(zenodo_metadata$state == "done" && strategy == "edition"){
      config$logger.info(sprintf("Zenodo: record '%s' already published. Need to unlock it for edition", zenodo_metadata$id))
      unlocked_rec <- ZENODO$editRecord(zenodo_metadata$id)
      if(is(unlocked_rec, "ZenodoRecord")){
        zenodo_metadata <- unlocked_rec
      }
    }
    
    #case where bucket is not kept by zenodo we try to get it from added resource
    if(!"bucket" %in% names(zenodo_metadata$links)) if(!is.null(entity$resources$zenodo_bucket)){
      zenodo_metadata$links$bucket <- entity$resources$zenodo_bucket
    }
  }
  
  doi <- zenodo_metadata$metadata$prereserve_doi$doi
  
  #if entity already comes with a DOI, we set it (this might be a preset DOI from Zenodo or elsewhere)
  if(!is.null(entity$identifiers[["doi"]])){
    doi <- entity$identifiers[["doi"]]
  }

  #if entity comes with a foreign DOI (not assigned by Zenodo)
  #we set the DOI (which set prereserve_doi to FALSE)
  if(!is.null(doi)) if(regexpr("zenodo", doi)<0){
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
    zenodo_metadata$setTitle(entity$titles[["title"]])
    zenodo_metadata$setDescription(entity$descriptions[["abstract"]])
    
    #keywords (free text) & subjects
    zenodo_metadata$metadata$keywords <- list()
    zenodo_metadata$metadata$subjects <- list()
    for(subject in entity$subjects){
      for(kwd in subject$keywords){
        kwd_name <- kwd$name
        kwd_uri <- kwd$uri
        if(is.null(kwd_uri))
          zenodo_metadata$addKeyword(kwd_name)
        else
          zenodo_metadata$addSubject(kwd_name, kwd_uri)
      }
    }
    
    #date
    pubDates <- entity$dates[sapply(entity$dates, function(date){date$key == "publication"})]
    date <- if(length(pubDates)>0) pubDates[[1]]$value else entity$dates[[1]]$value
    zenodo_metadata$setPublicationDate(date)
    #version (mapped to geoflow descriptions edition)
    edition = entity$descriptions[["edition"]]
    if(!is.null(edition)){
      zenodo_metadata$setVersion(edition)
    }
    #upload type
    #TODO think on how to map upload types between Dublin core, ISO/OGC metadata, Zenodo  
    if(!is.null(entity$types[["generic"]])) zenodo_metadata$setUploadType(tolower(entity$types[["generic"]]))
    if(!is.null(entity$types[["zenodoUploadType"]])) zenodo_metadata$setUploadType(entity$types[["zenodoUploadType"]])
    
    #publication type
    if(zenodo_metadata$metadata$upload_type == "publication"){
      if(!is.null(entity$types[["zenodoPublicationType"]]))
        zenodo_metadata$setPublicationType(entity$types[["zenodoPublicationType"]])
    }
    #image type
    if(zenodo_metadata$metadata$upload_type == "image"){
      if(!is.null(entity$types[["zenodoImageType"]]))
        zenodo_metadata$setImageType(entity$types[["zenodoImageType"]])
    }
    
    #contacts
    #TODO think if correct handle all contacts (whatever roles) as creators (author/co-authors)
    contact_added <- list()
    zenodo_metadata$metadata$creators <- list()
    contacts <- list()
    if(length(entity$contacts)>0) contacts <- entity$contacts[sapply(entity$contacts, function(x){x$role == "owner"})]
    if(length(contacts)>0) for(contact in contacts){
      
      #manage orcid?
      orcid <- NULL
      contact_ids <- contact$identifiers
      if(any(names(contact_ids)=="orcid")){
        contact_ids <- contact_ids[names(contact_ids)=="orcid"]
        if(length(contact_ids)>0) orcid <- contact_ids[[1]]
      }
      #add/update creators
      if(!(contact$identifiers[["id"]] %in% contact_added)){
        if(is.na(contact$firstName) || is.na(contact$lastName)){
          zenodo_metadata$addCreator(
            name = contact$organizationName,
            affiliation = contact$organizationName,
            orcid = orcid
          )
        }else{
          zenodo_metadata$addCreator(
            firstname = contact$firstName, 
            lastname = contact$lastName, 
            affiliation = contact$organizationName,
            orcid = orcid
          )
        }
        contact_added <- c(contact_added, contact$identifiers[["id"]])
      }
    }
    
    #Licenses
    if(length(entity$rights)>0){
      licenses <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "license"})]
      if(length(licenses)>0){
        license <- licenses[[1]]$value
        accepted_licenses <- ZENODO$getLicenses()$id
        if(license%in%accepted_licenses){
        zenodo_metadata$setLicense(license)
        }else{
        config$logger.warn(sprintf("Zenodo :license specified (%s) in entity doesn't match Zenodo accepted list of licenses. license %s ignored!", 
                                   license,license))
        }  
      }
    }
    
    #TODO myrec$setAccessRight
    
    #communities
    if(!is.na(communities)){
      zenodo_metadata$metadata$communities <- list()
      for(community in communities) zenodo_metadata$addCommunity(community)
    }
  }else{
    config$logger.info("Skipping update of Zenodo record metadata (option 'update_metadata' FALSE)")
  }
  
  #file uploads (for new or edited records)
  #note: for new versions this is managed directly with ZENODO$depositRecordVersion
  if(depositWithFiles & (!update | (update & update_files)) & record_state == "unsubmitted"){
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
    data_files <- list.files(file.path(getwd(),"data"), pattern = depositDataPattern)
    if(length(data_files)>0){
      if(entity$data$upload){
        
        if(zipEachDataFile){
          config$logger.info("Zenodo: 'zipEachDaTafile' is true - zipping data files")
          data_files <- lapply(data_files, function(data_file){
            config$logger.info(sprintf("Zenodo: 'zipEachDaTafile' is true - zipping each data file '%s'", data_file))
            fileparts <- unlist(strsplit(data_file, "\\."))
            if(length(fileparts)>1) fileparts <- fileparts[1:(length(fileparts)-1)]
            filename <- paste0(fileparts, collapse = ".")
            outfilename <- file.path(getwd(), "data", paste0(filename, ".zip"))
            zip::zipr(zipfile = outfilename, files = data.file)
            return(outfilename)
          })
        }
        
        config$logger.info("Zenodo: uploading data files...")
        for(data_file in data_files){
          config$logger.info(sprintf("Zenodo: uploading data file '%s'", data_file))
          ZENODO$uploadFile(file.path(getwd(), "data", data_file), record = zenodo_metadata)
        }
      }else{
        config$logger.warn("Zenodo: entity data 'upload' is false, skip data files upload...")
      }
    }
    #upload metadata files, if any
    metadata_files <- list.files(file.path(getwd(),"metadata"), pattern = depositMetadataPattern)
    if(length(metadata_files)>0){
      if(length(metadata_files)>0) metadata_files <- metadata_files[!endsWith(metadata_files, ".rds")]
      if(length(metadata_files)>0){
        if(entity$data$upload){
          config$logger.info("Zenodo: uploading metadata files...")
          for(metadata_file in metadata_files){
            config$logger.info(sprintf("Zenodo: uploading metadata file '%s'", metadata_file))
            ZENODO$uploadFile(file.path(getwd(), "metadata",metadata_file), record = zenodo_metadata)
          }
        }else{
          config$logger.warn("Zenodo: entity data 'upload' is false, skip metadata files upload...")
        }
      }
    }
  }else{
    config$logger.info("Skipping update of Zenodo record files (option 'update_files' and/or 'depositWithFiles FALSE)")
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
      if(regexpr("zenodo", doi)>0) if(doi != entity$identifiers[["doi"]]){ 
        config$logger.warn(sprintf("DOI specified (%s) in entity doesn't match Zenodo record Concept DOI (%s). Zenodo 'publish' action ignored!", 
                                   entity$identifiers[["doi"]], doi))
        publish <- FALSE
      }
    }
  }
  config$logger.info(sprintf("Deposit record with id '%s' - publish = %s", zenodo_metadata$id, tolower(as.character(publish))))
  out <- switch(record_state,
      "unsubmitted" = ZENODO$depositRecord(zenodo_metadata, publish = publish),
      "inprogress" = ZENODO$depositRecord(zenodo_metadata, publish = publish),
      "done" = {
        switch(strategy,
          "edition" = ZENODO$depositRecord(zenodo_metadata, publish = publish),
          "newversion" = {
            data_files <- list.files(file.path(getwd(),"data"), pattern = depositDataPattern)
            
            if(zipEachDataFile){
              config$logger.info("Zenodo: 'zipEachDaTafile' is true - zipping data files")
              data_files <- lapply(data_files, function(data_file){
                config$logger.info(sprintf("Zenodo: 'zipEachDaTafile' is true - zipping each data file '%s'", data_file))
                fileparts <- unlist(strsplit(data_file, "\\."))
                if(length(fileparts)>1) fileparts <- fileparts[1:(length(fileparts)-1)]
                filename <- paste0(fileparts, collapse = ".")
                outfilename <- file.path(getwd(), "data", paste0(filename, ".zip"))
                zip::zipr(zipfile = outfilename, files = data.file)
                return(outfilename)
              })
            }
            
            metadata_files <- list.files(file.path(getwd(),"metadata"))
            files_to_upload <- if(depositWithFiles & (!update | (update & update_files))) c(data_files, metadata_files) else NULL
            ZENODO$depositRecordVersion(
              record = zenodo_metadata, 
              delete_latest_files = TRUE,
              files = files_to_upload,
              publish = publish
            )
          }
        )
      }
  )
  if(!is(out,"ZenodoRecord")){
    errMsg <- sprintf("Zenodo: %s", out$errors[[1]]$message)
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #we set the (prereserved) doi to the entity in question
  doi_to_save <- try(out$metadata$prereserve_doi$doi, silent = TRUE)
  if(is(doi_to_save, "try-error")) doi_to_save <- entity$identifiers[["doi"]]
  config$logger.info(sprintf("Setting DOI '%s' to save and export for record",out$metadata$prereserve_doi$doi))
  for(i in 1:length(config$metadata$content$entities)){
    ent <- config$metadata$content$entities[[i]]
    if(ent$identifiers[["id"]]==entity$identifiers[["id"]]){
      if(regexpr("zenodo", doi)>0){
        config$metadata$content$entities[[i]]$identifiers[["zenodo_doi_to_save"]] <- out$metadata$prereserve_doi$doi
        config$metadata$content$entities[[i]]$identifiers[["zenodo_conceptdoi_to_save"]] <- out$getConceptDOI()
        config$metadata$content$entities[[i]]$setStatus("zenodo", ifelse(publish, "published", "draft"))
      }
      break;
    }
  }
  entity$identifiers[["zenodo_doi_to_save"]] <- out$metadata$prereserve_doi$doi
  entity$identifiers[["zenodo_conceptdoi_to_save"]] <- out$getConceptDOI()
  entity$setStatus("zenodo", ifelse(publish, "published", "draft"))
  
  #if publish, we save all
  if(publish){
    config$logger.info(sprintf("Export record to Zenodo metadata formats", out$getConceptDOI()))
    out$exportAsAllFormats(file.path(getwd(),"metadata",entity$identifiers[["id"]]))
  }
  
}
