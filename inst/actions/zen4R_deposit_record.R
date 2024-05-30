function(action, entity, config){
  
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
  #skipDataDownload
  skipDataDownload = FALSE
  if(!is.null(config$profile$options$skipFileDownload)){
    config$logger.warn("Global option 'skipFileDownload' is deprecated, use 'skipDataDownload instead!")
    skipDataDownload = config$profile$options$skipFileDownload
  }
  skipDataDownload <- if(!is.null(config$profile$options$skipDataDownload)) config$profile$options$skipDataDownload else FALSE
  
  #options
  depositWithFiles <- action$getOption("depositWithFiles")
  depositDataPattern <- action$getOption("depositDataPattern")
  depositMetadataPattern <- action$getOption("depositMetadataPattern")
  zipEachDataFile <- action$getOption("zipEachDataFile")
  publish <- action$getOption("publish")
  review <- action$getOption("review")
  strategy <- action$getOption("strategy")
  deleteOldFiles <- action$getOption("deleteOldFiles")
  update_metadata <- action$getOption("update_metadata")
  update_files <- action$getOption("update_files")
  communities <- action$getOption("communities")
  
  #get_zenodo_metadata
  get_zenodo_metadata = function(){
    #zenodo object
    zen_meta <- NULL
    
    #how to deal with existing records / new versions
    #we try first to use DOI if existing, assuming it's a concept DOI, if not a simple DOI
    #if nothing is found, we use the default get depositions by identifier, used if no DOI is specified
    #this approach is possible because the Zenodo record has a related identifier as 'other', specified when
    #creating the record. For existing record we also check the presence of the 'other' as related identifier
    #e.g. my-metadata-identifier
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
            if(related_identifier$scheme == "other"){
              if(related_identifier$identifier == entity$identifiers[["id"]] &
                 related_identifier$relation_type$id == "isidenticalto"){
                zen_meta <<- deposit
                break
              }
            }
          }
        }
      }))
    }
    return(zen_meta)
  }
  
  #zenodo_metadata
  zenodo_metadata = get_zenodo_metadata()
  
  #doi
  doi <- NULL
  
  #action to perform: create empty record or update existing record
  update <- FALSE
  record_status <- NULL
  if(is.null(zenodo_metadata)){
    config$logger.info(sprintf("Zenodo: No existing Zenodo record with related identifier '%s'", entity$identifiers[["id"]]))
    config$logger.info("Zenodo: creating a new deposit empty record")
    reserveDOI = TRUE
    if(!is.null(entity$identifiers[["doi"]])) if(regexpr("zenodo", entity$identifiers[["doi"]])<0){
      reserveDOI = FALSE
    }
    zenodo_metadata <- ZENODO$createEmptyRecord(reserveDOI = reserveDOI)
    if("bucket" %in% names(zenodo_metadata$links)){
      entity$addResource("zenodo_bucket", zenodo_metadata$links$bucket) #attempt to keep bucket since zenodo seems not to retrieve always bucket link
    }
    zenodo_metadata$addRelatedIdentifier(
      identifier = entity$identifiers[["id"]],
      scheme = "other",
      relation_type = "isidenticalto"
    )
    record_status <- zenodo_metadata$status
  }else{
    config$logger.info(sprintf("Zenodo: Existing record with related identifier '%s' ('other' scheme)", entity$identifiers[["id"]]))
    update <- TRUE
    record_status <- zenodo_metadata$status
    
    #case of published records and 'edition' strategy, need to unlock record
    if(record_status == "published" &&
       zenodo_metadata$is_published &&
       !zenodo_metadata$is_draft &&
       strategy == "edition"){
      config$logger.info(sprintf("Zenodo: record '%s' already published. Need to unlock it for edition", zenodo_metadata$id))
      unlocked_rec <- ZENODO$editRecord(zenodo_metadata$id)
      if(is(unlocked_rec, "ZenodoRecord")){
        zenodo_metadata <- unlocked_rec
      }
    }
    
    if(length(zenodo_metadata$getVersions())>0 && strategy == "newversion"){
      #to know if the record has been already published, we check if there is at least one version set
      #the property "is_published" is transient to the record and can't be used
      if(!depositWithFiles){
        #the first run of the action will be skipped and we will wait for the final generic_uploader run
        #with depositWithFiles=TRUE to run the version to avoid two versions to be created
        config$logger.info(sprintf("Zenodo: record '%s' already published. Skip 1st run to create version with generic uploader at the end of the workflow", zenodo_metadata$id))
        return(TRUE)
      }
    }
    
    if(record_status == "new_version_draft"){
      config$logger.info("Draft version already set. Discard initial changes before publishing new version...")
      ZENODO$discardChanges(zenodo_metadata$id)
      zenodo_metadata = get_zenodo_metadata()
      record_status = zenodo_metadata$status
    }
    
    #case where bucket is not kept by zenodo we try to get it from added resource
    if(!"bucket" %in% names(zenodo_metadata$links)) if(!is.null(entity$resources$zenodo_bucket)){
      zenodo_metadata$links$bucket <- entity$resources$zenodo_bucket
    }
  }
  
  doi <- zenodo_metadata$pids$doi$identifier
  
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
    if(!is.null(entity$titles[["alternative"]])){
      zenodo_metadata$addAdditionalTitle(entity$titles[["alternative"]], type = "alternative-title")
    }
    zenodo_metadata$setDescription(entity$descriptions[["abstract"]])
    if(!is.null(entity$descriptions[["info"]])){
      zenodo_metadata$addAdditionalDescription(entity$descriptions[["info"]], type = "technical-info")
    }
      
    #keywords (free text) & subjects
    zenodo_metadata$metadata$keywords <- list()
    zenodo_metadata$metadata$subjects <- list()
    for(subject in entity$subjects){
      for(kwd in subject$keywords){
        zenodo_metadata$addSubject(kwd$name)
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
    #resource type
    #TODO think on how to map resource types between Dublin core, ISO/OGC metadata, Zenodo  
    if(!is.null(entity$types[["generic"]])) zenodo_metadata$setResourceType(tolower(entity$types[["generic"]]))
    if(!is.null(entity$types[["zenodoResourceType"]])) zenodo_metadata$setResourceType(entity$types[["zenodoResourceType"]])
    
    #publisher
    if(length(entity$contacts)>0){
      publisher <- "Zenodo"
      publishers <- entity$contacts[sapply(entity$contacts, function(x){x$role == "publisher"})]
      if(length(publishers)>0){
        publisher = publishers[[1]]$organizationName #we assume publisher is an organization
      }
      zenodo_metadata$setPublisher(publisher)
    }
    
    #creators
    #we first look for contacts with 'author' role (that is the most suitable for Zenodo deposition creators)
    #if there is no 'author', we look for owners
    contact_added <- list()
    zenodo_metadata$metadata$creators <- list()
    contacts <- list()
    if(length(entity$contacts)>0){
      contacts <- entity$contacts[sapply(entity$contacts, function(x){x$role == "author"})]
      if(length(contacts)==0) contacts <- entity$contacts[sapply(entity$contacts, function(x){x$role == "owner"})]
    }
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
            affiliations = contact$organizationName, 
            orcid = orcid
          )
        }else{
          zenodo_metadata$addCreator(
            firstname = contact$firstName, 
            lastname = contact$lastName, 
            affiliations = contact$organizationName,
            orcid = orcid
          )
        }
        contact_added <- c(contact_added, contact$identifiers[["id"]])
      }
    }
    #TODO contributors
    
    #Licenses
    if(length(entity$rights)>0){
      licenses <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "license"})]
      if(length(licenses)>0){
        license <- licenses[[1]]$values[[1]]
        the_license <- ZENODO$getLicenseById(license)
        if(!is.null(the_license)){
          zenodo_metadata$setLicense(license, sandbox = ZENODO$sandbox)
        }else{
          config$logger.warn(sprintf("Zenodo :license specified (%s) in entity doesn't match Zenodo accepted list of licenses. license %s ignored!", 
                                     license,license))
        }  
      }
    }

    # AccessRight  
    zenodo_metadata$setAccessPolicyRecord("public") #always the case for Zenodo (at least for now)
    # Access right with the following values: 'public','restricted'
    if(length(entity$rights)>0){
        accessRights <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "accessright"})]
        config$logger.info(sprintf("accessRight: '%s'", accessRights))
        if(length(accessRights)>0){
        accessRight <- accessRights[[1]]$values[[1]]
        config$logger.info(sprintf("accessRight Value: '%s'", accessRight))
        zenodo_metadata$setAccessPolicyFiles(accessRight)
        if(accessRight == "restricted"){
          #manage embargo
          embargoDates <- entity$dates[sapply(entity$dates, function(date){date$key == "embargo"})]
          if(length(embargoDates)>0){
            embargoDate = embargoDates[[1]]$value
            config$logger.info(sprintf("Setting embargo date '%s'", embargoDate))
            embargoReason = ""
            embargoReasons = entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "embargoreason"})]
            if(length(embargoReasons)>0){
              embargoReason = embargoReasons[[1]]$values[[1]]
              config$logger.info(sprintf("Setting embargo reason: %s", embargoReason))
            }
            zenodo_metadata$setAccessPolicyEmbargo(active = TRUE, until = as.Date(embargoDate), reason = embargoReason)
          }
          #access conditions
          #TODO to review if available through new Zeonod API
          # accessConditions <- entity$rights[sapply(entity$rights, function(x){tolower(x$key) == "accessconditions"})]
          # if(length(accessConditions)>0){
          #   zenodo_metadata$setAccessConditions(accessConditions[[1]]$values[[1]])
          # }
        }
      }else{
        config$logger.info(sprintf("Zenodo: accessRight specified in entity not available. accessRight will be set to public!"))
        zenodo_metadata$setAccessPolicyFiles("public")
      }
    }else{
        config$logger.info(sprintf("Zenodo: Rights is empty. accessRight will be set to public!"))
      zenodo_metadata$setAccessPolicyFiles("public")
    }
    
    #references
    if(length(entity$relations)>0){
      references = entity$relations[sapply(entity$relations, function(x){tolower(x$key) == "ref"})]
      if(length(references)>0){
        for(reference in references){
          ref = reference$name
          if(!is.null(reference$description)) ref = reference$description
          if(!is.null(reference$link)) ref = paste0(ref,". ",reference$link)
          zenodo_metadata$addReference(ref)
        }
      }
    }
    
    #grants
    if(length(entity$relations)){
      grants = entity$relations[sapply(entity$relations, function(x){tolower(x$key) == "grant"})]
      if(length(grants)>0){
        for(grant in grants){
          zenodo_metadata$addGrant(grant, sandbox = ZENODO$sandbox)
        }
      }
    }
    
  }else{
    config$logger.info("Skipping update of Zenodo record metadata (option 'update_metadata' FALSE)")
  }
  
  #file uploads (for new or edited records)
  #note: for new versions this is managed directly with ZENODO$depositRecordVersion
  if(depositWithFiles & (!update | (update & update_files)) & record_status == "draft" & zenodo_metadata$is_draft){
    if(deleteOldFiles & !skipDataDownload){
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
            zip::zipr(zipfile = outfilename, files = data_file)
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
  print(record_status)
  out <- switch(record_status,
                "draft" = {
                  config$logger.info(sprintf("Deposit draft record with id '%s' - publish = %s", zenodo_metadata$id, tolower(as.character(publish))))
                  ZENODO$depositRecord(
                    zenodo_metadata,
                    publish = FALSE #management of publication later
                  )
                },
                "published" = {
                  switch(strategy,
                   "edition" = {
                     config$logger.info(sprintf("Edit published record with id '%s' - publish = %s", zenodo_metadata$id, tolower(as.character(publish))))
                     ZENODO$depositRecord(zenodo_metadata, publish = publish)
                    },
                   "newversion" = {
                     config$logger.info(sprintf("Deposit record version with id '%s' - publish = %s", zenodo_metadata$id, tolower(as.character(publish))))
                     data_files <- list.files(file.path(getwd(),"data"), pattern = depositDataPattern, full.names = T)
                     
                     if(zipEachDataFile){
                       config$logger.info("Zenodo: 'zipEachDaTafile' is true - zipping data files")
                       data_files <- lapply(data_files, function(data_file){
                         data_file = basename(data_file)
                         config$logger.info(sprintf("Zenodo: 'zipEachDaTafile' is true - zipping each data file '%s'", data_file))
                         fileparts <- unlist(strsplit(data_file, "\\."))
                         if(length(fileparts)>1) fileparts <- fileparts[1:(length(fileparts)-1)]
                         filename <- paste0(fileparts, collapse = ".")
                         outfilename <- file.path(getwd(), "data", paste0(filename, ".zip"))
                         zip::zipr(zipfile = outfilename, files = data_file)
                         return(outfilename)
                       })
                     }
                     
                     metadata_files <- list.files(file.path(getwd(),"metadata"), full.names = TRUE)
                     files_to_upload <- if(depositWithFiles & (!update | (update & update_files))) c(data_files, metadata_files) else NULL
                     
                     config$logger.info("Deposit record version...")
                     ZENODO$depositRecordVersion(
                       record = zenodo_metadata, 
                       delete_latest_files = deleteOldFiles,
                       files = files_to_upload,
                       publish = FALSE #management of publication later
                     )
                   }
                  )
                },
                "new_version_draft" = {
                  switch(strategy,
                    "newversion" = {
                     config$logger.info(sprintf("Deposit record version with id '%s' - publish = %s", zenodo_metadata$id, tolower(as.character(publish))))
                     data_files <- list.files(file.path(getwd(),"data"), pattern = depositDataPattern, full.names = T)
                     
                     if(zipEachDataFile){
                       config$logger.info("Zenodo: 'zipEachDaTafile' is true - zipping data files")
                       data_files <- lapply(data_files, function(data_file){
                         data_file = basename(data_file)
                         config$logger.info(sprintf("Zenodo: 'zipEachDaTafile' is true - zipping each data file '%s'", data_file))
                         fileparts <- unlist(strsplit(data_file, "\\."))
                         if(length(fileparts)>1) fileparts <- fileparts[1:(length(fileparts)-1)]
                         filename <- paste0(fileparts, collapse = ".")
                         outfilename <- file.path(getwd(), "data", paste0(filename, ".zip"))
                         zip::zipr(zipfile = outfilename, files = data_file)
                         return(outfilename)
                       })
                     }
                     
                     metadata_files <- list.files(file.path(getwd(),"metadata"), full.names = TRUE)
                     files_to_upload <- if(depositWithFiles & (!update | (update & update_files))) c(data_files, metadata_files) else NULL
                     
                     config$logger.info("Deposit record version...")
                     ZENODO$depositRecordVersion(
                       record = zenodo_metadata, 
                       delete_latest_files = deleteOldFiles,
                       files = files_to_upload,
                       publish = FALSE #management of publication later
                     )
                   }
                  )
                }
  )
  if(!is(out,"ZenodoRecord")){
    errMsg <- sprintf("Zenodo: %s", out$errors[[1]]$message)
    config$logger.error(errMsg)
    stop(errMsg)
  }else{
    
    if(publish){
      if(review){
        #publication is delegated to a review procedure by a community
        config$logger.info("Delegated record publication (through community review)")
        goReview = FALSE
        if(length(communities)>0){
          zen_com = ZENODO$getCommunityById(communities[1])
          if(!is.null(zen_com)){
            goReview = TRUE
          }else{
            config$logger.warn(sprintf("Community '%s' doesn't exist, aborting submission of record for review", communities[1]))
          }
        }else{
          config$logger.warn(sprintf("No community defined, aborting submission of record for review"))
        }
        
        if(goReview){
          ZENODO$createReviewRequest(out, community = communities[1])
          submitted_for_review = ZENODO$submitRecordForReview(out$id)
          if(!submitted_for_review){
            config$logger.warn(sprintf("Record submission for review to community '%s' didn't work as expected, aborting...", communities[1]))
            ZENDO$deleteReviewRequest(out)
          }
        }
      }else{
        #direct publication without review by a community
        config$logger.info("Direct record publication (without review)")
        out = ZENODO$publishRecord(out$id)
      }
    }
    
    
    #business logic for communities linkage to published records
    #check that record is not yet associated to community
    #If ok, then check there is no pending request
    #If ok, we submit it to the community
    #If the geoflow user is maintainer of this community, make possible to accept immediatly the record
    if(length(communities)>0){
      if(out$metadata$status == "published"){
        config$logger.info("Adding published record to communities")
        for(community in communities){
          config$logger.info(sprintf("-> Processing community %s", community))
          zen_com = ZENODO$getCommunityById(community)
          #we check the comunity exists
          if(!is.null(zen_com)){
            #we check if the record is already in community
            rec_coms = ZENODO$getRecordCommunities(out)
            has_com = FALSE
            if(length(rec_coms)>0) has_com = any(sapply(rec_coms, function(x){x$id == zen_com$id}))
            if(!has_com){
              #if record is not in community we check pending requests
              pending_reqs = ZENODO$getRequests(q = sprintf("status:submitted AND receiver.community:%s AND topic.record:%s", zen_com$id, out$id))
              if(length(pending_reqs)==0){
                #!! This code assumes the record has been published and is not in draft stage
                #TODO investigate the API method to assign community to draft
                ZENODO$submitRecordToCommunities(record, communities = community)
                #TODO in case the geoflow user is manager for the community, give action option to accept it immediatly
              }
            }
          }
        }
      }else{
        config$logger.info("Record is not published, abort addition to communities!")
      }
    }
  }
  
  #we set the (prereserved) doi to the entity in question
  doi_to_save <- out$pids$doi$identifier
  if(!is.null(entity$identifiers[["doi"]])) doi_to_save <- entity$identifiers[["doi"]]
  config$logger.info(sprintf("Setting DOI '%s' to save and export for record",doi_to_save))
  for(i in 1:length(config$metadata$content$entities)){
    ent <- config$metadata$content$entities[[i]]
    if(ent$identifiers[["id"]]==entity$identifiers[["id"]]){
      if(regexpr("zenodo", doi)>0){
        config$metadata$content$entities[[i]]$identifiers[["zenodo_doi_to_save"]] <- out$getDOI()
        config$metadata$content$entities[[i]]$identifiers[["zenodo_conceptdoi_to_save"]] <- out$getConceptDOI()
        config$metadata$content$entities[[i]]$setStatus("zenodo", ifelse(publish, if(review) "under review" else "published", "draft"))
      }
      break;
    }
  }
  entity$identifiers[["zenodo_doi_to_save"]] <- out$getDOI()
  entity$identifiers[["zenodo_conceptdoi_to_save"]] <- out$getConceptDOI()
  entity$setStatus("zenodo", ifelse(publish, if(review) "under review" else "published", "draft"))
  
  #if publish, we save all
  if(publish){
    config$logger.info(sprintf("Export record '%s' to Zenodo metadata formats", out$getConceptDOI()))
    out$exportAsAllFormats(file.path(getwd(),"metadata",entity$identifiers[["id"]]))
  }
  
}
