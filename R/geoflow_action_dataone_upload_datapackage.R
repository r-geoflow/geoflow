dataone_upload_datapackage <- function(entity, config, options){
  
  #options
  publish <- TRUE #see https://github.com/DataONEorg/rdataone/issues/262
  accessRules <- NA
  
  #check if DataOne software is available
  DATAONE <- config$software$output$dataone
  if(is.null(DATAONE)){
    errMsg <- "This action requires a DataOne software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #action
  members <- list()
  packageId <- entity$identifiers[["packageId"]]
  action <- ifelse(is.null(packageId),"CREATE","UPDATE")
  update <- action == "UPDATE"
  
  #create datapackage
  dp <- switch(action,
    "CREATE" = new("DataPackage"),
    "UPDATE" = dataone::getDataPackage(DATAONE, identifier = packageId, lazyLoad = TRUE, limit="0MB", quiet=FALSE)
  )
  if(update){
    members <- datapack::getIdentifiers(dp)
  }
   
  #EML metadata
  dp_eml_meta_obj <- NULL
  eml_file <- file.path("metadata", paste0(entity$identifiers[["id"]], "_EML.xml"))
  if(file.exists(eml_file)){
    eml_meta_obj <- EML::read_eml(eml_file)
    newPackageId <- eml_meta_obj$packageId
    eml_format <- head(unlist(strsplit(eml_meta_obj$schemaLocation, " ")), n = 1)
    dp_eml_meta_obj <<- new(
      "DataObject",
      format = eml_format,
      filename = eml_file
    )
    dp <- datapack::addMember(dp, dp_eml_meta_obj)
  }
  
  #other metadata
  other_md_files <- list.files("metadata")
  other_md_files <- other_md_files[startsWith(other_md_files, entity$identifiers[["id"]])]
  other_md_files <- other_md_files[!endsWith(other_md_files, "_EML.xml")]
  if(length(other_md_files)>0){
    for(other_md_file in other_md_files){
      other_md_file_path <- file.path("metadata", other_md_file)
      dp_other_meta_obj <- new(
        "DataObject",
        format = mime::guess_type(other_md_file_path),
        filename = other_md_file_path
      )
      dp <- datapack::addMember(dp, dp_other_meta_obj, dp_eml_meta_obj)
    }
  }
  
  #data
  data_files <- list.files("data")
  data_files <- data_files[startsWith(data_files, entity$identifiers[["id"]])]
  if(length(data_files)>0){
    for(data_file in data_files){
      data_file_path <- file.path("data", data_file)
      dp_data_obj <- new(
        "DataObject",
        format = mime::guess_type(data_file_path),
        filename = data_file_path
      )
      dp <- datapack::addMember(dp, dp_data_obj, dp_eml_meta_obj)
    }
  }
  
  if(update){
    for(member in members) datapack::removeMember(dp, member, removeRelationships = TRUE)
    attr(dp, "sysmeta")@serialVersion <- attr(dp, "sysmeta")@serialVersion + 1
    attr(dp, "sysmeta")@dateUploaded <- attr(dp, "sysmeta")@dateSysMetadataModified
  }
  
  #upload
  out <- try(
    uploadDataPackage(
      DATAONE,
      dp,
      public = publish,
      accessRules = accessRules,
      quiet = FALSE
    )
  )
  
  if(is(out, "try-error")){
    errMsg <- sprintf("Error during uploading data package to DataOne:\n%s", as(out, "character"))
    config$logger.error(errMsg)
  }else{
    config$logger.info(sprintf("Successfully uploaded data package '%s'", out))
  }
  
  #output table of DOIs
  if(is(out, "character")){
    infoMsg <- switch(action,
      "CREATE" = sprintf("Successfully created data package with id '%s'", 
                         entity$identifiers[["id"]]),
      "UPDATE" = sprintf("Successfully updated Dataverse dataset with id '%s' (packageId: %s)", 
                         entity$identifiers[["id"]], packageId)
    )
    config$logger.info(infoMsg)
    
    #get the packageId assigned by DataOne
    packageId_to_save <- out
    config$logger.info(sprintf("Setting packageId '%s' (inherited from DataOne) to save and export for record", packageId_to_save))
    for(i in 1:length(config$metadata$content$entities)){
      ent <- config$metadata$content$entities[[i]]
      if(ent$identifiers[["id"]]==entity$identifiers[["id"]]){
        config$metadata$content$entities[[i]]$identifiers[["dataone_packageId_to_save"]] <- packageId_to_save
        config$metadata$content$entities[[i]]$setStatus("dataone", ifelse(publish, "published", "draft"))
        break;
      }
    }
    entity$identifiers[["packageId"]] <- packageId_to_save
    entity$identifiers[["dataone_packageId_to_save"]] <- packageId_to_save
    entity$setStatus("dataone", ifelse(publish, "published", "draft"))
  }

}