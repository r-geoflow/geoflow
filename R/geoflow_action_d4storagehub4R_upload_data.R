d4storagehub4R_upload_data <- function(entity, config, options){
  
  #options
  depositWithFiles <- if(!is.null(options$depositWithFiles)) options$depositWithFiles else FALSE
  
  #verify existing of software and workspace property
  #------------------------------------------------------------------------------------------------- 
  D4STORAGE_HUB <- config$software$output$d4storagehub
  if(is.null(D4STORAGE_HUB)){
    errMsg <- "This action requires a d4storagehub software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  D4STORAGE_HUB_CONFIG <- config$software$output$d4storagehub_config
  workspace <- D4STORAGE_HUB_CONFIG$properties$workspace
  if(is.null(workspace)) if(!is.null(entity$data$workspaces$d4storagehub)) workspace <- entity$data$workspaces$d4storagehub
  if(is.null(workspace)){
    errMsg <- "The d4storagehub configuration requires a workspace for publishing action"
    config$logger.error(errMsg)
    stop(errMsg)
  }    
  
  #verify  if folder exist and create it if missing
  #-------------------------------------------------------------------------------------------------  
  workspace<- file.path(workspace,entity$getEntityJobDirname())
    folderID <- D4STORAGE_HUB$searchWSFolderID(folderPath = file.path(workspace,"data"))
    if (is.null(folderID)) {
      config$logger.info(sprintf("Creating folder [%s] in d4cience workspace", workspace))
      D4STORAGE_HUB$createFolder(folderPath = workspace, name="data", description = entity$titles[['title']], hidden = FALSE, recursive = TRUE)
    }

  #upload
  #-------------------------------------------------------------------------------------------------
  if(entity$data$upload){
    config$logger.info("Upload mode is set to true")
    fileName = entity$data$uploadSource[[1]]
    filePath = file.path(getwd(),"data",fileName)
    if(startsWith(entity$data$uploadType,"db")){
      errMsg <- "Skipping upload: Upload mode is no valid for 'database' type"
      config$logger.error(errMsg)
      stop(errMsg)
    }else{
      config$logger.info(sprintf("Trying to upload %s to d4science workspace folder %s", fileName, file.path(workspace, "data")))
      D4STORAGE_HUB$uploadFile (folderPath = file.path(workspace, "data"), file=filePath, description = "", archive = FALSE)
      config$logger.info(sprintf("File %s successfully uploaded to the d4science folder %s", fileName, file.path(workspace, "data")))
    }
  }  

  #enrish with relation
  #-------------------------------------------------------------------------------------------------
    new_d4storagehub_link<- geoflow_relation$new()
    new_d4storagehub_link$setKey("http")
    new_d4storagehub_link$setName(fileName)
    new_d4storagehub_link$setDescription(paste0(entity$titles[['title']]," - D4Science Data Download (",entity$data$uploadType,")"))
    new_d4storagehub_link$setLink(D4STORAGE_HUB$getPublicFileLink(file.path(workspace, "data",fileName)))
    
    entity$addRelation(new_d4storagehub_link)

  #other files uploads
  if(depositWithFiles){
    #check if other data files than uploadSource exists
    data_files <- setdiff(list.files(file.path(getwd(),"data")),fileName)
    if(length(data_files)>0){
      #upload other data files
      for(data_file in data_files){
        config$logger.info(sprintf("D4storagehub: uploading data file '%s'", data_file))
        D4STORAGE_HUB$uploadFile (folderPath = file.path(workspace, "data"), file=file.path(getwd(),"data",data_file), description = "", archive = FALSE)
      }
    }else{
          config$logger.warn("D4storagehub: no other data files to upload")
    }
  
    #check if metadata files exists
    metadata_files <- list.files(file.path(getwd(),"metadata"))
    if(length(metadata_files)>0){
      metadata_folderID <- D4STORAGE_HUB$searchWSFolderID(folderPath = file.path(workspace,"metadata"))
      #check if metadata folder exists
      if (is.null(metadata_folderID)) {
        config$logger.info(sprintf("Creating folder [%s] in d4cience workspace", file.path(workspace,"metadata")))
        D4STORAGE_HUB$createFolder(folderPath = workspace, name="metadata", description = entity$titles[['title']], hidden = FALSE, recursive = TRUE)
      }
      #upload metadata files
      for(metadata_file in metadata_files){
        config$logger.info(sprintf("D4storagehub: uploading metadata file '%s'", metadata_file))
        D4STORAGE_HUB$uploadFile (folderPath = file.path(workspace, "metadata"), file=file.path(getwd(),"metadata",data_file), description = "", archive = FALSE)
      }
    }else{
      config$logger.warn("D4storagehub: no metadata files to upload")
    }
  }else{
      config$logger.info("Skipping upload of no uploadSource files (option 'depositWithFiles' FALSE)")
  }    
}

