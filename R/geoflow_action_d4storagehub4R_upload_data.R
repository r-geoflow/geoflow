d4storagehub4R_upload_data <- function(entity, config, options){
  
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
  folderName = "data"
  
    folderID <- D4STORAGE_HUB$searchWSFolderID(folderPath = workspace)
    if (is.null(folderID)) {
      config$logger.info(sprintf("Creating folder [%s] in d4cience workspace", workspace))
      D4STORAGE_HUB$createFolder(folderPath = workspace, name=folderName, description = "", hidden = FALSE, recursive = TRUE)
    }

  #upload
  #-------------------------------------------------------------------------------------------------
  if(entity$data$upload){
    config$logger.info("Upload mode is set to true")
    fileName = entity$data$uploadSource
    filePath = file.path(getwd(),folderName,fileName)
    if(startsWith(entity$data$uploadType,"db")){
      errMsg <- "Skipping upload: Upload mode is no valid for 'database' type"
      config$logger.error(errMsg)
      stop(errMsg)
    }else{
      config$logger.info(sprintf("Trying to upload %s to d4science workspace folder %s", fileName, file.path(workspace, folderName)))
      D4STORAGE_HUB$uploadFile (folderPath = file.path(workspace, folderName), file=filePath, description = "", archive = FALSE)
      config$logger.info(sprintf("File %s successfully uploaded to the d4science folder %s", fileName, file.path(workspace, folderName)))
    }
  }  

  #enrish with relation
  #-------------------------------------------------------------------------------------------------
    new_d4storagehub_link<- geoflow_relation$new()
    new_d4storagehub_link$setKey("http")
    new_d4storagehub_link$setName(fileName)
    new_d4storagehub_link$setDescription(paste0(entity$titles[['title']]," - D4Science Data Download (",entity$data$uploadType,")"))
    new_d4storagehub_link$setLink(D4STORAGE_HUB$getPublicFileLink(file.path(workspace, folderName,fileName)))
    
    entity$addRelation(new_d4storagehub_link)
}