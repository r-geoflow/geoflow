function(action, entity, config){
  
  if(!requireNamespace("ocs4R", quietly = TRUE)){
    stop("The 'ocs4R-upload' action requires the 'ocs4R' package")
  }
  
  OCS_CONFIG = config$software$output$ocs_config
  OCS = config$software$output$ocs
  if(is.null(OCS)){
    errMsg <- "This action requires a OCS software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  workspace <- OCS_CONFIG$properties$workspace
  if(is.null(workspace)) if(!is.null(entity$data$workspaces$ocs)) workspace <- entity$data$workspaces$ocs
  if(is.null(workspace)){
    errMsg <- "The OCS configuration requires a workspace for publishing action"
    config$logger.error(errMsg)
    stop(errMsg)
  }    
  if(!startsWith(workspace, "/")) workspace = paste0("/", workspace)

  #upload
  #-------------------------------------------------------------------------------------------------
  if(entity$data$upload){
    #try to create collection (remote folder)
    workspace_name = substring(workspace, 2, nchar(workspace))
    try(OCS$makeCollection(name = workspace_name))
    
    config$logger.info("Upload mode is set to true")
    fileName = entity$data$uploadSource[[1]]
    filePath = file.path(getwd(),"data",fileName)
    if(startsWith(entity$data$uploadType,"db")){
      errMsg <- "Skipping upload: Upload mode is no valid for 'database' type"
      config$logger.error(errMsg)
      stop(errMsg)
    }else{
      config$logger.info(sprintf("Trying to upload %s to cloud workspace folder %s", fileName, workspace))
      OCS$uploadFile(filename = filePath, relPath = workspace, delete_if_existing = FALSE)
      config$logger.info(sprintf("File %s successfully uploaded to the cloud folder %s", fileName, workspace))
    }
  }
  
  #enrish with relation
  #-------------------------------------------------------------------------------------------------
  cloud_link_url = try(OCS$shareAsPublicLink(workspace, filePath, permissions = "read"), silent = TRUE)
  if(!is(cloud_link_url, "try-error")){
    new_cloud_link<- geoflow_relation$new()
    new_cloud_link$setKey("http")
    new_cloud_link$setName(fileName)
    new_cloud_link$setDescription(paste0(entity$titles[['title']]," - Cloud Data Download (",entity$data$uploadType,")"))
    new_cloud_link$setLink(cloud_link_url)
    entity$addRelation(new_cloud_link)
  }
  
}