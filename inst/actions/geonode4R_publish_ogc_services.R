function(action, entity, config){
  
  if(!requireNamespace("geonode4R", quietly = TRUE)){
    stop("The 'geonode4R-publish-ogc-services' action requires the 'geonode4R' package")
  }
  
  #shortcut for geonode config
  GEONODE_CONFIG <- config$software$output$geonode_config
  GEONODE <- config$software$output$geonode
  if(is.null(GEONODE)){
    errMsg <- "This action requires a GeoNode software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  data_objects <- list()
  if(is.null(entity$data$dir)){
    data_objects <- list(entity$data)
  }else{
    data_objects <- entity$data$getData()
  }
  
  if(length(data_objects)>0) for(data_object in data_objects){
  
    #datasource
    datasource <- data_object$uploadSource[[1]]
    datasource_name <- NULL
    datasource_file <- NULL
    if(!is.null(datasource)){
      datasource_name <- unlist(strsplit(datasource, "\\."))[1]
      datasource_file <- attr(datasource, "uri")
      attributes(datasource) <- NULL
    }else{
      if(data_object$upload){
        errMsg <- sprintf("Upload source is missing!")
        stop(errMsg)
      }
    }
    
    #layername/sourcename
    layername <- if(!is.null(data_object$layername)) data_object$layername else entity$identifiers$id
    
    #upload
    #-------------------------------------------------------------------------------------------------
    if(data_object$upload){
      
      config$logger.info("Upload mode is set to true")
      if(startsWith(data_object$uploadType,"db") || data_object$uploadType == "other"){
        warnMsg <- "Skipping upload: Upload mode is only valid for types 'shp', 'spatialite' or 'h2'"
        config$logger.warn(warnMsg)
      }else{
        uploaded <- FALSE
        config$logger.info("Upload from local file(s)")
        filepath <- file.path(getwd(), "data", datasource)
        config$logger.info(sprintf("File to upload to GeoNode: %s", filepath))
        if(file.exists(filepath)){
          config$logger.info(sprintf("Upload file '%s' [%s] to GeoNode...", filepath, data_object$uploadType))
          files = list.files(path = "data", pattern = datasource_name, full.names = TRUE)
          files = files[!endsWith(files, ".zip")]
          created = GEONODE$upload(files)
          uploaded = created$success
        }else{
          errMsg <- sprintf("Upload from local file(s): no zipped file found for source '%s' (%s)", filepath, datasource)
          config$logger.error(errMsg)
          stop(errMsg)
        }
        
        if(uploaded){
          infoMsg <- sprintf("Successful GeoNode upload for file '%s' (%s)", datasource_file, data_object$uploadType)
          config$logger.info(infoMsg)
        }else{
          errMsg <- "Error during GeoNode file upload. Aborting 'geonode4R' action!"
          config$logger.error(errMsg)
          stop(errMsg)
        }
      }
    }
    
  }
  
}