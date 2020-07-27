#For write generic action
sf_write_generic <- function(entity, config, options){
  #options
  createIndexes <- ifelse(!is.null(options$createIndexes), options$createIndexes, FALSE)
  #function
  writeWorkflowDataResource(
    entity = entity,
    config = config,
    obj = NULL,
    useFeatures = TRUE,
    resourcename = NULL,
    useUploadSource = TRUE,
    createIndexes = createIndexes,
    type=options$type
  )
}

#For write in dbi
sf_write_dbi <- function(entity, config, options){
  #options
  createIndexes<-ifelse(!is.null(options$createIndexes), options$createIndexes, FALSE) 
  #function
  writeWorkflowDataResource(
    entity = entity,
    config = config,
    obj = NULL,
    useFeatures = TRUE,
    resourcename = NULL,
    useUploadSource = TRUE,
    createIndexes = createIndexes,
    type = "dbtable"
  )
}

#For write as shp
sf_write_shp <- function(entity, config, options){
  writeWorkflowDataResource(
    entity = entity,
    config = config,
    obj = NULL,
    useFeatures = TRUE,
    resourcename = NULL,
    useUploadSource = TRUE,
    type = "shp"
  )
}

    