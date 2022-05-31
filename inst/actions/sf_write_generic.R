function(action, entity, config){
  
  #options
  options <- action$options
  createIndexes <- ifelse(!is.null(options$createIndexes), options$createIndexes, FALSE)
  overwrite <- ifelse(!is.null(options$overwrite), options$overwrite, TRUE)
  append <- ifelse(!is.null(options$append), options$append, FALSE)
  chunk.size <- ifelse(!is.null(options$chunk.size), options$chunk.size, 0L)
  #function
  writeWorkflowJobDataResource(
    entity = entity,
    config = config,
    obj = NULL,
    useFeatures = TRUE,
    resourcename = NULL,
    useUploadSource = TRUE,
    createIndexes = createIndexes,
    overwrite = overwrite,
    append = append,
    chunk.size = chunk.size,
    type=options$type
  )
}