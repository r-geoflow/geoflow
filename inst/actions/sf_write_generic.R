function(action, entity, config){
  
  #options
  createIndexes <- action$getOption("createIndexes")
  overwrite <- action$getOption("overwrite")
  append <- action$getOption("append")
  chunk.size <- action$getOption("chunk.size")
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