function(action, entity, config){
  writeWorkflowJobDataResource(
    entity = entity,
    config = config,
    obj = NULL,
    useFeatures = TRUE,
    resourcename = NULL,
    useUploadSource = TRUE,
    type = "shp"
  )
}