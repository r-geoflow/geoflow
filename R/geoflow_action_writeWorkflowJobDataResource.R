#For import in dbi
action_import_dbi<-function(entity,config,options){
  writeWorkflowDataResource(entity=entity,config=config,obj=NULL,useFeatures=TRUE,resourcename=NULL,useUploadSource=TRUE,createIndexes=options$createIndexes,type="dbtable")
  #options
  createIndexes<-ifelse(!is.null(options$createIndexes), options$createIndexes, FALSE) }

#For enrich features of shp
action_import_shp<-function(entity,config,options){
  writeWorkflowDataResource(entity=entity,config=config,obj=NULL,useFeatures=TRUE,resourcename=NULL,useUploadSource=TRUE,type="shp")}

#For generic action
action_import_generic<-function(entity,config,options){
  writeWorkflowDataResource(entity=entity,config=config,obj=NULL,useFeatures=TRUE,resourcename=NULL,useUploadSource=TRUE,createIndexes=options$createIndexes,type=options$type)
  #options
  createIndexes<-ifelse(!is.null(options$createIndexes), options$createIndexes, FALSE) }
    