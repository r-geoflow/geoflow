#For import in dbi
action_import_dbi<-function(entity,config,options){
  writeWorkflowDataResource(entity=entity=config,config,obj=NULL,useFeatures=TRUE,resourcename=NULL,useUploadSource=TRUE,type="dbtable")}
#For enrich features of shp
action_import_shp<-function(entity,config,options){
  writeWorkflowDataResource(entity=entity,config=config,obj=NULL,useFeatures=TRUE,resourcename=NULL,useUploadSource=TRUE,type="shp")}
#For generic action
action_import_generic<-function(entity,config,options){
  writeWorkflowDataResource(entity=entity,config=config,obj=NULL,useFeatures=TRUE,resourcename=NULL,useUploadSource=TRUE,type=options$type)}

    