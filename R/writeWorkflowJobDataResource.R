#' @name writeWorkflowJobDataResource
#' @aliases writeWorkflowJobDataResource
#' @title writeWorkflowJobDataResource
#' @description \code{writeWorkflowJobDataResource} allows to transform datasource into different formats
#'
#' @usage writeWorkflowJobDataResource(entity,config,obj,useFeatures,resourcename,useUploadSource,type)
#'
#' @param entity a entity object as read by \code{geoflow_entity} 
#' @param config a configuration object as read by \code{initWorkflow}
#' @param obj a sf file
#' @param useFeatures a boolean condition to define if features must be attach to obj file
#' @param resourcename name of data input
#' @param useUploadSource a boolean condition to define if resourcename is same as uploadSource information
#' @param type format to convert into list :"shp","dbtable 
#' 
#' @author Alexandre Bennici, \email{bennicialexandre@@gmail.com}
#' @export
#'

writeWorkflowJobDataResource <- function(entity, config,obj=NULL,useFeatures=FALSE,resourcename=NULL,useUploadSource=FALSE,type){
  config$logger.info("------------------------------------------------------------------------------")
  config$logger.info("Write data resource start")
  if(is.null(obj) && !useFeatures){
    errMsg<-"Error: specify at least an object or useFeatures = TRUE"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  if(useFeatures){
    config$logger.info("No object specified, use entity$data$features by default")
    obj<-entity$data$features
  }
  
  if(is.null(resourcename) && !useUploadSource){
    errMsg<-"Error: specify a resourcename or useUploadSource = TRUE"
    config$logger.error(errMsg)
    stop(errMsg)  
  }
  
  if(useUploadSource){
    config$logger.info("No resourcename specified, use entity$data$uploadSource by default")
    resourcename<-entity$data$uploadSource[[1]]
  }
  
  if(!type %in% c("shp","dbtable")){
    errMsg<-"Error: unrecognized type, specify a type at this list : 'shp','dbtable'"
    config$logger.error(errMsg)
    stop(errMsg)
  } 
  
  switch(type,
         "shp" = {
           
           resourcename_parts <- unlist(strsplit(resourcename, "\\."))
           if(length(resourcename_parts)>1) resourcename <- resourcename_parts[1]
           
           config$logger.info(sprintf("Format type: %s", type))
           st_write(obj = obj, paste0("./data/",resourcename,".shp"), delete_layer = TRUE)
           config$logger.info("write shp file to data job directory")
           zip::zipr(zipfile = paste0("./data/",resourcename, ".zip"), files = paste0(getwd(),"./data/",list.files(path="./data",pattern = resourcename)))
           config$logger.info("zip datafiles for server export")
           if(useFeatures){
             config$logger.info("object use to data features") 
             df<-st_read(paste0("./data/",resourcename,".shp"), quiet=TRUE) 
             if(attr(df, "sf_column")== "geometry"){
               df$the_geom <- st_geometry(df)
               attr(df, "sf_column") <- "the_geom"
               df$geometry <- NULL
             }
             entity$data$features<-df
           }
         },
         "dbtable" = {
           if(is.null(config$software$output$dbi)){
             errMsg<-"Error: no dbi output detected, branch one in the config"
             config$logger.error(errMsg)
             stop(errMsg)   
           }  
           config$logger.info(sprintf("Format type: %s", type))
           if(class(obj)[1]=="sf"){
           st_write(obj = obj, dsn = config$software$output$dbi, layer =resourcename , layer_options = 'OVERWRITE=YES')
           
           #enforce srid/geometry type in geometry_columns
           srid <- unlist(strsplit(st_crs(obj)$input, ":"))[2]
           geometryName <- attr(obj, "sf_column")
           geometryType <- unlist(strsplit(class(st_geometry(obj))[1], "sfc_"))[2]
           alter_sql <- sprintf("alter table %s alter column %s type geometry(%s, %s);", 
                                resourcename, geometryName, geometryType, srid)
           DBI::dbExecute(config$software$output$dbi, alter_sql)
           }else{
             dbWriteTable(conn=config$software$output$dbi, name =resourcename, value=obj , overwrite=TRUE)  
           }
         }
  )
  config$logger.info("Write data resource end")
  config$logger.info("------------------------------------------------------------------------------")
}

