#' @name writeWorkflowJobDataResource
#' @aliases writeWorkflowJobDataResource
#' @title writeWorkflowJobDataResource
#' @description \code{writeWorkflowJobDataResource} allows to transform datasource into different formats
#'
#' @usage writeWorkflowJobDataResource(obj,type)
#'
#' @param entity a entity object as read by \code{geoflow_entity} 
#' @param config a configuration object as read by \code{initWorkflow}
#' @param obj a sf file
#' @param resourcename name of data input
#' @param type format to convert into list :"shp","dbtable 
#' 
#' @author Alexandre Bennici, \email{bennicialexandre@@gmail.com}
#' @export
#'

writeWorkflowJobDataResource <- function(entity, config,obj=NULL,resourcename,type){
  if(!type%in%c("shp","dbtable")) config$logger.error('type must be in : "shp" , "dbtable"')
if(is.null(obj))obj=entity$data$features
  resourcename_parts <- unlist(strsplit(resourcename, "\\."))
  if(length(resourcename_parts)>1) resourcename <- resourcename_parts[1]
  switch(type,
         "shp"={
           config$logger.info(sprintf("Format type: %s", type))
           st_write(obj = obj, paste0("./data/",resourcename,".shp"), delete_layer = TRUE)
           config$logger.info("write shp file to data job directory")
           zip::zipr(zipfile = paste0("./data/",resourcename, ".zip"), files = paste0(getwd(),"./data/",list.files(path="./data",pattern = resourcename)))
           config$logger.info("zip datafiles for server export")
           },
          "dbtable"={
           config$logger.info(sprintf("Format type: %s", type))
           st_write(obj = obj, dsn = config$software$output$dbi, layer =resourcename , layer_options = 'OVERWRITE=YES')
           }
        )
}

