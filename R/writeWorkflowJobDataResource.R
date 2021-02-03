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
#' @param createIndexes a boolean condition for possibility to create indexes for each column
#' @param overwrite a boolean condition for writing to DB. Default is \code{TRUE}
#' @param append a boolean condition for appending to DB existing table. Default is \code{FALSE}
#' @param chunk.size an object of class \code{integer} giving the size of chunks to apply for DB upload.
#' Default is equal to \code{OL}, meaning DB upload will be done without chunking.
#' @param type format to convert into list :"shp","dbtable" 
#' 
#' @author Alexandre Bennici, \email{bennicialexandre@@gmail.com}
#' @export
#'

writeWorkflowJobDataResource <- function(entity, config, obj=NULL,
                                         useFeatures=FALSE, resourcename=NULL, useUploadSource=FALSE,
                                         createIndexes=FALSE, overwrite=TRUE, append = FALSE, chunk.size = 0L, 
                                         type){
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
             #sf upload
             if(chunk.size>0){
               chunks <- split(obj, ceiling(seq_along(1:nrow(obj))/chunk.size))
               config$logger.info(sprintf("Upload DB data by chunks [nb of chunks: %s]", length(chunks)))
               
               #1st chunk with overwrite param applied
               config$logger.info(sprintf("Upload data to DB: chunk 1 of %s", length(chunks)))
               st_write(obj = chunks[[1]], dsn = config$software$output$dbi, layer =resourcename , 
                        layer_options = paste0('OVERWRITE=',ifelse(overwrite,'YES','NO')),
                        append = append)
               
               #then apply to other chunks with fixed overwrite=NO
               if(length(chunks)>1){
                 chunk_idx = 2
                 outchunk = lapply(chunks[2:length(chunks)], function(chunk){
                   config$logger.info(sprintf("Upload data to DB: chunk %s of %s", chunk_idx, length(chunks)))
                   st_write(obj = chunk, dsn = config$software$output$dbi, layer = resourcename, 
                            append = TRUE)
                   chunk_idx <<- chunk_idx+1
                 })
               }
             }else{
               #no chunking
               config$logger.info("Upload data to DB as single chunk")
               st_write(obj = obj, dsn = config$software$output$dbi, layer =resourcename , 
                        layer_options = paste0('OVERWRITE=',ifelse(overwrite,'YES','NO')),
                        append = append)
             }
             
             #enforce srid/geometry type in geometry_columns
             srid <- st_crs(obj, parameters = TRUE)$epsg
             if(is.null(srid)){
              srid <- 4326
              st_crs(obj) <- srid
             }
             geometryName <- attr(obj, "sf_column")
             geometryType <- unlist(strsplit(class(st_geometry(obj))[1], "sfc_"))[2]
             if(!is.na(srid)){
              alter_sql <- sprintf("alter table %s alter column %s type geometry(%s, %s);", 
                                  resourcename, geometryName, geometryType, srid)
              DBI::dbExecute(config$software$output$dbi, alter_sql)
             }
             #create index for each colunm except geometry
             if(createIndexes){
               for (column_name in setdiff(names(obj),geometryName)){
                 config$logger.info(sprintf("Drop index for column : %s",column_name))
                 drop_index_sql <- sprintf("DROP INDEX %s_%s_idx;", resourcename, column_name)
                 try(DBI::dbExecute(config$software$output$dbi, drop_index_sql), silent = TRUE)
               }
               for (column_name in setdiff(names(obj),geometryName)){
                 config$logger.info(sprintf("Indexes created for column : %s",column_name))
                 create_index_sql <- sprintf("CREATE INDEX %s_%s_idx  ON %s  (%s);", resourcename, column_name, resourcename, column_name)
                 DBI::dbExecute(config$software$output$dbi, create_index_sql)
               }
             }
           }else{
             if(chunk.size>0){
               chunks <- split(obj, ceiling(seq_along(1:nrow(obj))/chunk.size))
               config$logger.info(sprintf("Upload DB data by chunks [nb of chunks: %s]", length(chunks)))
               
               #1st chunk with overwrite param applied
               config$logger.info(sprintf("Upload data to DB: chunk 1 of %s", length(chunks)))
               dbWriteTable(conn=config$software$output$dbi, name =resourcename, value=chunks[[1]],
                            overwrite = overwrite, append = append)
               
               #then apply to other chunks with fixed overwrite=NO
               if(length(chunks)>1){
                 chunk_idx = 2
                 outchunk = lapply(chunks[2:length(chunks)], function(chunk){
                   config$logger.info(sprintf("Upload data to DB: chunk %s of %s", chunk_idx, length(chunks)))
                   dbWriteTable(conn=config$software$output$dbi, name =resourcename, value=chunk, append = TRUE)
                   chunk_idx <<- chunk_idx+1
                 })
               }
             }else{
               #no chunking
               config$logger.info("Upload data to DB as single chunk")
               dbWriteTable(conn=config$software$output$dbi, name =resourcename, value=obj,
                            overwrite = overwrite, append = append)
             }
             
             #create index for each column
             if(createIndexes){
               for (column_name in names(obj)){
                 config$logger.info(sprintf("Drop index for column : %s",column_name))
                 drop_index_sql <- sprintf("DROP INDEX %s_%s_idx;", resourcename, column_name)
                 try(DBI::dbExecute(config$software$output$dbi, drop_index_sql), silent = TRUE)
               }
               for (column_name in names(obj)){
                 config$logger.info(sprintf("Create index for column : %s",column_name))
                 create_index_sql <- sprintf("CREATE INDEX %s_%s_idx  ON %s  (%s);", resourcename, column_name, resourcename, column_name)
                 DBI::dbExecute(config$software$output$dbi, create_index_sql)
               }
             }
           }
         }
  )
  config$logger.info("Write data resource end")
  config$logger.info("------------------------------------------------------------------------------")
}

