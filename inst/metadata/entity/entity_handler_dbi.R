#handle_entities_dbi
handle_entities_dbi <- function(config, source, handle = TRUE){
  dbi <- config$software$input$dbi
  if(is.null(dbi)){
    stop("There is no database input software configured to handle entities from DB")
  }
  
  #db source
  is_query <- startsWith(tolower(source), "select ")
  if(is_query){
    source <- try(DBI::dbGetQuery(dbi, source))
    if(is(source,"try-error")){
      errMsg <- sprintf("Error while trying to execute DB query '%s'.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }else{
    source <- try(DBI::dbGetQuery(dbi, sprintf("select * from %s", source)))
    if(is(source,"try-error")){
      errMsg <- sprintf("Error while trying to read DB table/view '%s'. Check if it exists in DB.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }
  if(!handle) return(source)
  
  #apply generic handler
  handle_entities_df <- source(system.file("metadata/entity", "entity_handler_df.R", package = "geoflow"))$value
  entities <- handle_entities_df(config, source)
  return(entities)
}