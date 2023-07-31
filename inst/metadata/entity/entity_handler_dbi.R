#handle_entities_dbi
handle_entities_dbi <- function(config, source, handle = TRUE){
  dbi <- config$software$input$dbi
  dbi_config <- config$software$input$dbi_config
  if(is.null(dbi)){
    stop("There is no database input software configured to handle entities from DB")
  }
  
  #db source
  is_default_source = FALSE
  query_all_tables = FALSE
  is_query <- if(!is.null(source)) startsWith(tolower(source), "select ") else FALSE
  if(is_query){
    source <- try(DBI::dbGetQuery(dbi, source), silent = TRUE)
    if(is(source,"try-error")){
      errMsg <- sprintf("Error while trying to execute DB query '%s'.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }else{
    if(is.null(source) | (!is.null(source) && source == "")){
      source = if(!is.null(dbi_config$properties$metadata_table) && nzchar(dbi_config$properties$metadata_table)){
        dbi_config$properties$metadata_table
      }else{
        "public.metadata"
      }
      config$logger.info(sprintf("Source is null or empty. Use default metadata table '%s'", source))
      is_default_source = TRUE
    }
    source <- try(DBI::dbGetQuery(dbi, sprintf("select * from %s", source)), silent = TRUE)
    if(is(source,"try-error")){
      if(is_default_source){
        query_all_tables <- TRUE
        warnMsg <- sprintf("Error while trying to read DB default table '%s' (table not in DB).", source)
        config$logger.warn(warnMsg)
      }else{
        errMsg <- sprintf("Error while trying to read DB table/view '%s'. Check if it exists in DB.", source)
        config$logger.error(errMsg)
        stop(errMsg)
      }
    }
  }
  if(!handle) return(source)
  
  #apply handlers
  entities <- if(query_all_tables){
    config$logger.info("Switch to DBI tables entity handler")
    #use a generic DBI geometry columns 
    source = dbi_config$parameters$dbname
    handle_entities_dbi_geometry_columns <-  source(system.file("metadata/entity", "entity_handler_dbi_geometry_columns.R", package = "geoflow"))$value
    handle_entities_dbi_geometry_columns(config, source)
  }else{
    config$logger.info("Use default tabular entity handler")
    #use the df entity handler based on the SQL query/table specified as source
    handle_entities_df <- source(system.file("metadata/entity", "entity_handler_df.R", package = "geoflow"))$value
    handle_entities_df(config, source)
  }
  return(entities)
}