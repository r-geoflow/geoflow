#handle_entities_dbi
handle_entities_dbi <- function(config, source, handle = TRUE){
  dbi <- config$software$input$dbi
  dbi_config <- config$software$input$dbi_config
  if(is.null(dbi)){
    stop("There is no database input software configured to handle entities from DB")
  }
  
  db_tables_query = sprintf("select * from geometry_columns where f_table_catalog = '%s'", source)
  db_tables <- try(DBI::dbGetQuery(dbi, db_tables_query))
  if(is(db_tables,"try-error")){
    errMsg <- sprintf("Error while trying to execute DB query '%s'.", db_tables_query)
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #DB comment utils
  #getDBTableComment
  getDBTableComment = function(dbi, schema, table){
    get_comment_sql = sprintf("select obj_description('%s.%s'::regclass, 'pg_class')",
                              schema, table)
    get_comment = DBI::dbGetQuery(dbi, get_comment_sql)
    return(get_comment$obj_description)
  }
  
  #entities
  entities = lapply(1:nrow(db_tables), function(i){
    db_table = db_tables[i,]
    entity = geoflow_entity$new()
    id = paste0(source, "_", db_table$f_table_schema,"_", db_table$f_table_name)
    entity$setIdentifier(key = "id", id)
    tbl_comment = getDBTableComment(dbi, db_table$f_table_schema, db_table$f_table_name)
    if(is.na(tbl_comment)) tbl_comment = db_table$f_table_name
    entity$setTitle(key = "title", tbl_comment)
    entity$setDescription(key = "abstract", tbl_comment)
    entity$setType(key = "generic", "dataset")
    entity_data = geoflow_data$new()
    entity_data$setSourceSql(sprintf("select * from %s.%s", db_table$f_table_schema, db_table$f_table_name))
    entity_data$setSourceType("dbquery")
    entity_data$setSpatialRepresentationType("vector")
    entity$setData(entity_data)
        
    return(entity)
  })
  return(entities)
}