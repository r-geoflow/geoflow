#handle_entities_dbi_geometry_columns
handle_entities_dbi_geometry_columns <- function(config, source, handle = TRUE){
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
  #getDBTableColumnComment
  getDBTableColumnComment = function(dbi, schema, table, column_index){
    get_comment_sql = sprintf("select col_description('%s.%s'::regClass, %s)",
                              schema, table, column_index)
    get_comment = DBI::dbGetQuery(dbi, get_comment_sql)
    return(get_comment$col_description)
  }
  
  #entities
  entities = lapply(1:nrow(db_tables), function(i){
    db_table = db_tables[i,]
    
    entity = geoflow_entity$new()
    entity$addDate("creation", Sys.time())
    
    #base fields
    id = paste0(source, "_", db_table$f_table_schema,"_", db_table$f_table_name)
    entity$setIdentifier(key = "id", id)
    tbl_comment = getDBTableComment(dbi, db_table$f_table_schema, db_table$f_table_name)
    if(is.na(tbl_comment)) tbl_comment = db_table$f_table_name
    entity$setTitle(key = "title", tbl_comment)
    entity$setDescription(key = "abstract", tbl_comment)
    entity$setType(key = "generic", "dataset")
    
    #data
    entity_data = geoflow_data$new()
    sql = sprintf("select * from %s.%s", db_table$f_table_schema, db_table$f_table_name)
    entity_data$setSourceSql(sql)
    entity_data$setSourceType("dbquery")
    entity_data$setSpatialRepresentationType("vector")
    #data/feature type
    fto = geoflow_featuretype$new(id = id)
    data_sample = sf::st_read(dbi, query = paste(sql, "limit 1;"))
    for(colname in colnames(data_sample)){
      col_idx = which(colnames(data_sample) == colname)
      col_comment = getDBTableColumnComment(dbi, db_table$f_table_schema, db_table$f_table_name, col_idx)
      if(is.na(col_comment)) col_comment = colname
      
      ftm = geoflow_featuremember$new(
        type = if(is(data_sample[[colname]], "character")) "attribute" else "variable",
        code = colname,
        name = col_comment,
        def = col_comment,
        defSource = NA,
        minOccurs = 0,
        maxOccurs = 1,
        uom = NA
      )
      fto$addMember(ftm)
    }
    entity_data$setFeatureTypeObj(fto)
  
    entity$setData(entity_data)
        
    return(entity)
  })
  return(entities)
}