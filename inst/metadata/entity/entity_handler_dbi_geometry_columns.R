#handle_entities_dbi_geometry_columns
handle_entities_dbi_geometry_columns <- function(handler, source, config, handle = TRUE){
  dbi <- config$software$input$dbi
  dbi_config <- config$software$input$dbi_config
  dbi_user <- dbi_config$parameters$user
  if(is.null(dbi)){
    stop("There is no database input software configured to handle entities from DB")
  }
  
  check_priv_geometry_columns_query <- sprintf("SELECT * FROM information_schema.table_privileges 
                                                WHERE table_name = 'geometry_columns' AND
                                                privilege_type = 'SELECT' AND
                                                grantee IN('%s','PUBLIC')", dbi_user)
  check_priv_geometry_columns = try(DBI::dbGetQuery(dbi, check_priv_geometry_columns_query))
  if(nrow(check_priv_geometry_columns)==0){
    warnMsg = sprintf("The 'geometry_columns' table is not granted for SELECT for public or user '%s'. An empty list of entities is returned!", dbi_user)
    config$logger.warn(warnMsg)
    return(list())
  }
  
  db_tables_query = sprintf("SELECT geo.* FROM geometry_columns AS geo 
                             LEFT JOIN information_schema.table_privileges AS priv on geo.f_table_name = priv.table_name 
                             WHERE geo.f_table_catalog = '%s' AND priv.privilege_type = 'SELECT' AND priv.grantee IN('%s','PUBLIC')",
                            source, dbi_user)
  db_tables <- try(DBI::dbGetQuery(dbi, db_tables_query))
  if(is(db_tables,"try-error")){
    errMsg <- sprintf("Error while trying to execute DB query '%s'.", db_tables_query)
    config$logger.error(errMsg)
    stop(errMsg)
  }
  if(nrow(db_tables)==0){
    warnMsg = sprintf("No table granted for SELECT for public or user '%s'. An empty list of entities is returned!", dbi_user)
    config$logger.warn(warnMsg)
    return(list())
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
    entity_data = create_geoflow_data_from_dbi(dbi, db_table$f_table_schema, db_table$f_table_name)
    entity$setData(entity_data)
    
    #associated styles
    if(DBI::dbExistsTable(dbi, "layer_styles")){
      #assume this is a special table
      styles_sql = sprintf("select * from layer_styles where f_table_schema='%s' and f_table_name='%s'", 
                           db_table$f_table_schema, db_table$f_table_name)
      styles = DBI::dbGetQuery(dbi, statement = styles_sql)
      if(nrow(styles)>0){
        styles[order(styles$useasdefault,decreasing = T),] #make sure we list the default one first
        #add style names in geoflow_data
        for(i in 1:nrow(styles)){
          style = styles[i,]
          entity$data$addStyle(style$stylename)
        }
        #add style defs as entity resource to delegate copy after entity dir is created
        entity$addResource("layer_styles", styles)
      }
    }
        
    return(entity)
  })
  return(entities)
}