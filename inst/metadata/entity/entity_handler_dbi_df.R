#handle_entities_dbi_df
handle_entities_dbi_df = function(handler, source, config){
  
  dbi <- config$software$input$dbi
  dbi_config <- config$software$input$dbi_config
  if(is.null(dbi)){
    stop("There is no database input software configured to handle entities from DB")
  }
  
  handle_entities_df <- source(system.file("metadata/entity", "entity_handler_df.R", package = "geoflow"))$value
  entities <- handle_entities_df(handler, source, config)
  
  enriched_entities <- lapply(entities, function(entity){
    expected_table_id = entity$identifiers$id
    table_entry_sql = sprintf("SELECT * FROM information_schema.tables WHERE table_name = '%s'", expected_table_id)
    table_entry_query = DBI::dbGetQuery(dbi, table_entry_sql)
    #check if a table is available in DB
    if(nrow(table_entry_query)>0){
      schema = table_entry_query$table_schema[1]
      entity_data = create_geoflow_data_from_dbi(dbi, schema, expected_table_id)
      entity$setData(entity_data)
      
      #associated styles
      if(DBI::dbExistsTable(dbi, "layer_styles")){
        #assume this is a special table
        styles_sql = sprintf("select * from layer_styles where f_table_schema='%s' and f_table_name='%s'", 
                             schema, expected_table_id)
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
      
    }else{
      warnMsg = sprintf("No DB table named '%s' available, skipping data enrichment from DB!", expected_table_id)
      config$logger.warn(warnMsg)
    }
    return(entity)
  })
  
  return(enriched_entities)
}