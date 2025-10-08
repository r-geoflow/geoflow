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
      
      #feth layer_styles (if any) from DBI
      entity = fetch_layer_styles_from_dbi(entity, dbi, schema, expected_table_id)
    }else{
      warnMsg = sprintf("No DB table named '%s' available, skipping data enrichment from DB!", expected_table_id)
      config$logger$WARN(warnMsg)
    }
    return(entity)
  })
  
  return(enriched_entities)
}
