#handle_entities_gsheets
handle_entities_gsheet <- function(handler, source, config, handle = TRUE){
  
  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  if(!handle) return(source)
  
  #apply generic handler
  handle_entities_df <- source(system.file("metadata/entity", "entity_handler_df.R", package = "geoflow"))$value
  entities <- handle_entities_df(handler, source, config)
  return(entities)
}