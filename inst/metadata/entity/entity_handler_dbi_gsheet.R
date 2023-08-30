#handle_entities_dbi_gsheet
handle_entities_dbi_gsheet <- function(handler, source, config, handle = TRUE){
  
  #read excel TODO -> options management: sep, encoding etc  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  if(!handle) return(source)
  
  #apply generic handler
  handle_entities_dbi_df <- source(system.file("metadata/entity", "entity_handler_dbi_df.R", package = "geoflow"))$value
  entities <- handle_entities_dbi_df(handler, source, config)
  return(entities)
}