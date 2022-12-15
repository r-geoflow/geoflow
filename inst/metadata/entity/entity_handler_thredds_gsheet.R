#handle_entities_thredds_gsheet
handle_entities_thredds_gsheet <- function(config, source, handle = TRUE){
  
  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  if(!handle) return(source)
  
  #apply generic handler
  handle_entities_thredds_df <- source(system.file("metadata/entity", "entity_handler_thredds_df.R", package = "geoflow"))$value
  entities <- handle_entities_thredds_df(config, source)
  return(entities)
}