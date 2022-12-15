#handle_entities_thredds_excel
handle_entities_thredds_excel <- function(config, source, handle = TRUE){
  
  #read excel TODO -> options management: sep, encoding etc
  source <- as.data.frame(readxl::read_excel(source))
  if(!handle) return(source)
  
  #apply generic handler
  handle_entities_thredds_df <- source(system.file("metadata/entity", "entity_handler_thredds_df.R", package = "geoflow"))$value
  entities <- handle_entities_thredds_df(config, source)
  return(entities)
}