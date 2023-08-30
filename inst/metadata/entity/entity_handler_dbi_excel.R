#handle_entities_dbi_excel
handle_entities_dbi_excel <- function(handler, source, config, handle = TRUE){
  
  #read excel TODO -> options management: sep, encoding etc
  source <- as.data.frame(readxl::read_excel(source))
  if(!handle) return(source)
  
  #apply generic handler
  handle_entities_dbi_df <- source(system.file("metadata/entity", "entity_handler_dbi_df.R", package = "geoflow"))$value
  entities <- handle_entities_dbi_df(handler, source, config)
  return(entities)
}