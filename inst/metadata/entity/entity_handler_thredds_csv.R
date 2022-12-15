#handle_entities_thredds_csv
handle_entities_thredds_csv <- function(config, source, handle = TRUE){
  
  #read csv TODO -> options management: sep, encoding etc
  source <- read.csv(source,stringsAsFactors = F)
  if(!handle) return(source)
  
  #apply generic handler
  handle_entities_thredds_df <- source(system.file("metadata/entity", "entity_handler_thredds_df.R", package = "geoflow"))$value
  entities <- handle_entities_thredds_df(config, source)
  return(entities)
}