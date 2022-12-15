#handle_entities_csv
handle_entities_csv <- function(config, source, handle = TRUE){
  
  #read csv TODO -> options management: sep, encoding etc
  #source <- read.csv(source,stringsAsFactors = F)
  source <- as.data.frame(readr::read_csv(source, guess_max = 0))
  if(!handle) return(source)
  
  #apply generic handler
  handle_entities_df <- source(system.file("metadata/entity", "entity_handler_df.R", package = "geoflow"))$value
  entities <- handle_entities_df(config, source)
  return(entities)
}