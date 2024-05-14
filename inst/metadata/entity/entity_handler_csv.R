#handle_entities_csv
handle_entities_csv <- function(handler, source, config, handle = TRUE){
  
  #read csv TODO -> options management: sep, encoding etc
  #source <- read.csv(source,stringsAsFactors = F)
  guess_max = handler$getOption("guess_max")
  if(is.null(guess_max)) guess_max = 0
  source <- as.data.frame(readr::read_csv(source, guess_max = guess_max))
  if(!handle) return(source)
  
  #apply generic handler
  handler_script = if(handler$getOption("enrich_from_dbi")) "entity_handler_dbi_df.R" else "entity_handler_df.R"
  handle_entities_df <- source(system.file("metadata/entity", handler_script, package = "geoflow"))$value
  entities <- handle_entities_df(handler, source, config)
  return(entities)
}