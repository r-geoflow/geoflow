#handle_dictionary_csv
handle_dictionary_csv <- function(config, source, handle = TRUE){
  
  #read csv TODO -> options management: sep, encoding etc
  #source <- read.csv(source)
  source <- as.data.frame(readr::read_csv(source, guess_max = 0))
  if(!handle) return(source)
  
  #apply generic handler
  handle_dictionary_df <- source(system.file("metadata/dictionary", "dictionary_handler_df.R", package = "geoflow"))$value
  dictionary <- handle_dictionary_df(config, source)
  return(dictionary)
}