#handle_dictionary_gsheet
handle_dictionary_gsheet <- function(handler, source, config, handle = TRUE){
  
  #read gsheet URL
  source <- read.csv(text = gsheet::gsheet2text(source))
  if(!handle) return(source)
  
  #apply generic handler
  handle_dictionary_df <- source(system.file("metadata/dictionary", "dictionary_handler_df.R", package = "geoflow"))$value
  dictionary <- handle_dictionary_df(handler, source, config)
  return(dictionary)
}