#handle_dictionary_excel
handle_dictionary_excel <- function(config, source, handle = TRUE){
  
  #read excel TODO -> options management: sep, encoding etc
  source <- as.data.frame(readxl::read_excel(source))
  if(!handle) return(source)
  
  #apply generic handler
  handle_dictionary_df <- source(system.file("metadata/dictionary", "dictionary_handler_df.R", package = "geoflow"))$value
  dictionary <- handle_dictionary_df(config, source)
  return(dictionary)
}