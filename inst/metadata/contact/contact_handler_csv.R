#handle_contacts_csv
handle_contacts_csv <- function(config, source, handle = TRUE){
  
  #read csv TODO -> options management: sep, encoding etc
  #source <- read.csv(source)
  source <- as.data.frame(readr::read_csv(source, guess_max = 0))
  if(!handle) return(source)
  
  #apply generic handler
  handle_contacts_df <- source(system.file("metadata/contact", "contact_handler_df.R", package = "geoflow"))$value
  contacts <- handle_contacts_df(config, source)
  return(contacts)
}