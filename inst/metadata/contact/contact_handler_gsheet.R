#handle_contacts_gsheet
handle_contacts_gsheet <- function(handler, source, config, handle = TRUE){
  
  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  if(!handle) return(source)
  
  #apply generic handler
  handle_contacts_df <- source(system.file("metadata/contact", "contact_handler_df.R", package = "geoflow"))$value
  contacts <- handle_contacts_df(handler, source, config)
  return(contacts)
}