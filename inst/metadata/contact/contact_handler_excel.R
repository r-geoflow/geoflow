#handle_contacts_excel
handle_contacts_excel <- function(config, source, handle = TRUE){
  
  isSourceUrl <- regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", source) > 0
  if(isSourceUrl){
    source_local_name <- "contacts.xlsx"
    if(endsWith(source, ".xlsx")) source_local_name <- basename(source)
    source_local <- file.path(tempdir(), source_local_name)
    download.file(url = source, destfile = source_local, mode = "wb")
    source <- source_local
  }
  
  #read excel TODO -> options management: sep, encoding etc
  source <- as.data.frame(readxl::read_excel(source))
  if(!handle) return(source)
  
  #apply generic handler
  handle_contacts_df <- source(system.file("metadata/contact", "contact_handler_df.R", package = "geoflow"))$value
  contacts <- handle_contacts_df(config, source)
  return(contacts)
}