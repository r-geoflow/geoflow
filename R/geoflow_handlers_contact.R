#' handle_contacts_df
#' @export
handle_contacts_df <- function(config, source){
  
  if(!is(source, "data.frame")){
    errMsg <- "Error in 'handle_contact_df': source parameter should be an object of class 'data.frame'"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  contacts <- list()
  rowNum <- nrow(source)
  config$logger.info(sprintf("Parsing %s contacts from tabular source", rowNum))
  for(i in 1:rowNum){
    source_contact <- source[i,]
    contact <- geoflow_contact$new()
    contact$setEmail(source_contact[,"Email"])
    contact$setIndividualName(paste(source_contact[,"FirstName"], source_contact[,"LastName"]))
    contact$setOrganizationName(source_contact[,"OrganizationName"])
    contact$setPositionName(source_contact[,"PositionName"])
    contact$setPostalAddress(source_contact[,"PostalAddress"])
    contact$setPostalCode(source_contact[,"PostalCode"])
    contact$setCity(source_contact[,"City"])
    contact$setCountry(source_contact[,"Country"])
    contact$setVoice(source_contact[,"Voice"])
    contact$setFacsimile(source_contact[,"Facsimile"])
    contact$setWebsite(source_contact[,"Website"])
    
    contacts <- c(contacts, contact)
  }
  return(contacts)
}

#' handle_contacts_gsheet
#' @export
handle_contacts_gsheet <- function(config, source){
  
  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  
  #apply generic handler
  contacts <- handle_contacts_df(config, source)
  return(contacts)
}

#' handle_contacts_csv
#' @export
handle_contacts_csv <- function(config, source){
  
  #read csv TODO -> options management: sep, encoding etc
  source <- read.csv(source)
  
  #apply generic handler
  contacts <- handle_contacts_df(config, source)
  return(contacts)
}
