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
    contact$setEmail(source_contact[,"email"])
    contact$setIndividualName(paste(source_contact[,"firstName"], source_contact[,"lastName"]))
    contact$setOrganizationName(source_contact[,"organizationName"])
    contact$setPositionName(source_contact[,"positionName"])
    contact$setPostalAddress(source_contact[,"postalAddress"])
    contact$setPostalCode(source_contact[,"postalCode"])
    contact$setCity(source_contact[,"city"])
    contact$setCountry(source_contact[,"country"])
    contact$setVoice(source_contact[,"voice"])
    contact$setFacsimile(source_contact[,"facsimile"])
    contact$setWebsite(source_contact[,"website"])
    
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
