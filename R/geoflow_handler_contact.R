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
    contact$setId(tolower(source_contact[,"Email"]))
    contact$setEmail(tolower(source_contact[,"Email"]))
    contact$setFirstName(source_contact[,"FirstName"])
    contact$setLastName(source_contact[,"LastName"])
    contact$setOrganizationName(source_contact[,"OrganizationName"])
    contact$setPositionName(source_contact[,"PositionName"])
    contact$setPostalAddress(source_contact[,"PostalAddress"])
    contact$setPostalCode(source_contact[,"PostalCode"])
    contact$setCity(source_contact[,"City"])
    contact$setCountry(source_contact[,"Country"])
    contact$setVoice(source_contact[,"Voice"])
    contact$setFacsimile(source_contact[,"Facsimile"])
    contact$setWebsiteUrl(source_contact[,"WebsiteUrl"])
    contact$setWebsiteName(source_contact[,"WebsiteName"])
    
    srcId <- source_contact[,"Identifier"]
    if(!is.na(srcId)){
      identifiers <- unlist(strsplit(sanitize_str(srcId), ";"))
      if(length(identifiers)>0){
        invisible(lapply(identifiers, function(identifier){
          id_obj <- geoflow_kvp$new(str = identifier)
          contact$addIdentifier(id_obj)
        }))
      }
    }
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

#' handle_contacts_excel
#' @export
handle_contacts_excel <- function(config, source){
  
  #read excel TODO -> options management: sep, encoding etc
  source <- as.data.frame(readxl::read_excel(source))
  
  #apply generic handler
  contacts <- handle_entities_df(config, source)
  return(contacts)
}

