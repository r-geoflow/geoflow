#handle_contacts_df
handle_contacts_df <- function(config, source){
  
  if(!is(source, "data.frame")){
    errMsg <- "Error in 'handle_contact_df': source parameter should be an object of class 'data.frame'"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #validation
  config$logger.info("Validating contacts")
  validation_report <- geoflow_validator_contacts$new(source = source)$validate_content()
  if(is.null(validation_report)){
    errMsg <- "Error of metadata structure for contacts"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  if(nrow(validation_report)==0){
    config$logger.info("No validation issue detected!")
  }else{
    config$logger.info("Validation issues -->")
    print(validation_report)
    if(any(validation_report$type == "ERROR")){
      errMsg <- "At least one error of metadata syntax has been detected, aborting..."
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }
  
  contacts <- list()
  rowNum <- nrow(source)
  config$logger.info(sprintf("Parsing %s contacts from tabular source", rowNum))
  for(i in 1:rowNum){
    source_contact <- source[i,]
    contact <- geoflow_contact$new()
    
    #identifier
    id <- sanitize_str(source_contact[,"Identifier"])
    if(!is.na(id)){
      config$logger.warn("Use 'Identifier' column as contact Ids. Make sure to use these identifiers in your dataset contact references")
      identifiers <-extract_cell_components(id)
      
      invisible(lapply(identifiers, function(identifier){
        identifier_splits <- unlist(strsplit(identifier, ":"))
        identifier_key <- "id"
        identifier_value <- ""
        if(length(identifier_splits)>1){
          identifier_key <- identifier_splits[1]
          identifier_value <- identifier_splits[2]
        }else{
          identifier_value <- identifier_splits
        }
        if(is.na(identifier_value)){
          config$logger.warn("Warning: Empty contact id will be ignored!")
        }else{
          contact$setIdentifier(key = identifier_key, identifier_value)
        }
      }))
    }else{
      config$logger.warn("Use 'Email' column as contact Ids. Make sure to upgrade your 'contacts' table to include contact ids in the 'Identifier' column")
      contact$setIdentifier("id", source_contact[,"Email"])
    }
    
    contact$setEmail(source_contact[,"Email"])
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
    if(!is.na(source_contact[,"WebsiteUrl"])& source_contact[,"WebsiteUrl"]!="") contact$setWebsiteUrl(source_contact[,"WebsiteUrl"])
    contact$setWebsiteName(source_contact[,"WebsiteName"])
    
    contacts <- c(contacts, contact)
  }
  attr(contacts, "source") <- source
  return(contacts)
}

#handle_contacts_gsheet
handle_contacts_gsheet <- function(config, source, handle = TRUE){
  
  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  if(!handle) return(source)
  
  #apply generic handler
  contacts <- handle_contacts_df(config, source)
  return(contacts)
}

#handle_contacts_csv
handle_contacts_csv <- function(config, source, handle = TRUE){
  
  #read csv TODO -> options management: sep, encoding etc
  source <- read.csv(source)
  if(!handle) return(source)
  
  #apply generic handler
  contacts <- handle_contacts_df(config, source)
  return(contacts)
}

#handle_contacts_excel
handle_contacts_excel <- function(config, source, handle = TRUE){
  
  #read excel TODO -> options management: sep, encoding etc
  source <- as.data.frame(readxl::read_excel(source))
  if(!handle) return(source)
  
  #apply generic handler
  contacts <- handle_entities_df(config, source)
  return(contacts)
}

#handle_contacts_dbi
handle_contacts_dbi <- function(config, source, handle = TRUE){
  dbi <- config$software$input$dbi
  if(is.null(dbi)){
    stop("There is no database input software configured to handle contacts from DB")
  }
  
  #db source
  is_query <- startsWith(tolower(source), "select ")
  if(is_query){
    source <- try(DBI::dbGetQuery(dbi, source))
    if(class(source)=="try-error"){
      errMsg <- sprintf("Error while trying to execute DB query '%s'.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }else{
    source <- try(DBI::dbReadTable(dbi, source))
    if(class(source)=="try-error"){
      errMsg <- sprintf("Error while trying to read DB table/view '%s'. Check if it exists in DB.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }
  if(!handle) return(source)
  
  #apply generic handler
  contacts <- handle_contacts_df(config, source)
  return(contacts)
  
}
