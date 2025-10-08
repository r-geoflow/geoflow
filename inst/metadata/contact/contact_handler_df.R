#handle_contacts_df
handle_contacts_df <- function(handler, source, config){
  
  if(!is(source, "data.frame")){
    errMsg <- "Error in 'handle_contact_df': source parameter should be an object of class 'data.frame'"
    config$logger$ERROR(errMsg)
    stop(errMsg)
  }
  
  #validation
  config$logger$INFO("Validating contacts")
  validation_report <- geoflow::geoflow_validator_contacts$new(source = source)$validate_content()
  if(is.null(validation_report)){
    errMsg <- "Error of metadata structure for contacts"
    config$logger$ERROR(errMsg)
    stop(errMsg)
  }
  if(nrow(validation_report)==0){
    config$logger$INFO("No validation issue detected!")
  }else{
    config$logger$INFO("Validation issues -->")
    print(validation_report)
    if(any(validation_report$type == "ERROR")){
      errMsg <- "At least one error of metadata syntax has been detected, aborting..."
      config$logger$ERROR(errMsg)
      stop(errMsg)
    }
  }
  
  contacts <- list()
  rowNum <- nrow(source)
  config$logger$INFO("Parsing %s contacts from tabular source", rowNum)
  for(i in 1:rowNum){
    source_contact <- source[i,]
    contact <- geoflow::geoflow_contact$new()
    
    #identifier
    id <- geoflow::sanitize_str(source_contact[,"Identifier"])
    if(!is.na(id)){
      config$logger$WARN("Use 'Identifier' column as contact Ids. Make sure to use these identifiers in your dataset contact references")
      identifiers <- geoflow::extract_cell_components(id)
      
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
          config$logger$WARN("Warning: Empty contact id will be ignored!")
        }else{
          contact$setIdentifier(key = identifier_key, identifier_value)
        }
      }))
    }else{
      config$logger$WARN("Use 'Email' column as contact Ids. Make sure to upgrade your 'contacts' table to include contact ids in the 'Identifier' column")
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

