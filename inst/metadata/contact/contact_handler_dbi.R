#handle_contacts_dbi
handle_contacts_dbi <- function(handler, source, config, handle = TRUE){
  dbi <- config$software$input$dbi
  if(is.null(dbi)){
    stop("There is no database input software configured to handle contacts from DB")
  }
  
  #db source
  is_query <- startsWith(tolower(source), "select ")
  if(is_query){
    source <- try(DBI::dbGetQuery(dbi, source))
    if(is(source,"try-error")){
      errMsg <- sprintf("Error while trying to execute DB query '%s'.", source)
      config$logger$ERROR(errMsg)
      stop(errMsg)
    }
  }else{
    source <- try(DBI::dbReadTable(dbi, source))
    if(is(source,"try-error")){
      errMsg <- sprintf("Error while trying to read DB table/view '%s'. Check if it exists in DB.", source)
      config$logger$ERROR(errMsg)
      stop(errMsg)
    }
  }
  if(!handle) return(source)
  
  #apply generic handler
  handle_contacts_df <- source(system.file("metadata/contact", "contact_handler_df.R", package = "geoflow"))$value
  contacts <- handle_contacts_df(handler, source, config)
  return(contacts)
  
}
