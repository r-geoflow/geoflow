#handle_contacts_ocs
handle_contacts_ocs <- function(config, source, handle = TRUE){
  
  if(!requireNamespace("ocs4R", quietly = TRUE)){
    stop("The OCS handler requires the 'ocs4R' package")
  }
  
  ocs <- config$software$input$ocs
  if(is.null(ocs)){
    stop("There is no OCS input software configured to handle contacts from an OCS service endpoint")
  }
  
  contacts_file <- ocs$downloadFile(relPath = dirname(source), filename = basename(source), outdir = tempdir())
  
  contacts <- switch(mime::guess_type(contacts_file),
                     "text/csv" = {
                       handle_contacts_csv <- source(system.file("metadata/contact", "contact_handler_csv.R", package = "geoflow"))$value
                       handle_contacts_csv(config = config, source = contacts_file, handle = handle)
                      },
                     "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = {
                       handle_contacts_excel <- source(system.file("metadata/contact", "contact_handler_excel.R", package = "geoflow"))$value
                       handle_contacts_excel(config = config, source = contacts_file, handle = handle)
                     }
  )
  return(contacts)
}