#handle_entities_ocs
handle_entities_ocs <- function(handler, source, config, handle = TRUE){
  
  if(!requireNamespace("ocs4R", quietly = TRUE)){
    stop("The OCS handler requires the 'ocs4R' package")
  }
  
  ocs <- config$software$input$ocs
  if(is.null(ocs)){
    stop("There is no OCS input software configured to handle entities from an OCS service endpoint")
  }
  
  entities_file <- ocs$downloadFile(relPath = dirname(source), filename = basename(source), outdir = tempdir())
  
  entities <- switch(mime::guess_type(entities_file),
                     "text/csv" = {
                       handle_entities_csv <- source(system.file("metadata/entity", "entity_handler_csv.R", package = "geoflow"))$value
                       handle_entities_csv(handler = handler, source = entities_file, config = config, handle = handle)
                      },
                     "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = {
                       handle_entities_excel <- source(system.file("metadata/entity", "entity_handler_excel.R", package = "geoflow"))$value
                       handle_entities_excel(handler = handler, source = entities_file, config = config, handle = handle)
                     }
  )
  return(entities)
}