#handle_dictionary_ocs
handle_dictionary_ocs <- function(handler, source, config, handle = TRUE){
  
  if(!requireNamespace("ocs4R", quietly = TRUE)){
    stop("The OCS handler requires the 'ocs4R' package")
  }
  
  ocs <- config$software$input$ocs
  if(is.null(ocs)){
    stop("There is no OCS input software configured to handle dictionary from an OCS service endpoint")
  }
  
  dict_file <- ocs$downloadFile(relPath = dirname(source), filename = basename(source), outdir = tempdir())
  
  dictionary <- switch(mime::guess_type(dict_file),
                       "text/csv" = {
                         handle_dictionary_csv <- source(system.file("metadata/dictionary", "dictionary_handler_csv.R", package = "geoflow"))$value
                         handle_dictionary_csv(handler = handler, source = dict_file, config = config, handle = handle)
                        },
                       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = {
                         handle_dictionary_excel <- source(system.file("metadata/dictionary", "dictionary_handler_excel.R", package = "geoflow"))$value
                         handle_dictionary_excel(handler = handler, source = dict_file, config = config, handle = handle)
                       }
  )
  return(dictionary)
}