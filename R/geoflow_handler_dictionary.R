#' @name register_dictionary_handlers
#' @aliases register_dictionary_handlers
#' @title register_dictionary_handlers
#' @description \code{register_dictionary_handlers} registers the default dictionary handlers for geoflow
#'
#' @usage register_dictionary_handlers()
#' 
#' @note Internal function called on load by geoflow
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
register_dictionary_handlers <- function(){
  handlers <- list(
    geoflow_handler$new(
      id = "csv",
      def = "Handle dictionary from a CSV file",
      fun = source(system.file("metadata/dictionary", "dictionary_handler_csv.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "excel",
      def = "Handle dictionary from a Microsoft Excel (xls,xlsx) file",
      packages = list("readxl"),
      fun = source(system.file("metadata/dictionary", "dictionary_handler_excel.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "gsheet",
      def = "Handle dictionary from a Google spreadsheet",
      packages = list("gsheet"),
      fun = source(system.file("metadata/dictionary", "dictionary_handler_gsheet.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "dbi",
      def = "Handle dictionary from a DB source",
      packages = list("DBI", "RSQLite", "RPostgres"),
      fun = source(system.file("metadata/dictionary", "dictionary_handler_dbi.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "ocs",
      def = "Handle dictionary from a tabulat data source (csv or excel) hosted on an OCS cloud",
      packages = list("ocs4R"),
      fun = source(system.file("metadata/dictionary", "dictionary_handler_ocs.R", package = "geoflow"))$value
    )
  )
  .geoflow$dictionary_handlers <- handlers
}

#' @name list_dictionary_handlers
#' @aliases list_dictionary_handlers
#' @title list_dictionary_handlers
#' @description \code{list_dictionary_handlers} lists the dictionary handlers supported by geoflow.
#'
#' @usage list_dictionary_handlers(raw)
#' 
#' @param raw Default value is \code{FALSE}, meaning the handlers will be listed as
#' \code{data.frame}. The output If \code{TRUE} the raw list of \link{geoflow_handler} 
#' is returned.
#' 
#' @return an object of class \code{data.frame} (or \code{list} of \link{geoflow_handler} if raw = FALSE)
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
list_dictionary_handlers <- function(raw = FALSE){
  handlers <- .geoflow$dictionary_handlers 
  if(raw){
    return(handlers)
  }else{
    handlers <- do.call("rbind", lapply(handlers, function(handler){
      return(data.frame(
        id = handler$id,
        definition = handler$def,
        packages = paste(handler$packages, collapse=","),
        status = handler$status,
        notes = handler$notes,
        stringsAsFactors = FALSE
      ))
    }))
  }
  return(handlers)
}

#' @name list_dictionary_handler_options
#' @aliases list_dictionary_handler_options
#' @title list_dictionary_handler_options
#' @description \code{list_dictionary_handler_options} lists the options available for a given dictionary handler supported by geoflow.
#'
#' @usage list_dictionary_handler_options(id, raw)
#' 
#' @param id An dictionary handler identifier
#' @param raw if raw list should be returned
#' 
#' @return an object of class \code{data.frame} (or \code{list} if raw is TRUE) listing the available handler options.
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
list_dictionary_handler_options <- function(id, raw = FALSE){
  out <- NULL
  handlers <- list_dictionary_handlers(raw = TRUE)
  handler <- handlers[sapply(handlers, function(x){x$id == id})]
  if(length(handler)==0) stop(sprintf("No handler with id '%s'!", id))
  handler <- handler[[1]]
  if(raw) return(handler$available_options)
  if(length(handler$available_options)>0){
    out <- data.frame(
      name = names(handler$available_options),
      definition = sapply(handler$available_options, function(x){x$def}),
      default = sapply(handler$available_options, function(x){paste0(x$default, collapse=",")}),
      stringsAsFactors = FALSE
    )
    row.names(out) <- 1:nrow(out)
  }else{
    out <- data.frame(name = character(0), definition = character(0))
  }
  return(out)
}
