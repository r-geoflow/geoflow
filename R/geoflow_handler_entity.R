#' @name register_entity_handlers
#' @aliases register_entity_handlers
#' @title register_entity_handlers
#' @description \code{register_entity_handlers} registers the default entity handlers for geoflow
#'
#' @usage register_entity_handlers()
#' 
#' @note Internal function called on load by geoflow
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
register_entity_handlers <- function(){
  handlers <- list(
    geoflow_handler$new(
      id = "csv",
      def = "Handle metadata entities from a CSV file",
      fun = source(system.file("metadata/entity", "entity_handler_csv.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "excel",
      def = "Handle metadata entities from a Microsoft Excel (xls,xlsx) file",
      packages = list("readxl"),
      fun = source(system.file("metadata/entity", "entity_handler_excel.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "gsheet",
      def = "Handle metadata entities from a Google spreadsheet",
      packages = list("gsheet"),
      fun = source(system.file("metadata/entity", "entity_handler_gsheet.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "dbi",
      def = "Handle metadata entities from a DB source",
      packages = list("DBI", "RSQLite", "RPostgres"),
      fun = source(system.file("metadata/entity", "entity_handler_dbi.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "dataverse",
      def = "Handle metadata entities built from a Dataverse source",
      packages = list("dataverse"),
      fun = source(system.file("metadata/entity", "entity_handler_dataverse.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "ocs",
      def = "Handle metadata entities from a tabulat data source (csv or excel) hosted on an OCS cloud",
      packages = list("ocs4R"),
      fun = source(system.file("metadata/entity", "entity_handler_ocs.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "ncdf",
      def = "Handle metadata entities from a Netcdf source",
      packages = list("ncdf4"),
      fun = source(system.file("metadata/entity", "entity_handler_ncdf.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "ncml",
      def = "Handle metadata entities from a NCML source",
      packages = list("XML"),
      fun = source(system.file("metadata/entity", "entity_handler_ncml.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "thredds",
      def = "Handle metadata entities from a Thredds server source",
      packages = list("ncdf4","thredds","XML","png","curl"),
      fun = source(system.file("metadata/entity", "entity_handler_thredds.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "thredds_csv",
      def = "Handle metadata thredds entities from a CSV file",
      fun = source(system.file("metadata/entity", "entity_handler_thredds_csv.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "thredds_excel",
      def = "Handle metadata thredds entities from a Microsoft Excel (xls,xlsx) file",
      packages = list("readxl"),
      fun = source(system.file("metadata/entity", "entity_handler_thredds_excel.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "thredds_gsheet",
      def = "Handle metadata thredds entities from a Google spreadsheet",
      packages = list("gsheet"),
      fun = source(system.file("metadata/entity", "entity_handler_thredds_gsheet.R", package = "geoflow"))$value
    ),
    geoflow_handler$new(
      id = "ogc_csw",
      def = "Handle metadata entities from an OGC CSW endpoint",
      packages = list("ows4R", "sf", "geometa"),
      fun = source(system.file("metadata/entity", "entity_handler_csw.R", package = "geoflow"))$value
    )
  )
  .geoflow$entity_handlers <- handlers
}

#' @name list_entity_handlers
#' @aliases list_entity_handlers
#' @title list_entity_handlers
#' @description \code{list_entity_handlers} lists the entity handlers supported by geoflow.
#'
#' @usage list_entity_handlers(raw)
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
list_entity_handlers <- function(raw = FALSE){
  handlers <- .geoflow$entity_handlers 
  if(raw){
    return(handlers)
  }else{
    handlers <- do.call("rbind", lapply(handlers, function(handler){
      return(data.frame(
        id = handler$id,
        definition = handler$def,
        packages = paste(handler$packages, collapse=","),
        stringsAsFactors = FALSE
      ))
    }))
  }
  return(handlers)
}