#' geoflow_handler
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_handler
#' @title Geoflow handler class
#' @description This class models a content handler. An handler is a method to handle
#' some content (eg entity or contact). It is mainly driven by a function that takes
#' as argument a \code{config} object, as the overall configuration created by geoflow 
#' \code{initWorkflow} function, and a source which identifiers the source to be handled,
#' that can be of a different type (eg a URL, a file path) depending on the handler.
#' @keywords handler
#' @return Object of \code{\link{R6Class}} for modelling a handler
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   handler <- geoflow_handler$new(
#'    id = "some-id",
#'    def = "some definition",
#'    packages = list(),
#'    fun = function(config, source){}
#'  )
#' }
#' 
#' @note This class is essentially called internally by geoflow to register default handlers
#' for entities and contacts.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_handler <- R6Class("geoflow_handler",
  public = list(
    #'@field id handler id
    id = NA,
    #'@field def handler definition
    def = NA,
    #'@field packages handler packages
    packages = list(),
    #'@field fun handler function
    fun = NA,
    #'@field script handler script
    script = NA,
    
    #'@description Initializes a \link{geoflow_handler}
    #'@param id id
    #'@param def def
    #'@param packages list of packages required for the handler
    #'@param fun the handler \code{function} having 2 arguments \code{config} and \code{source}
    #'@param script a handler script
    initialize = function(id, def = "", packages = list(), fun = NULL, script = NULL){
      self$id <- id
      self$def <- def
      self$packages <- packages
      self$fun <- fun
      self$script <- script
    },
    
    #'@description Check that all packages required for the handler are available, if yes,
    #'    import them in the R session, and return a \code{data.frame} giving the 
    #'    packages names and version. If one or more packages are unavailable,
    #'    an error is thrown and user informed of the missing packages.
    checkPackages = function(){
      #check package dependencies
      self$INFO(sprintf("Check package dependencies for handler '%s'", self$id))
      out_pkgs <- try(check_packages(self$packages))
      if(is(out_pkgs,"try-error")){
        errMsg <- sprintf("One or more packages are not imported although required for handler '%s'", self$id)
        self$ERROR(errMsg)
        stop(errMsg)
      }else{
        if(is.null(out_pkgs)){
          self$INFO(sprintf("No additional package required for handler '%s':", self$id))
        }else{
          self$INFO(sprintf("The following packages have been imported for handler '%s':", self$id))
          print(out_pkgs)
        }
      }
    }
  )
)

#' @name register_contact_handlers
#' @aliases register_contact_handlers
#' @title register_contact_handlers
#' @description \code{register_contact_handlers} registers the default contact handlers for geoflow
#'
#' @usage register_contact_handlers()
#' 
#' @note Internal function called on load by geoflow
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
register_contact_handlers <- function(){
  handlers <- list(
    geoflow_handler$new(
      id = "csv",
      def = "Handle metadata contacts from a CSV file",
      fun = handle_contacts_csv
    ),
    geoflow_handler$new(
      id = "excel",
      def = "Handle metadata contacts from a Microsoft Excel (xls,xlsx) file",
      packages = list("readxl"),
      fun = handle_contacts_excel
    ),
    geoflow_handler$new(
      id = "gsheet",
      def = "Handle metadata contacts from a Google spreadsheet",
      packages = list("gsheet"),
      fun = handle_contacts_gsheet
    ),
    geoflow_handler$new(
      id = "dbi",
      def = "Handle metadata contacts from a DB source",
      packages = list("DBI", "RSQLite", "RPostgres"),
      fun = handle_contacts_dbi
    ),
    geoflow_handler$new(
      id = "ocs",
      def = "Handle metadata contacts from a tabulat data source (csv or excel) hosted on an OCS cloud",
      packages = list("ocs4R"),
      fun = handle_contacts_ocs
    )
  )
  .geoflow$contact_handlers <- handlers
}

#' @name list_contact_handlers
#' @aliases list_contact_handlers
#' @title list_contact_handlers
#' @description \code{list_contact_handlers} lists the contact handlers supported by geoflow.
#'
#' @usage list_contact_handlers(raw)
#' 
#' @param raw Default value is \code{FALSE)}, meaning the handlers will be listed as
#' \code{data.frame}. The output If \code{TRUE} the raw list of \link{geoflow_handler} 
#' is returned.
#' 
#' @return an object of class \code{data.frame} (or \code{list} of \link{geoflow_handler} if raw = FALSE)
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
list_contact_handlers <- function(raw = FALSE){
  handlers <- .geoflow$contact_handlers 
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
      fun = handle_entities_csv
    ),
    geoflow_handler$new(
      id = "excel",
      def = "Handle metadata entities from a Microsoft Excel (xls,xlsx) file",
      packages = list("readxl"),
      fun = handle_entities_excel
    ),
    geoflow_handler$new(
      id = "gsheet",
      def = "Handle metadata entities from a Google spreadsheet",
      packages = list("gsheet"),
      fun = handle_entities_gsheet
    ),
    geoflow_handler$new(
      id = "dbi",
      def = "Handle metadata entities from a DB source",
      packages = list("DBI", "RSQLite", "RPostgres"),
      fun = handle_entities_dbi
    ),
    geoflow_handler$new(
      id = "ocs",
      def = "Handle metadata entities from a tabulat data source (csv or excel) hosted on an OCS cloud",
      packages = list("ocs4R"),
      fun = handle_entities_ocs
    ),
    geoflow_handler$new(
      id = "ncdf",
      def = "Handle metadata entities from a Netcdf source",
      packages = list("ncdf4"),
      fun = handle_entities_ncdf
    ),
    geoflow_handler$new(
      id = "ncml",
      def = "Handle metadata entities from a NCML source",
      packages = list("XML"),
      fun = handle_entities_ncml
    ),
    geoflow_handler$new(
      id = "thredds",
      def = "Handle metadata entities from a Thredds server source",
      packages = list("ncdf4","thredds","XML","png","curl"),
      fun = handle_entities_thredds
    ),
    geoflow_handler$new(
      id = "thredds_csv",
      def = "Handle metadata thredds entities from a CSV file",
      fun = handle_entities_thredds_csv
    ),
    geoflow_handler$new(
      id = "thredds_excel",
      def = "Handle metadata thredds entities from a Microsoft Excel (xls,xlsx) file",
      packages = list("readxl"),
      fun = handle_entities_thredds_excel
    ),
    geoflow_handler$new(
      id = "thredds_gsheet",
      def = "Handle metadata thredds entities from a Google spreadsheet",
      packages = list("gsheet"),
      fun = handle_entities_thredds_gsheet
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
      fun = handle_dictionary_csv
    ),
    geoflow_handler$new(
      id = "excel",
      def = "Handle dictionary from a Microsoft Excel (xls,xlsx) file",
      packages = list("readxl"),
      fun = handle_dictionary_excel
    ),
    geoflow_handler$new(
      id = "gsheet",
      def = "Handle dictionary from a Google spreadsheet",
      packages = list("gsheet"),
      fun = handle_dictionary_gsheet
    ),
    geoflow_handler$new(
      id = "dbi",
      def = "Handle dictionary from a DB source",
      packages = list("DBI", "RSQLite", "RPostgres"),
      fun = handle_dictionary_dbi
    ),
    geoflow_handler$new(
      id = "ocs",
      def = "Handle dictionary from a tabulat data source (csv or excel) hosted on an OCS cloud",
      packages = list("ocs4R"),
      fun = handle_dictionary_ocs
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
        stringsAsFactors = FALSE
      ))
    }))
  }
  return(handlers)
}


