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
#'    fun = function(config, source){}
#'  )
#' }
#' 
#' @note This class is essentially called internally by geoflow to register default handlers
#' for entities and contacts.
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(id, def, fun, script)}}{
#'    This method is used to instantiate a geoflow_handler object
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_handler <- R6Class("geoflow_handler",
  public = list(
    id = NA,
    def = NA,
    fun = NA,
    script = NA,
    initialize = function(id, def = "", fun = NULL, script = NULL){
      self$id <- id
      self$def <- def
      self$fun <- fun
      self$script <- script
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
      fun = handle_contacts_excel
    ),
    geoflow_handler$new(
      id = "gsheet",
      def = "Handle metadata contacts from a Google spreadsheet",
      fun = handle_contacts_gsheet
    ),
    geoflow_handler$new(
      id = "dbi",
      def = "Handle metadata contacts from a DB source",
      fun = handle_contacts_dbi
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
      fun = handle_entities_excel
    ),
    geoflow_handler$new(
      id = "gsheet",
      def = "Handle metadata entities from a Google spreadsheet",
      fun = handle_entities_gsheet
    ),
    geoflow_handler$new(
      id = "dbi",
      def = "Handle metadata entities from a DB source",
      fun = handle_entities_dbi
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
        stringsAsFactors = FALSE
      ))
    }))
  }
  return(handlers)
}

