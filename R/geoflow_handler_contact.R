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
  yml_files = list.files(system.file("metadata/contact", package = "geoflow"), pattern = "yml")
  handlers <- lapply(yml_files, function(file){
    geoflow_handler$new(yaml = system.file("metadata/contact", file, package = "geoflow"))
  })
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
        status = handler$status,
        notes = handler$notes,
        maintainer = if(!is.null(handler$maintainer$name)){handler$maintainer$name}else{ if(handler$maintainer$orphaned) "<orphaned>" else "<unknown>" },
        stringsAsFactors = FALSE
      ))
    }))
  }
  return(handlers)
}

#' @name list_contact_handler_options
#' @aliases list_contact_handler_options
#' @title list_contact_handler_options
#' @description \code{list_contact_handler_options} lists the options available for a given contact handler supported by geoflow.
#'
#' @usage list_contact_handler_options(id, raw)
#' 
#' @param id An contact handler identifier
#' @param raw if raw list should be returned
#' 
#' @return an object of class \code{data.frame} (or \code{list} if raw is TRUE) listing the available handler options.
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
list_contact_handler_options <- function(id, raw = FALSE){
  out <- NULL
  handler <- get_contact_handler(id)
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

#' @name get_contact_handler
#' @aliases get_contact_handler
#' @title get_contact_handler
#' @description \code{get_contact_handler} allows to get a contact handler
#' 
#' @usage get_contact_handler(id)
#' 
#' @param id A contact handler identifier
#' @return an object of class \link{geoflow_handler}
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
get_contact_handler <- function(id){
  handlers <- list_contact_handlers(raw = TRUE)
  handler <- handlers[sapply(handlers, function(x){x$id == id})]
  if(length(handler)==0) stop(sprintf("No handler with id '%s'!", id))
  handler <- handler[[1]]
  return(handler)
}

#' @name read_contacts
#' @aliases read_contacts
#' @title read_contacts
#' @description \code{read_contacts} allows to read contacts
#' 
#' @usage read_contacts(id, source, config)
#' 
#' @param id a contact handler identifier
#' @param source source
#' @param config a geoflow config (output of \link{initWorkflow}). Default is \code{NULL}
#' @return a list of object of class \link{geoflow_contact}
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
read_contacts <- function(id, source, config = NULL){
  handler <- get_contact_handler(id)
  if(is.null(config)){
    config = add_config_logger(list())
  }
  handler$fun(
    handler = handler,
    source = source,
    config = config,
    handle = TRUE
  )
}