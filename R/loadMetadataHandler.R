#' @name loadMetadataHandler
#' @aliases loadMetadataHandler
#' @title loadMetadataHandler
#' @description \code{loadMetadataHandler} allows to load a metadata handler
#'
#' @usage loadMetadataHandler(config, element, type)
#'                 
#' @param config a geoflow configuration (as list). Only used to write logs, can be NULL.
#' @param element a geoflow configuration metadata list element
#' @param type either 'contacts', 'entities' or 'dictionnary'
#' @return an object of class \link{geoflow_handler}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
loadMetadataHandler <- function(config, element, type){
  md_handler <- NULL
  if(is.null(element)) return(md_handler)
  h <- element$handler
  if(is.null(h)){
    errMsg <- "Missing 'handler' (default handler id, or function name from custom script)"
    if(!is.null(config)) config$logger$ERROR(errMsg)
    stop(errMsg)
  }
  
  #type of handler
  isHandlerId <- is.null(element$script)
  if(isHandlerId){
    if(!is.null(config)) config$logger$INFO("Try to use embedded %s handler", type)
    #in case handler id is specified
    md_default_handlers <- switch(type,
                                  "contacts" = list_contact_handlers(raw=TRUE),
                                  "entities" = list_entity_handlers(raw=TRUE),
                                  "dictionary" = list_dictionary_handlers(raw=TRUE)
    )
    md_default_handler_ids <- sapply(md_default_handlers, function(x){x$id})
    if(!(h %in% md_default_handler_ids)){
      errMsg <- sprintf("Unknown handler '%s'. Available handlers are: %s",
                        h, paste(md_default_handler_ids, collapse=","))
    }
    h_src <- element$source
    if(is.null(h_src)){
      errMsg <- sprintf("Missing 'source' for handler '%s'", h)
    }
    
    md_handler <- md_default_handlers[sapply(md_default_handlers, function(x){x$id==h})][[1]]
    md_handler$options = element$options
    
  }else{
    #in case handler is a script
    h_script <- element$script
    if(!is.null(config)) config$logger$INFO("Try to use custom handler '%s' from script '%s'", h, h_script)
    isScriptUrl <- regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", h_script) > 0
    if(!isScriptUrl) if(!file.exists(h_script)){
      errMsg <- sprintf("File '%s' does not exist in current directory!", h_script)
      if(!is.null(config)) config$logger$ERROR(errMsg)
      stop(errMsg)
    }
    source(h_script) #load script
    md_handler_fun <- try(eval(parse(text = h)))
    if(is(md_handler_fun,"try-error")){
      errMsg <- sprintf("Failed loading function '%s. Please check the script '%s'", h, h_script)
      if(!is.null(config)) config$logger$ERROR(errMsg)
      stop(errMsg)
    }
    
    #check custom handler arguments
    args <- names(formals(md_handler_fun))
    if(!all(c("handler", "source", "config") %in% args)){
      errMsg <- "The handler function should at least include the parameters (arguments) 'handler', 'source' and 'config'"
      if(!is.null(config)) config$logger$ERROR(errMsg)
      stop(errMsg)
    }
    md_handler = geoflow_handler$new(id = h, fun = md_handler_fun, options = element$options)
  }
  return(md_handler)
}
