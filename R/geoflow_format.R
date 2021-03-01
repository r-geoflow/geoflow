#' geoflow_format
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_format
#' @title Geoflow format class
#' @description This class models a format
#' @keywords format, mime, mimetype
#' @return Object of \code{\link{R6Class}} for modelling a format
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   format <- geoflow_format$new()
#'   format$setKey("distribution")
#'   format$setName("text/csv")
#'   format$setUri("https://www.iana.org/assignments/media-types/text/csv")
#'   format$setDescription("CSV format")
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new)}}{
#'    This method is used to instantiate a geoflow_format object
#'  }
#'  \item{\code{setKey(key)}}{
#'    Set key
#'  }
#'  \item{\code{setName(name)}}{
#'    Sets name 
#'  }
#'  \item{\code{setUri(uri)}}{
#'    Sets URI
#'  }
#'  \item{\code{setDescription(description)}}{
#'    Sets description
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_format <- R6Class("geoflow_format",
 public = list(
   key = NULL,
   name = NULL,
   uri = NULL,
   description = NULL,
   initialize = function(str = NULL){
     if(!is.null(str)){
       format_kvp <- extract_kvp(str)
       key <- format_kvp$key
       self$setKey(key)
       value = format_kvp$values[[1]]
       uri <- attr(value, "uri")
       description <- attr(value, "description")
       attr(value, "uri") <- NULL
       attr(value, "description") <- NULL
       self$setUri(uri)
       self$setName(value)
       self$setDescription(description)
     }
   },
   
   #setKey
   setKey = function(key){
     self$key <- key
   },
   
   #setName
   setName = function(name){
     self$name <- name
   },
   
   #setUri
   setUri = function(uri){
     self$uri <- uri
   },
   
   #setDescription
   setDescription = function(description){
     self$description <- description
   }
   
 )
  
)