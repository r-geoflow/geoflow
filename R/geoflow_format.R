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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_format <- R6Class("geoflow_format",
 public = list(
   #'@field key format key
   key = NULL,
   #'@field name key name
   name = NULL,
   #'@field uri key URI
   uri = NULL,
   #'@field description key description
   description = NULL,
   
   #'@description Initializes a \link{geoflow_format}
   #'@param str character string to initialize object using key-based syntax
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
   
   #'@description Sets format key
   #'@param key key
   setKey = function(key){
     self$key <- key
   },
   
   #'@description Sets format name
   #'@param name name
   setName = function(name){
     self$name <- name
   },
   
   #'@description Sets format URI
   #'@param uri URI
   setUri = function(uri){
     self$uri <- uri
   },
   
   #'@description Sets format description
   #'@param description description
   setDescription = function(description){
     self$description <- description
   }
   
 )
  
)