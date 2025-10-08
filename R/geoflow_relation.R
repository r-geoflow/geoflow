#' geoflow_relation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_relation
#' @title Geoflow relation class
#' @description This class models an relation
#' @keywords relation
#' @return Object of \code{\link[R6]{R6Class}} for modelling an relation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   relation <- geoflow_relation$new()
#'   relation$setKey("wms")
#'   relation$setLink("http://somelink/wms")
#'   relation$setMimeType("application/xml")
#'   relation$setName("layername")
#'   relation$setDescription("layer description")
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_relation <- R6Class("geoflow_relation",
 public = list(
   #'@field key relation key
   key = NULL,
   #'@field link relation link
   link = NULL,
   #'@field mimeType relation mime
   mimeType = NULL,
   #'@field name relation name
   name = NULL,
   #'@field description relation name
   description = NULL,
   
   #'@description Initializes an object of class \link{geoflow_relation}
   #'@param str character string to initialize from using key-based syntax
   initialize = function(str = NULL){
     if(!is.null(str)){
       kvp <- extract_kvp(str)
       self$setKey(kvp$key)
       name <- kvp$values[[1]]
       link <- attr(name, "uri")
       description <- attr(name, "description")
       attr(name, "uri") <- NULL
       attr(name, "description") <- NULL
       if(!is.null(link)){
          self$setLink(link)
          req = try(httr::HEAD(link), silent = TRUE)
          if(!is(req,"try-error")){
             mimetype = httr::headers(req)[["Content-Type"]]
             if(!is.null(mimetype)){
                self$setMimeType(mimetype)
             }
          }
       }
       self$setName(name)
       self$setDescription(description)
     }
   },
   
   #'@description Set key
   #'@param key key
   setKey = function(key){
     self$key <- key
   },
   
   #'@description Set link
   #'@param link link
   setLink = function(link){
     self$link <- link
   },
   
   #'@description Set mime type
   #'@param mimeType mime type
   setMimeType = function(mimeType){
      self$mimeType = mimeType
   },
   
   #'@description Set name
   #'@param name name
   setName = function(name){
     self$name <- name
   },
   
   #'@description Set description
   #'@param description description
   setDescription = function(description){
     self$description <- description
   }
 )                                  
)
