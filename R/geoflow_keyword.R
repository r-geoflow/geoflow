#' geoflow_keyword
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_keyword
#' @title Geoflow keyword class
#' @description This class models a keyword
#' @keywords keyword
#' @return Object of \code{\link[R6]{R6Class}} for modelling a keyword
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   kwd <- geoflow_keyword$new()
#'   kwd$setName("keyword")
#'   kwd$setUri("http://somelink/keyword")
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_keyword <- R6Class("geoflow_keyword",
 public = list(
   #'@field name keyword
   name = NULL,
   #'@field uri keyword uri
   uri = NULL,
   
   #'@description Initializes a \link{geoflow_keyword}
   #'@param name keyword name
   #'@param uri keyword URI
   initialize = function(name = NULL, uri = NULL){
      self$name <- name
      self$uri <- uri
   },
   
   #'@description Sets keyword
   #'@param name keyword name
   setName = function(name){
     self$name <- name
   },
   
   #'@description Sets keyword URI
   #'@param uri keyword URI
   setUri = function(uri){
     self$uri <- uri
   }
 )                                  
)
