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
#' @return Object of \code{\link{R6Class}} for modelling a keyword
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   kwd <- geoflow_keyword$new()
#'   kwd$setName("keyword")
#'   kwd$setUri("http://somelink/keyword")
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new)}}{
#'    This method is used to instantiate a geoflow_keyword object
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_keyword <- R6Class("geoflow_keyword",
 public = list(
   name = NULL,
   uri = NULL,
   initialize = function(){},
   
   #setName
   setName = function(name){
     self$name <- name
   },
   
   #setUri
   setUri = function(uri){
     self$uri <- uri
   }
 )                                  
)