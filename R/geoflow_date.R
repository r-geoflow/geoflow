#' geoflow_date
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_date
#' @title Geoflow date class
#' @description This class models an date
#' @keywords date
#' @return Object of \code{\link{R6Class}} for modelling an date
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   date <- geoflow_date$new()
#'   date$setKey("creation")
#'   date$setValue(Sys.time())
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate a geoflow_date object
#'  }
#'  \item{\code{setKey(key)}}{
#'    Set the key
#'  }
#'  \item{\code{setValue(value)}}{
#'    Set the value
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_date <- R6Class("geoflow_date",
   public = list(
     key = "creation",
     value = Sys.time(),
     initialize = function(){},
     
     #setKey
     setKey = function(key){
       if(!is(key, "character")){
         stop("The key should be an object of class 'character")
       }
       self$key <- key
     },
     
     #setValue
     setValue = function(value){
       if(is(value, "character")){
         value <- sanitize_date(value)
       }
       if(!(is(value, "Date") | inherits(value, "POSIXt"))){
         stop("The value should be a Date or POSIXt object")
       }
       self$value <- value
     }
   )                                  
)