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
#' @return Object of \code{\link[R6]{R6Class}} for modelling an date
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   date <- geoflow_date$new()
#'   date$setKey("creation")
#'   date$setValue(Sys.time())
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_date <- R6Class("geoflow_date",
   public = list(
     #'@field key date key. Default is "creation"
     key = "creation",
     #'@field value date value. Default is generated with \code{Sys.time()}
     value = Sys.time(),
     
     #'@description Initializes a \link{geoflow_date}
     initialize = function(){},
     
     #'@description Sets the date key
     #'@param key date key
     setKey = function(key){
       if(!is(key, "character")){
         stop("The key should be an object of class 'character")
       }
       self$key <- key
     },
     
     #'@description Sets the date value. The method will check validity of date value.
     #'@param value date value
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
