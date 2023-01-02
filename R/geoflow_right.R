#' geoflow_right
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_right
#' @title Geoflow right class
#' @description This class models an right
#' @keywords right
#' @return Object of \code{\link{R6Class}} for modelling an right
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   right <- geoflow_right$new()
#'   right$setKey("use")
#'   right$setValue("No restrictions")
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_right <- R6Class("geoflow_right",
  public = list(
    #'@field key right key
    key = NULL,
    #'@field value right value
    value = NULL,
    
    #'@description Initializes an object of class \link{geoflow_right}
    #'@param str character string to initialize from using key-based syntax
    #'@param kvp an object of class \link{geoflow_kvp}
    initialize = function(str = NULL, kvp = NULL){
      if(!is.null(str)){
        right <- extract_kvp(str)
        self$setKey(right$key)
        self$setValue(paste(right$values, collapse=","))
      }else if(!is.null(kvp)){
        self$setKey(kvp$key)
        self$setValue(kvp$values)
      }
    },
    
    #'@description Sets key
    #'@param key key
    setKey = function(key){
      self$key <- key
    },
    
    #'@description Sets value
    #'@param value value
    setValue = function(value){
      self$value <- value
    }
  )                                  
)