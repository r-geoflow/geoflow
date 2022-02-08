#' geoflow_kvp
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_kvp
#' @title Geoflow kvp (Key Values pair) class
#' @description This class models an kvp (Key Values pair)
#' @keywords kvp
#' @return Object of \code{\link{R6Class}} for modelling an kvp (Key Values pair)
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   #with setters
#'   kvp <- geoflow_kvp$new()
#'   kvp$setKey("thekey")
#'   kvp$setValue("thevalue")
#'   #from string
#'   kvp <- geoflow_kvp$new(str = "thekey:thevalue1,thevalue2")
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_kvp <- R6Class("geoflow_kvp",
  public = list(
    #'@field key the KVP key
    key = NULL,
    #'@field value the KVP value
    value = NULL,
    
    #'@description Initializes a Key-Value pair (KVP)
    #'@param str character string to initialize from using key-based syntax
    initialize = function(str = NULL){
      if(!is.null(str)){
        kvp <- unlist(strsplit(str,':\\s*(?=([^"]*"[^"]*")*[^"]*$)', perl = T))
        if(length(kvp)!=2) stop("Invalid Key-value pair string")
        self$setKey(kvp[1])
        self$setValue(kvp[2])
      }
    },
    
    #'@description Set KVP key
    #'@param key the key
    setKey = function(key){
      self$key <- key
    },
    
    #'@description Set KVP value
    #'@param value the value
    setValue = function(value){
      self$value <- value
    }
  )                                  
)