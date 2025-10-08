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
#' @return Object of \code{\link[R6]{R6Class}} for modelling an kvp (Key Values pair)
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_kvp <- R6Class("geoflow_kvp",
  public = list(
    #'@field key the KVP key
    key = NULL,
    #'@field values the KVP values
    values = NULL,
    #'@field locale a locale definition for the KVP
    locale = NULL,
    
    #'@description Initializes a Key-Value pair (KVP)
    #'@param key key
    #'@param values values
    #'@param locale locale
    initialize = function(key = NULL, values = NULL, locale = NULL){
      if(!is.null(key)) self$setKey(key)
      if(!is.null(values)) self$setValues(values)
      if(!is.null(locale)) self$setLocale(locale)
    },
    
    #'@description Set KVP key
    #'@param key the key
    setKey = function(key){
      self$key <- key
    },
    
    #'@description Set KVP values
    #'@param values the values
    setValues = function(values){
      self$values <- values
    },
    
    #'@description Set KVP locale
    #'@param locale locale
    setLocale = function(locale){
      self$locale <- locale
    }
  )                                  
)
