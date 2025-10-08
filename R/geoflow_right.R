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
#' @return Object of \code{\link[R6]{R6Class}} for modelling an right
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   right <- geoflow_right$new()
#'   right$setKey("use")
#'   right$setValues("No restrictions")
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_right <- R6Class("geoflow_right",
  public = list(
    #'@field key right key
    key = NULL,
    #'@field values right values
    values = list(),
    
    #'@description Initializes an object of class \link{geoflow_right}
    #'@param str character string to initialize from using key-based syntax
    #'@param kvp an object of class \link{geoflow_kvp}
    initialize = function(str = NULL, kvp = NULL){
      if(!is.null(str)){
        right <- extract_kvp(str)
        self$setKey(right$key)
        self$setValues(right$values)
      }else if(!is.null(kvp)){
        self$setKey(kvp$key)
        values = lapply(1:length(kvp$values), function(i){
          right = kvp$values[[i]]
          attributes(right) <- NULL
          val_locale_attrs <- attributes(kvp$values)
          for(attr_name in names(val_locale_attrs)){
            locale_value <- val_locale_attrs[[attr_name]][[i]]
            attributes(locale_value) <- NULL
            attr(right, attr_name) <- locale_value
          }
          return(right)
        })
        self$setValues(values)
      }
    },
    
    #'@description Sets key
    #'@param key key
    setKey = function(key){
      self$key <- key
    },
    
    #'@description Sets values
    #'@param values values
    setValues = function(values){
      self$values <- c(self$values, values)
    }
  )                                  
)
