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
#' @section Methods:
#' \describe{
#'  \item{\code{new(str)}}{
#'    This method is used to instantiate a geoflow_right object
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
geoflow_right <- R6Class("geoflow_right",
  public = list(
    key = NULL,
    value = NULL,
    initialize = function(str = NULL){
      if(!is.null(str)){
        right <- extract_kvp(str)
        self$setKey(right$key)
        self$setValue(right$values[[1]])
      }
    },
    
    #setKey
    setKey = function(key){
      self$key <- key
    },
    
    #setValue
    setValue = function(value){
      self$value <- value
    }
  )                                  
)