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
#' @section Methods:
#' \describe{
#'  \item{\code{new(str)}}{
#'    This method is used to instantiate a geoflow_kvp object
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
geoflow_kvp <- R6Class("geoflow_kvp",
  public = list(
    key = NULL,
    value = NULL,
    initialize = function(str = NULL){
      if(!is.null(str)){
        kvp <- unlist(strsplit(str,":"))
        if(length(kvp)!=2) stop("Invalid Key-value pair string")
        self$setKey(kvp[1])
        self$setValue(kvp[2])
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