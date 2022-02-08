#' geoflowLogger
#'
#' @docType class
#' @export
#' @keywords logger
#' @return Object of \code{\link{R6Class}} for modelling a simple logger
#' @format \code{\link{R6Class}} object.
#'
#' @section Abstract Methods:
#' \describe{
#'  \item{\code{INFO(text)}}{
#'    Logger to report information. Used internally
#'  }
#'  \item{\code{WARN(text)}}{
#'    Logger to report warnings. Used internally
#'  }
#'  \item{\code{ERROR(text)}}{
#'    Logger to report errors. Used internally
#'  }
#' }
#' 
#' @note Logger class used internally by geoflow
#'
geoflowLogger <- R6Class("geoflowLogger",
   private = list(
     logger = function(type, text){
       cat(sprintf("[geoflow][%s] %s \n", type, text))
     }
   ),
   public = list(
     #'@description Prints an INFO logger message
     #'@param text logger message
     INFO = function(text){private$logger("INFO", text)},
     
     #'@description Prints an WARN logger message
     #'@param text logger message
     WARN = function(text){private$logger("WARN", text)},
     
     #'@description Prints an ERROR logger message
     #'@param text logger message
     ERROR = function(text){private$logger("ERROR", text)},
     
     #'@description Initializes an object of class \link{geoflowLogger}
     initialize = function(){}
   )
)