#' geoflowLogger
#'
#' @docType class
#' @export
#' @keywords logger
#' @return Object of \code{\link[R6]{R6Class}} for modelling a simple logger
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Logger class used internally by geoflow
#'
geoflowLogger <- R6Class("geoflowLogger",
   private = list(
     logger = function(type, txt, ...){
       log_txt <- sprintf(txt, ...)
       cat(sprintf("[geoflow][%s] %s \n", type, log_txt))
     }
   ),
   public = list(
     
     #'@field verbose verbose
     verbose = TRUE,
     
     #'@field debug debug
     debug = FALSE,

     #'@description Util to separate chunks of logs of different natures
     #'@param char A character to be used, eg '='
     separator = function(char){
       cat(paste0(paste0(rep(char,100),collapse=""),"\n"))
     },
     
     #'@description Prints an INFO logger message
     #'@param txt logger message
     #'@param ... values to be passed into txt. See \link{sprintf}
     INFO = function(txt, ...){
       if(self$verbose) private$logger("INFO", txt, ...)
     },
     
     #'@description Prints an WARN logger message
     #'@param txt logger message
     #'@param ... values to be passed into txt. See \link{sprintf}
     WARN = function(txt, ...){
       if(self$verbose) private$logger("WARN", txt, ...)
     },
     
     #'@description Prints an ERROR logger message
     #'@param txt logger message
     #'@param ... values to be passed into txt. See \link{sprintf}
     ERROR = function(txt, ...){
       if(self$verbose) private$logger("ERROR", txt, ...)
     },
     
     #'@description Prints an DEBUG logger message
     #'@param txt logger message
     #'@param ... values to be passed into txt. See \link{sprintf}
     DEBUG = function(txt, ...){
       if(self$verbose & self$debug) private$logger("DEBUG", txt, ...)
     },
     
     #'@description Initializes an object of class \link{geoflowLogger}
     #'@param verbose \code{TRUE} if the logger is enabled, \code{FALSE} otherwise. Default is \code{TRUE}
     #'@param debug \code{TRUE} if the debugger is enabled, \code{FALSE} otherwise. Default is \code{FALSE}
     initialize = function(verbose = TRUE, debug = FALSE){
       self$verbose = verbose
       self$debug = debug
     }
   )
)
