#' geoflow_process
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_process
#' @title Geoflow process class
#' @description This class models an process
#' @keywords process
#' @return Object of \code{\link{R6Class}} for modelling an process
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   process <- geoflow_process$new()
#'   process$setRationale("rationale")
#'   process$setDescription("description")
#'   processor <- geoflow_contact$new()
#'   process$addProcessor(processor)
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_process <- R6Class("geoflow_process",
  list(
    #'@field rationale process rationale
    rationale = NULL,
    #'@field description process description
    description = NULL,
    #'@field processors object of class \code{list}
    processors = list(),
    
    #'@description Initializes the \link{geoflow_process}
    initialize = function(){
    },
    
    #'@description Set process rationale
    #'@param rationale the process rationale
    setRationale = function(rationale){
      self$rationale <- rationale
    },
    
    #'@description Set process description
    #'@param description Set the process description
    setDescription = function(description){
      self$description <- description
    },
    
    #'@description Adds processor
    #'@param processor, object of class \link{geoflow_contact}
    addProcessor = function(processor){
      if(!is(processor, "geoflow_contact")){
        stop("The processor should be an object of class 'geoflow_contact")
      }
      self$processors <- c(self$processors, processor)
    }
    
  )                                  
)