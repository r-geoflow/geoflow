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
#'   process$setProcessor(processor)
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate a geoflow_process object
#'  }
#'  \item{\code{setRationale(rationale)}}{
#'    Set rationale
#'  }
#'  \item{\code{setDescription(description)}}{
#'    Set description
#'  }
#'  \item{\code{setProcessor(processor)}}{
#'    Set processor, object of class \code{geoflow_contact}
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_process <- R6Class("geoflow_process",
  list(
    rationale = NULL,
    description = NULL,
    processor = NULL,
    initialize = function(){
    },
    
    setRationale = function(rationale){
      self$rationale <- rationale
    },
    
    setDescription = function(description){
      self$description <- description
    },
    
    setProcessor = function(processor){
      if(!is(processor, "geoflow_contact")){
        stop("The processor should be an object of class 'geoflow_contact")
      }
      self$processor <- processor
    }
    
  )                                  
)