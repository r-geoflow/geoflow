#'geoflow_process
#'@export
geoflow_process <- R6Class("geoflow_process",
  list(
    rationale = NULL,
    description = NULL,
    processor = NULL,
    initialize = function(str = NULL){
      if(!is.null(str)){
      }
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