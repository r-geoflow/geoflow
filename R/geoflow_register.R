#' geoflow_register
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_register
#' @title Geoflow register class
#' @description This class models a register to be used by geoflow
#' @keywords registers
#' @return Object of \code{\link{R6Class}} for modelling a register
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   register <- geoflow_register$new(
#'    id = "some-id",
#'    def = "definition",
#'    fun = function(){}
#'   )
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_register <- R6Class("geoflow_register",
  public = list(
    #'@field id register id
    id = NULL,
    #'@field def register def
    def = NULL,
    #'@field fun register function
    fun = NULL,
    #'@field data register data
    data = NULL,
    
    #'@description Initializes an object of class \link{geoflow_register}
    #'@param id id
    #'@param def def
    #'@param fun fun
    initialize = function(id, def, fun){
      self$id <- id
      self$def <- def
      self$fun <- fun
    },
    
    #'@description Fetchs the register data using the register function
    #'@param config a geoflow config object
    fetch = function(config = NULL){
      fetched <- self$fun(config)
      if(inherits(fetched, "try-error")){
        stop(sprintf("Unexpected error while fetching register '%s. Check the register function", self$id))
      }
      self$check(fetched)
      self$data <- fetched
    },
    
    #'@description Checks the register data structure. The structure of the \code{data.frame} returned
    #' by the register function should be of 4 columns including "code", "uri", "label", "definition". 
    #' In case the data structure is not valid, the function throws an error.
    #' @param data a register data structure
    check = function(data){
      mandatory_columns <- c("code", "uri", "label", "definition")
      if(!all(mandatory_columns %in% colnames(data))){
        stop(sprintf("The output structure of register '%s data must contain the following columns [%s]",
             self$id, paste0(mandatory_columns, collapse = ",")))
      }
    }
    
  )
)

#' @name register_registers
#' @aliases register_registers
#' @title register_registers
#' @description \code{register_registers} registers default geoflow registers
#'
#' @usage register_registers()
#' 
#' @note Function called on load by geoflow
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
register_registers <- function(){
  registers <- list()
  .geoflow$registers <- registers
}

#' @name list_registers
#' @aliases list_registers
#' @title list_registers
#' @description \code{list_registers} lists the registers supported by geoflow.
#'
#' @usage list_registers(raw)
#' 
#' @param raw Default value is \code{FALSE}, meaning the registers will be listed as
#' \code{data.frame}. The output If \code{TRUE} the raw list of \link{geoflow_register} 
#' is returned.
#' 
#' @return an object of class \code{data.frame} (or \code{list} of \link{geoflow_register} if raw = FALSE)
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
list_registers <- function(raw = FALSE){
  registers <- .geoflow$registers
  if(raw){
    return(registers)
  }else{
    registers <- do.call("rbind", lapply(registers, function(obj){
      obj.out <- data.frame(
        id = obj$id,
        def = obj$def,
        stringsAsFactors = FALSE
      )
      return(obj.out)
    }))
  }
  return(registers)
}