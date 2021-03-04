#' geoflow_provenance
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_provenance
#' @title Geoflow provenance class
#' @description This class models an provenance
#' @keywords provenance
#' @return Object of \code{\link{R6Class}} for modelling an provenance
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   provenance <- geoflow_provenance$new()
#'   provenance$setStatement("statement")
#'   process1 <- geoflow_process$new()
#'   process1$setRationale("task 1")
#'   process1$setDescription("Performs task 1")
#'   provenance$addProcess(process1)
#'   process2 <- geoflow_process$new()
#'   process2$setRationale("task 2")
#'   process2$setDescription("Performs task 2")
#'   provenance$addProcess(process2)
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(str)}}{
#'    This method is used to instantiate a geoflow_provenance object
#'  }
#'  \item{\code{setStatement(statement)}}{
#'    Set statement, object of class \code{character}
#'  }
#'  \item{\code{addProcess(process)}}{
#'    Adds process, object of class \code{geoflow_process}
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_provenance <- R6Class("geoflow_provenance",
   list(
     statement = NULL,
     processes = list(),
     initialize = function(str = NULL){
       if(!is.null(str)){
         data_props <- extract_cell_components(sanitize_str(str))
         state_prop <- data_props[[1]]
         if(!startsWith(state_prop, "statement")){
           stop("The data 'statement' is mandatory")
         }
         state_prop <- unlist(strsplit(state_prop,"statement:"))[2]
         self$setStatement(state_prop)
         if(length(data_props)>1){
           data_props <- data_props[2:length(data_props)]
           #processes
           processes <- data_props[sapply(data_props, function(x){startsWith(x, "process:")})]
           processes <- lapply(processes, function(process){
             return(extract_kvp(process))
           })
           #processors
           processors <- data_props[sapply(data_props, function(x){startsWith(x,"processor:")})]
           processors_splits <- unlist(strsplit(processors, ":"))
           processors <- tolower(unlist(strsplit(processors_splits[2],",")))
           #control processors vs. processes
           if(length(processors)!=length(processes)){
              stop(sprintf("Number of processors [%s] doesn't match the number of processes [%s]",
                           length(processors), length(processes)))
           }
           if(length(processes)>0 & length(processors)>0 & length(processes)==length(processors)){
             for(i in 1:length(processes)){
               process <- processes[[i]]$values[[1]]
               process_obj <- geoflow_process$new()
               process_des <- attr(process, "description")
               process_obj$setDescription(process_des)
               attr(process, "description") <- NULL
               process_obj$setRationale(process)
               processor_obj <- geoflow_contact$new()
               processor_obj$setIdentifier(key = "id", processors[i])
               processor_obj$setRole("processor")
               process_obj$setProcessor(processor_obj)
               self$addProcess(process_obj)
             }
           }
         }
       }
     },
     
     #setStatement
     setStatement = function(statement){
       self$statement <- statement
     },
     
     #addProcess
     addProcess = function(process){
       if(!is(process, "geoflow_process")){
         stop("The argument should be an object of class 'geoflow_process'")
       }
       self$processes <- c(self$processes, process)
     }
     
   )                                  
)