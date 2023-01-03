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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_provenance <- R6Class("geoflow_provenance",
   list(
     #'@field statement provenance statement
     statement = NULL,
     #'@field processes list of processes, as objects of class \link{geoflow_process}
     processes = list(),
     
     #'@description Initializes a \link{geoflow_provenance}
     #'@param str character string to initialize a provenance using key-based syntax
     initialize = function(str = NULL){
       if(!is.null(str)){
         strs <- extract_cell_components(sanitize_str(str))
         kvps <- extract_kvps(strs)
         
         #statement
         statement <- kvps[sapply(kvps, function(kvp){kvp$key == "statement"})]
         if(length(statement)==0) stop("No provenance statement, statement is mandatory")
         if(length(statement)>1) warning("More than one provenance statement declared, only the first will be considered!")
         statement = statement [[1]]
         self$setStatement(statement$values)
         
         #processes
         processes <- kvps[sapply(kvps, function(kvp){kvp$key == "process"})]
         if(length(processes)>0){
            for(process in processes){ #in case processes defined on various lines
               print(process)
               process_attrs <- attributes(process$values)
               for(i in 1:length(process$values)){ #in case processes defined on same line (eg for i18n)
                  value = if(is.list(process$values)) process$values[[i]] else process$values[i]
                  process_obj <- geoflow_process$new()
                  rationale = value
                  description = process_attrs$description
                  attributes(rationale) <- NULL
                  if(is.null(description)) description = attr(value, "description")
                  if(length(process_attrs)>0){
                     process_attrs <- process_attrs[names(process_attrs)!="description"]
                     for(attr_name in names(process_attrs)){
                        rationale_i18n = process_attrs[[attr_name]][[i]]
                        attr(description, attr_name) <- attr(rationale_i18n, "description")
                        attributes(rationale_i18n) = NULL
                        attr(rationale, attr_name) <- rationale_i18n
                     }
                  }
                  process_obj$setRationale(rationale)
                  process_obj$setDescription(description)
                  self$addProcess(process_obj)
               }
            }
         }
       }
     },
     
     #'@description Set process statement
     #'@param statement process statement
     setStatement = function(statement){
       self$statement <- statement
     },
     
     #'@description Adds process
     #'@param process, object of class \link{geoflow_process}
     addProcess = function(process){
       if(!is(process, "geoflow_process")){
         stop("The argument should be an object of class 'geoflow_process'")
       }
       self$processes <- c(self$processes, process)
     }
     
   )                                  
)