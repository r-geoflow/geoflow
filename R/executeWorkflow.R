#' @name executeWorkflow
#' @aliases executeWorkflow
#' @title executeWorkflow
#' @description \code{executeWorkflow} allows to execute a workflow
#'
#' @usage executeWorkflow(file, dir)
#'                 
#' @param file a JSON geoflow configuration file
#' @param dir a directory where to execute the workflow
#' @param shinyMonitor a monitor shiny function to increase progress bar 
#' @return the path of the job directory
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
executeWorkflow <- function(file, dir = ".", shinyMonitor = NULL){
  
  #options
  .defaultOptions <- options()
  options(stringsAsFactors = FALSE)
  options(gargle_oob_default = TRUE)
  
  #manage monitor
  if(!is.null(shinyMonitor)){
    if(isTRUE(shinyMonitor)){
      shinyMonitor = function(step,init=FALSE, entity,action){
        if(init){
          shiny::setProgress(value = step, detail = paste0("Initialsation of workflow","...",step,"%"))
        }else{
          shiny::setProgress(value = step, detail = paste0("Executing action:",action,"of entity:",entity,"...",step,"%"))
        }
      }
    }else if(!is.function(shinyMonitor)){
        shinyMonitor = NULL
      }else{}
  }
  
  #1. Init the workflow based on configuration file
  config <- initWorkflow(file, dir = dir)
  
  #2. Inits workflow job (create directories)
  wd <- getwd()
  if(!is.null(dir)) setwd(dir)
  jobdir <- initWorkflowJob(config)
  config$debug <- FALSE
  config$job <- jobdir
  
  #3. Execute the workflow job
  capture.output({
    exec <- try(executeWorkflowJob(config,monitor = monitor))
    if(class(exec)=="try-error"){
      setwd(wd)
      closeWorkflow(config)
      stop(sprintf("Workflow '%s' failed, check logs at: %s", config$profile$id, file.path(jobdir, "job-logs.txt")))
    }
  }, file = file.path(jobdir, "job-logs.txt"))
  
  #4. close workflow
  closeWorkflow(config)
  
  #reset options
  options(.defaultOptions)
  
  setwd(wd)
  return(jobdir)
}