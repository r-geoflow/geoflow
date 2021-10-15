#' @name executeWorkflow
#' @aliases executeWorkflow
#' @title executeWorkflow
#' @description \code{executeWorkflow} allows to execute a workflow
#'
#' @usage executeWorkflow(file, dir, monitor)
#'                 
#' @param file a JSON geoflow configuration file
#' @param dir a directory where to execute the workflow
#' @param monitor a monitor function to increase progress bar 
#' @return the path of the job directory
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
executeWorkflow <- function(file, dir = ".", monitor = NULL){
  
  #options
  .defaultOptions <- options()
  options(stringsAsFactors = FALSE)
  options(gargle_oob_default = TRUE)
  
  #1. Init the workflow based on configuration file
  config <- initWorkflow(file, dir = dir)
  
  #2. Inits workflow job (create directories)
  wd <- getwd()
  if(!is.null(dir)) setwd(dir)
  jobdir <- initWorkflowJob(config)
  config$debug <- FALSE
  config$job <- jobdir
  
  #manage monitor
  if(!is.null(monitor)){
    if(is.function(monitor)){
      if(all(names(formals(monitor))==c("step","config","entity","action"))){
        config$logger.info("use monitor function to trace processing steps")
      }else{
        config$logger.info("Monitor function escaped, parameter(s) missing or in wrong order")
        monitor=NULL
      }
    }else{
      config$logger.info("Monitor escaped, monitor must be a function")
      monitor = NULL
    }
  }
  
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