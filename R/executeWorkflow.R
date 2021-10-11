#' @name executeWorkflow
#' @aliases executeWorkflow
#' @title executeWorkflow
#' @description \code{executeWorkflow} allows to execute a workflow
#'
#' @usage executeWorkflow(file, dir)
#'                 
#' @param file a JSON geoflow configuration file
#' @param dir a directory where to execute the workflow
#' @return the path of the job directory
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
executeWorkflow <- function(file, dir = "."){
  
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
  
  #3. Execute the workflow job
  capture.output({
    exec <- try(executeWorkflowJob(config))
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