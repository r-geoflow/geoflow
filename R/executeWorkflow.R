#' @name executeWorkflow
#' @aliases executeWorkflow
#' @title executeWorkflow
#' @description \code{executeWorkflow} allows to execute a workflow
#'
#' @usage executeWorkflow(file, dir)
#'                 
#' @param file a JSON geoflow configuration file
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
executeWorkflow <- function(file, dir = NULL){
  
  wd <- getwd()
  if(!is.null(dir)) setwd(dir)
  
  #options
  .defaultOptions <- options()
  options(stringsAsFactors = FALSE)
  options(gargle_oob_default = TRUE)
  
  #1. Init the workflow based on configuration file
  config <- initWorkflow(file)
  
  #2. Inits workflow job (create directories)
  jobdir <- initWorkflowJob(config)
  config$job <- jobdir
  
  #3. Execute the workflow job
  capture.output({
    exec <- try(executeWorkflowJob(config))
    if(class(exec)=="try-error"){
      setwd(config$wd)
      closeWorkflow(config)
    }
  }, file = file.path(jobdir, "job-logs.txt"))
  
  #4. close workflow
  closeWorkflow(config)
  
  #reset options
  options(.defaultOptions)
  
  setwd(wd)

}