#' @name executeWorkflow
#' @aliases executeWorkflow
#' @title executeWorkflow
#' @description \code{executeWorkflow} allows to execute a workflow
#'
#' @usage executeWorkflow(file, dir, queue, 
#'                        on_initWorkflowJob, on_initWorkflow, on_closeWorkflow, 
#'                        monitor, session)
#'                 
#' @param file a JSON geoflow configuration file
#' @param dir a directory where to execute the workflow
#' @param queue an \pkg{ipc} queue to use geoflow in \pkg{geoflow-shiny}
#' @param on_initWorkflowJob a function to trigger once \code{initWorkflowJob} is executed
#' @param on_initWorkflow a function to trigger once \code{initWorkflow} is executed
#' @param on_closeWorkflow a function to trigger once \code{closeWorkflow} is executed
#' @param monitor a monitor function to increase progress bar
#' @param session a \pkg{shiny} session object (optional) to run geoflow in a \pkg{shiny} context
#' @return the path of the job directory
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
executeWorkflow <- function(file, dir, 
                            queue = NULL, 
                            on_initWorkflowJob = NULL,
                            on_initWorkflow = NULL,
                            on_closeWorkflow = NULL,
                            monitor = NULL,
                            session = NULL){
  
  #options
  .defaultOptions <- options()
  options(stringsAsFactors = FALSE)
  options(gargle_oob_default = TRUE)
  
  #optional shiny session object
  if(!is.null(session)) if(!is(session, "ShinySession")){
    stop("The 'session' argument should specify an object of class 'ShinySession'")
  }
  
  #1. Init the workflow based on configuration file
  wd <- getwd()
  on.exit(setwd(wd))
  config <- list()
  jobDirPath <- initWorkflowJob(dir = dir)
  config$job <- jobDirPath
  if(!is.null(on_initWorkflowJob)){
    on_initWorkflowJob(config = config, queue = queue)
  }
  
  capture.output({
    setwd(wd)
    config <- try(initWorkflow(file = file, dir = dir, jobDirPath = jobDirPath, session = session))
    if(is(config,"try-error")){
      stop(sprintf("Workflow failed during initialization, check logs at: %s", file.path(jobDirPath, "job-logs.txt")))
    }
    setwd(jobDirPath)
    config$debug <- FALSE
    if(!is.null(on_initWorkflow)){
      on_initWorkflow(config = config, queue = queue)
    }
  }, file = file.path(jobDirPath, "job-logs.txt"))
  
  #manage monitor
  if(!is.null(monitor)){
    if(is.function(monitor)){
      if(all(names(formals(monitor))==c("step","config","entity","action","queue"))){
        config$logger$INFO("use monitor function to trace processing steps")
      }else{
        config$logger$INFO("Monitor function escaped, parameter(s) missing or in wrong order")
        monitor=NULL
      }
    }else{
      config$logger$INFO("Monitor escaped, monitor must be a function")
      monitor = NULL
    }
  }
  
  #2. Execute the workflow job
  capture.output({
    exec <- try(executeWorkflowJob(config, queue = queue, monitor = monitor))
    if(is(exec,"try-error")){
      setwd(wd)
      closeWorkflow(config)
      stop(sprintf("Workflow failed during execution, check logs at: %s", file.path(config$job, "job-logs.txt")))
    }
    
    #3. close workflow
    closeWorkflow(config)
    if(!is.null(on_closeWorkflow)){
      on_closeWorkflow(config = config, queue = queue)
    }
    
  }, file = file.path(config$job, "job-logs.txt"), append = TRUE)
  
  #reset options
  options(.defaultOptions)
  
  setwd(wd)
  return(config$job)
}
