#' @name executeWorkflowJob
#' @aliases executeWorkflowJob
#' @title executeWorkflowJob
#' @description \code{executeWorkflowJob} allows to execute a workflow job
#'
#' @usage executeWorkflowJob(config)
#'                 
#' @param config a configuration object as read by \code{initWorkflow}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'    
executeWorkflowJob <- function(config){
  capture.output({
    config$logger.info("Executing workflow job...")
    tasks <- config$tasks
    if(is.null(tasks)){
      config$logger.warn("No actions enabled for this workflow!")
    }else{
      for(task in tasks) source(task)
    }
  },file = file.path(getwd(), "logs", "job.log"))
}