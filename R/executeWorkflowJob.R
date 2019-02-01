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
      if(length(tasks)==0){
        config$logger.warn("No actions enabled for this workflow!")
      }else{
        config$logger.info(sprintf("Workflow with %s actions", length(tasks)))
        for(i in 1:length(tasks)){config$logger.info(sprintf("Action %s: %s", i, tasks[[i]]))}
        for(i in 1:length(tasks)){
          eval(expr = parse(tasks[[i]]))
        } 
      }
    }
  },file = file.path(getwd(), "logs", "job.log"))
}