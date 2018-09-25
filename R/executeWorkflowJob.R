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
    config$logger.info(sprintf("Executing workflow job with main R function '%s'...", config$main))
    eval(parse(text = sprintf("%s(config)", config$main)))
  },file = file.path(getwd(), "logs", "job.log"))
}