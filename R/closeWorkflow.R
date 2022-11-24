#' @name closeWorkflow
#' @aliases closeWorkflow
#' @title closeWorkflow
#' @description \code{closeWorkflow} allows to close a workflow
#'
#' @usage closeWorkflow(config)
#'                 
#' @param config a configuration object as read by \code{closeWorkflow}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
closeWorkflow <- function(config){
  #unload env environment
  unload_workflow_environment(config)
  #close DBs
  if(!is.null(config$software$input$dbi)){
    config$logger.info("Closing input database connection")
    DBI::dbDisconnect(config$software$input$dbi)
    config$software$input$dbi <- config$software$input$dbi_config
    config$software$input$dbi_config <- NULL
  }
  if(!is.null(config$software$output$dbi)){
    config$logger.info("Closing output database connection")
    DBI::dbDisconnect(config$software$output$dbi)
    config$software$output$dbi <- config$software$output$dbi_config
    config$software$output$dbi_config <- NULL
  }
  #Geoserver API manager
  if(!is.null(config$software$output$geoserver)){
    config$logger.info("Reset Geoserver API manager")
    config$software$output$geoserver <- config$software$output$geoserver_config
    config$software$output$geoserver_config <- NULL
  }
  #Geonetwork API manager
  if(!is.null(config$software$output$geonetwork)){
    config$logger.info("Reset Geonetwork API manager")
    config$software$output$geonetwork <- config$software$output$geonetwork_config
    config$software$output$geonetwork_config <- NULL
  }
  #WFS
  if(!is.null(config$software$output$wfs)){
    config$logger.info("Reset WFS client")
    config$software$output$wfs <- config$software$output$wfs_config
    config$software$output$wfs_config <- NULL
  }
  #CSW
  if(!is.null(config$software$output$csw)){
    config$logger.info("Reset CSW client")
    config$software$output$csw <- config$software$output$csw_config
    config$software$output$csw_config <- NULL
  }
  setwd(config$wd)
  set_line_separator() #default line separator
  
}