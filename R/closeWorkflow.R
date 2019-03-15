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
  #close DB
  if(!is.null(config$software$db)){
    config$logger.info("Closing database connection")
    dbDisconnect(config$software$db)
    config$software$db <- config$software$db_config
    config$software$db_config <- NULL
  }
  #Geoserver API manager
  if(!is.null(config$software$geoserver)){
    config$logger.info("Reset Geoserver API manager")
    config$software$geoserver <- config$software$geoserver_config
    config$software$geoserver_config <- NULL
  }
  #Geonetwork API manager
  if(!is.null(config$software$geonetwork)){
    config$logger.info("Reset Geonetwork API manager")
    config$software$geonetwork <- config$software$geonetwork_config
    config$software$geonetwork_config <- NULL
  }
  #WFS
  if(!is.null(config$software$wfs)){
    config$logger.info("Reset WFS client")
    config$software$wfs <- config$software$wfs_config
    config$software$wfs_config <- NULL
  }
  #CSW
  if(!is.null(config$software$csw)){
    config$logger.info("Reset CSW client")
    config$software$csw <- config$software$csw_config
    config$software$csw_config <- NULL
  }
  setwd(config$wd)
}