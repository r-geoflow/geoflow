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
  if(!is.null(config$sdi$db)){
    config$logger.info("Closing database connection")
    dbDisconnect(config$sdi$db)
    config$sdi$db <- config$sdi$db_config
    rm(config$sdi$db_config)
  }
  #Geoserver API manager
  if(!is.null(config$sdi$geoserver)){
    config$logger.info("Reset Geoserver API manager")
    config$sdi$geoserver <- config$sdi$geoserver_config
    rm(config$sdi$geoserver_config)
  }
  #Geonetwork API manager
  if(!is.null(config$sdi$geonetwork)){
    config$logger.info("Reset Geonetwork API manager")
    config$sdi$geonetwork <- config$sdi$geonetwork_config
    rm(config$sdi$geonetwork_config)
  }
  #WFS
  if(!is.null(config$sdi$wfs)){
    config$logger.info("Reset WFS client")
    config$sdi$wfs <- config$sdi$wfs_config
    rm(config$sdi$wfs_config)
  }
  #CSW
  if(!is.null(config$sdi$csw)){
    config$logger.info("Reset CSW client")
    config$sdi$csw <- config$sdi$csw_config
    rm(config$sdi$csw_config)
  }
  setwd(config$wd)
}