#' @name debugWorkflow
#' @aliases debugWorkflow
#' @title debugWorkflow
#' @description \code{debugWorkflow} allows to initiate a workflow job for
#' developers to work / debug on workflow actions.
#'
#' @usage debugWorkflow(i, copyData, runLocalActions)
#'                 
#' @param entityIndex index of the entity within the list of loaded entities. Default is 1
#' @param copyData whether data should be downloaded/copied to job data directory.
#' Default is \code{TRUE}.
#' @param runSoftwareActions whether software actions should be run. Default is \code{TRUE}.
#' @param runLocalActions whether entity data local actions (if any) should be run.
#' Default is \code{TRUE}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
debugWorkflow <- function(file, entityIndex = 1, 
                          copyData = TRUE, 
                          runSoftwareActions = TRUE,  
                          runLocalActions = TRUE){
  
  #options
  .defaultOptions <- options()
  options(stringsAsFactors = FALSE)
  options(encoding = "UTF-8")
  
  #1. Init the workflow based on configuration file
  config <- initWorkflow(file)
  assign("config", config, envir = .GlobalEnv)
  
  #2. Inits workflow job (create directories)
  jobdir <- initWorkflowJob(config)
  config$job <- jobdir
  
  #entities
  entities <- config$getEntities()
  entity <- entities[[entityIndex]]
  assign("entity", entity, envir = .GlobalEnv)

  #run software actions?
  if(runSoftwareActions){
    #function to run software actions
    runSoftwareActions <- function(config, softwareType, actionType){
      software_list <- config$software[[softwareType]]
      if(length(software_list)>0){
        software_names <- names(software_list)
        software_names <- software_names[!endsWith(software_names, "_config")]
        for(software_name in software_names){
          software <- software_list[[software_name]]
          software_cfg <- software_list[[paste0(software_name, "_config")]]
          if(length(software_cfg$actions)>0){
            if(actionType %in% names(software_cfg$actions)){
              config$logger.info(sprintf("Executing input software action '%s'",actionType))
              software_cfg$actions[[actionType]](config, software, software_cfg)
            }
          }
        }
      }
    }
    #run software 'onstart' actions
    runSoftwareActions(config, "input", "onstart")
    runSoftwareActions(config, "output", "onstart")
  }
  
  #copy data?
  if(copyData) entity$copyDataToJobDir(config)
  
  #runLocalActions?
  if(runLocalActions){
    if(!is.null(entity$data)) {
      if(entity$data$run){
        if(length(entity$data$actions)>0){
          for(i in 1:length(entity$data$actions)){
            entity_action <- entity$data$actions[[i]]
            config$logger.info(sprintf("Executing entity data action %s: '%s' ('%s')", i, entity_action$id, entity_action$script))
            entity_action$fun(entity, config, entity_action$options)
            assign("options", entity_action$options, envir = .GlobalEnv)
          }
          #we trigger entity enrichment (in case entity data action involved modification of entity)
          entity$enrichWithMetadata()
        }
      }else{
        config$logger.info("Execution of entity data actions is disabled")
      }
    }
  }
}