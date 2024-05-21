#' @name debugWorkflow
#' @aliases debugWorkflow
#' @title debugWorkflow
#' @description \code{debugWorkflow} allows to initiate a workflow job for
#' developers to work / debug on workflow actions.
#'
#' @usage debugWorkflow(file, dir, entityIndex, copyData, runSoftwareActions, runLocalActions)
#'                 
#' @param file configuration file
#' @param dir directory where to debug/execute the workflow
#' @param entityIndex index of the entity within the list of loaded entities. Default is 1
#' @param copyData whether data should be downloaded/copied to job data directory.
#' Default is \code{TRUE}.
#' @param runSoftwareActions whether software actions should be run. Default is \code{TRUE}.
#' @param runLocalActions whether entity data local actions (if any) should be run.
#' Default is \code{TRUE}
#' 
#' @return a named list including the workflow \code{config}, the selected \code{entity} and
#' the eventual \code{options} associated to the entity.
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
debugWorkflow <- function(file, dir = ".", entityIndex = 1, 
                          copyData = TRUE, 
                          runSoftwareActions = TRUE,  
                          runLocalActions = TRUE){
  
  wd <- getwd()
  if(!is.null(dir)) setwd(dir)
  
  #options
  .defaultOptions <- options()
  options(stringsAsFactors = FALSE)
  
  #1. Init the workflow based on configuration file
  jobDirPath <- initWorkflowJob(dir = dir)
  setwd(wd)
  config <- initWorkflow(file, dir = dir, jobDirPath = jobDirPath)
  setwd(jobDirPath)
  config$debug <- TRUE
  .geoflow$debug = list()
  .geoflow$debug$config = config
  
  #entities
  entities <- config$getEntities()
  entity <- entities[[entityIndex]]
  .geoflow$debug$entity <- entity

  #skipDataDownload
  skipDataDownload = FALSE
  if(!is.null(config$profile$options$skipFileDownload)){
    config$logger.warn("Global option 'skipFileDownload' is deprecated, use 'skipDataDownload instead!")
    skipDataDownload = config$profile$options$skipFileDownload
  }
  skipDataDownload <- if(!is.null(config$profile$options$skipDataDownload)) config$profile$options$skipDataDownload else FALSE
  skipEnrichWithData = if(!is.null(config$profile$options$skipEnrichWithData)) config$profile$options$skipEnrichWithData else FALSE
  skipEnrichWithDatatypes = if(!is.null(config$profile$options$skipEnrichWithDatatypes)) config$profile$options$skipEnrichWithDatatypes else FALSE
  
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
  
  #create entity jobdir
  jobdir = config$job
  entity$prepareEntityJobDir(config, jobdir)
  #let's work in entity jobdir
  setwd(entity$getEntityJobDirPath(config, jobdir))
  config$logger.info(sprintf("Entity working directory: %s", getwd()))

  #enrich metadata with dynamic properties
  if(!is.null(entity$data)){
    #data features/coverages
    if(!skipDataDownload){
      config$logger.info("SkipDataDownload is false: copying and fetching data...")
      #we copy data to job data dir (for data files)
      entity$copyDataToJobDir(config, jobdir)
      #vector data: we enrich entity with features
      #enrich with data types
      if(!skipEnrichWithDatatypes) entity$enrichWithDatatypes(config, jobdir)
      #control is added in case of entity already enriched with features/coverages (when loaded from custom R entity handlers)
      if(!skipEnrichWithData) if(is.null(entity$data$features) && is.null(entity$data$coverages)){
        entity$enrichWithData(config, jobdir)
      }
      setwd(entity$getEntityJobDirPath(config, jobdir)) #make sure we are in entity jobdir
      #we check if the source and upload are both different file format (csv,shp,gpkg) and process automatically to conversion from source to upload type
      entity$prepareFeaturesToUpload(config)
    }else{
      config$logger.info("SkipDataDownload is true:")
      if(!skipEnrichWithData){
        config$logger.info("SkipEnrichWithData is false: Fetching spatial coverage from data (for DB sources only)...")
        #alternative behaviors in case we don't download data, applies to DB only
        entity$enrichSpatialCoverageFromDB(config)
      }else{
        config$logger.info("SkipEnrichWithData is true")
      }
    }
    #extra identifiers to use in entity identification/actions
    entity$enrichWithIdentifiers(config)
    #data relations (eg. geosapi & OGC data protocol online resources)
    entity$enrichWithRelations(config)
    
    #data formats
    entity$enrichWithFormats(config)
    
    #data subjects
    entity$enrichWithSubjects(config)
  }
  
  #runLocalActions?
  if(runLocalActions){
    if(!is.null(entity$data)) {
      if(entity$data$run){
        if(length(entity$data$actions)>0){
          for(i in 1:length(entity$data$actions)){
            entity_action <- entity$data$actions[[i]]
            config$logger.info(sprintf("Executing entity data action %s: '%s' ('%s')", i, entity_action$id, entity_action$script))
            entity_action$run(entity, config)
            .geoflow$debug$options <- entity_action$options
          }
          #we trigger entity enrichment (in case entity data action involved modification of entity)
          entity$enrichWithMetadata()
        }
      }else{
        config$logger.info("Execution of entity data actions is disabled")
      }
    }
  }
  
  return(.geoflow$debug)
  
}