#' @name initWorkflowJob
#' @aliases initWorkflowJob
#' @title initWorkflowJob
#' @description \code{initWorkflowJob} allows to init a workflow job
#'
#' @usage initWorkflowJob(config)
#'                 
#' @param config a configuration object as read by \code{initWorkflow}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'   
initWorkflowJob <- function(config){
  config$logger.info("Init Workflow job directory")
  config$logger.info("========================================================================")
  config_file <- config$src
  mainDir <- config$wd
  subDir <- "jobs"
  if (!file.exists(subDir)){
    dir.create(file.path(mainDir, subDir))
  }
  setwd(file.path(mainDir, subDir))
  jobDir <- format(Sys.time(),paste0("%Y%m%d%H%M%S"))
  config$logger.info(sprintf("Initialize workflow job '%s'", jobDir))
  
  #create directories
  jobDirPath <- file.path(mainDir, subDir, jobDir)
  if (file.exists(subDir)){
    setwd(jobDirPath)
  } else {
    dir.create(jobDirPath)
    setwd(jobDirPath)
  }
  config$logger.info(sprintf("Workflow job directory:", jobDirPath))
  
  #copy configuration file
  file.copy(from = file.path(mainDir, config_file), to = getwd())
  #rename copied file
  file.rename(from = config_file, to = "job.json")
  
  #create sub directories as listed in the configuration file
  directories <- c("data", "metadata", "logs")
  for(directory in directories){
    if (!file.exists(directory)){
      config$logger.info(sprintf("Creating '%s' directory: %s",directory, 
                                 file.path(getwd(), directory)))
      dir.create(file.path(getwd(), directory))
    }
  }
  config$logger.info("Copying action scripts to job directory")
  for(task in config$tasks){
    config$logger.info(sprintf("Copying %s ...", task))
    file.copy(from = file.path(config$wd, task), to = getwd())
  }
}