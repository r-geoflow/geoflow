#' @name initWorkflowJob
#' @aliases initWorkflowJob
#' @title initWorkflowJob
#' @description \code{initWorkflowJob} allows to init a workflow job
#'
#' @usage initWorkflowJob(config)
#'                 
#' @param config a configuration object as read by \code{initWorkflow}
#' 
#' @return the job directory path
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
  if (!dir.exists(file.path(mainDir, subDir))){
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
  config$logger.info(sprintf("Workflow job directory: %s", jobDirPath))
  
  #copy configuration file
  file.copy(from = config_file, to = getwd())
  #rename copied file
  file.rename(from = file.path(getwd(), basename(config_file)), to = "job.json")
  
  #create sub directories as listed in the configuration file
  job_targets <- sapply(config$actions, function(x){if(!is.na(x$target)) if(x$target=="job") return(x$target_dir)})
  job_targets <- job_targets[!sapply(job_targets,is.null)]
  directories <- unique(job_targets)
  directories <- directories[!is.na(directories)]
  for(directory in directories){
    if (!file.exists(directory)){
      dir_name <- file.path(jobDirPath, directory)
      config$logger.info(sprintf("Creating '%s' job directory: %s",directory, dir_name))
      dir.create(dir_name)
    }
  }
  
  if(config$profile$mode == "raw"){
    config$logger.info("Copying raw action scripts to job directory")
    for(action in config$actions){
      config$logger.info(sprintf("Copying %s ...", action$script))
      file.copy(from = file.path(config$wd, action$script), to = getwd())
    }
  }
  return(jobDirPath)
}