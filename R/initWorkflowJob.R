#' @name initWorkflowJob
#' @aliases initWorkflowJob
#' @title initWorkflowJob
#' @description \code{initWorkflowJob} allows to init a workflow job
#'
#' @usage initWorkflowJob(dir)
#'                 
#' @param dir a directory where to initialize/execute the workflow
#' 
#' @return the job directory path
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'   
initWorkflowJob <- function(dir = "."){
  mainDir <- tools::file_path_as_absolute(dir)
  subDir <- "jobs"
  if (!dir.exists(file.path(mainDir, subDir))){
    dir.create(file.path(mainDir, subDir))
  }
  #create job dir
  jobDir <- format(Sys.time(),paste0("%Y%m%d%H%M%S"))
  jobDirPath <- file.path(mainDir, subDir, jobDir)
  dir.create(jobDirPath)
  
  return(jobDirPath)
}