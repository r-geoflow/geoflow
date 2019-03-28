#' @name executeWorkflowJob
#' @aliases executeWorkflowJob
#' @title executeWorkflowJob
#' @description \code{executeWorkflowJob} allows to execute a workflow job
#'
#' @usage executeWorkflowJob(config)
#'                 
#' @param config a configuration object as read by \code{initWorkflow}
#' @param jobdir the Job directory
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'    
executeWorkflowJob <- function(config, jobdir){
  capture.output({
    config$logger.info("Executing workflow job...")
    actions <- config$actions
    if(is.null(actions)){
      config$logger.warn("No actions enabled for this workflow!")
    }else{
      if(length(actions)==0){
        config$logger.warn("No actions enabled for this workflow!")
      }else{
        config$logger.info(sprintf("Workflow mode: %s", config$mode))
        config$logger.info(sprintf("Workflow with %s actions", length(actions)))
        for(i in 1:length(actions)){config$logger.info(sprintf("Action %s: %s", i, actions[[i]]$id))}
        
        if(config$mode == "entity"){
          #execute entity-based actions
          entities <- config$metadata$content$entities
          if(!is.null(entities)){
            invisible(lapply(entities, function(entity){
              
              #if entity has data we copy data to job data dir
              if(!is.null(entity$data)) entity$copyDataToJobDir(config, jobdir)
              
              #run sequence of actions
              for(i in 1:length(actions)){
                action <- actions[[i]]
                config$logger.info(sprintf("Executing Action %s: %s - for entity %s", i, action$id, entity$identifiers[["id"]]))
                action$fun(entity, config, action$options)
              }
              #if zenodo is among actions, file upload (and possibly publish) to be managed here
              withZenodo <- sapply(actions, function(x){x$id=="zen4R-deposit-record"})
              if(any(withZenodo)){
                zen_action <- actions[withZenodo][[1]]
                act_options <- actions$options
                act_options$depositWithFiles <- TRUE
                zen_action$fun(entity, config, act_options)
              }
              
            }))
          }
        }else if(config$mode == "raw"){
          #execute raw actions (--> not based on metadata entities)
          for(i in 1:length(actions)){
            action <- actions[[i]]
            config$logger.info(sprintf("Executing Action %s: %s", i, action$id))
            eval(expr = parse(action$script))
          }
        }
      }
    }
  },file = file.path(getwd(), "job-logs.txt"))
}