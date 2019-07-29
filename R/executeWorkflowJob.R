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
    config$logger.info("Executing workflow job...")
    
    #options
    skipFileDownload <- if(!is.null(config$options$skipFileDownload)) config$options$skipFileDownload else FALSE
  
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
            
            #cleaning in case Zenodo action is enabled with clean properties 
            withZenodo <- any(sapply(actions, function(x){x$id=="zen4R-deposit-record"}))
            if(withZenodo){
              ZENODO_CONFIG <- config$software$output$zenodo_config
              clean <- ZENODO_CONFIG$properties$clean
              if(!is.null(clean)) if(!is.null(clean$run)) if(as.logical(clean$run)){
                config$logger.info("Zenodo action 'clean' activated: deleting deposits prior to new deposits!")
                if(is.null(clean$query) 
                   & length(clean$doi)==0 
                   & length(clean$community)==0){
                  config$logger.info("Zenodo: no query or list of DOIs specified for cleaning: cleaning all draft deposits")
                  config$software$output$zenodo$deleteRecords()
                }else{
                  #selective cleaning by Zenodo ElasticSearch query
                  if(!is.null(clean$query)){
                    config$logger.info(sprintf("Zenodo: cleaning draft deposits based on query '%s'",clean$query))
                    config$software$output$zenodo$deleteRecords(q = clean$query)
                  }
                  #selective cleaning by prereserved DOI(s)
                  if(!is.null(clean$doi)){
                    config$logger.info(sprintf("Zenodo: cleaning draft deposits with prereserved DOIs [%s]", paste(clean$doi, collapse=",")))
                    invisible(lapply(clean$doi, config$software$output$zenodo$deleteRecordByDOI))
                  }
                  #selective cleaning by community(ies)
                  if(!is.null(clean$community)){
                    config$logger.info(sprintf("Zenodo: cleaning draft deposits in communities [%s]", paste(clean$community, collapse=",")))
                    invisible(lapply(clean$community, function(community){
                      config$software$output$zenodo$deleteRecords(q = sprintf("communities:%s", community))
                    }))
                  }
                }
              }
            }
            config$logger.info("Zenodo: sleeping 5 seconds...")
            Sys.sleep(5)
            
            for(entity in entities){
              
              #if entity has data we copy data to job data dir
              if(!is.null(entity$data) & !skipFileDownload) entity$copyDataToJobDir(config, jobdir)
              
              #run sequence of actions
              for(i in 1:length(actions)){
                action <- actions[[i]]
                config$logger.info(sprintf("Executing Action %s: %s - for entity %s", i, action$id, entity$identifiers[["id"]]))
                action$fun(entity, config, action$options)
              }
              #if zenodo is among actions, file upload (and possibly publish) to be managed here
              if(withZenodo){
                #if Zenodo is the only action then let's sleep to avoid latence issues when listing depositions
                #if no sleep is specified, getDepositions doesn't list yet the newly deposited recorded with
                #isIdenticalTo relationship
                if(length(actions)==1){
                  config$logger.info("Zenodo: sleeping 5 seconds...")
                  Sys.sleep(5)
                }
                zen_action <- actions[sapply(actions, function(x){x$id=="zen4R-deposit-record"})][[1]]
                act_options <- zen_action$options
                act_options$depositWithFiles <- TRUE
                zen_action$fun(entity, config, act_options)
              }
            }
            
            #in case Zenodo was enabled, we create an output table of DOIs & export altered source entities
            if(withZenodo){
              config$logger.info("Exporting reference list of DOIs to job directory")
              out_zenodo_dois <- do.call("rbind", lapply(entities, function(entity){
                return(data.frame(
                  Identifier = entity$identifiers[["id"]], 
                  Status = entity$status,
                  DOI_for_allversions = entity$identifiers[["conceptdoi_to_save"]],
                  DOI_for_version = entity$identifiers[["doi_to_save"]],
                  stringsAsFactors = FALSE
                ))
              }))
              write.csv(out_zenodo_dois, file = file.path(getwd(),"metadata", "zenodo_dois.csv"), row.names = FALSE)
          
              config$logger.info("Exporting source entities table enriched with DOIs")
              src_entities <- config$src_entities
              src_entities$Identifier <- sapply(1:nrow(src_entities), function(i){
                identifier <- src_entities[i, "Identifier"]
                if(!endsWith(identifier,";")) identifier <- paste0(identifier, ";\n")
                if(regexpr("doi", identifier)>0) return(identifier)
                if(out_zenodo_dois[i,"Status"] == "published") return(identifier)
                identifier <- paste0(identifier, "doi:", out_zenodo_dois[i,"DOI_for_allversions"])
                return(identifier)
              })
              write.csv(src_entities, file = file.path(getwd(),"metadata","zenodo_entities_with_doi_for_publication.csv"), row.names = FALSE)
            }
            
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
}