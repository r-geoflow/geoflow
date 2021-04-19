#' @name executeWorkflowJob
#' @aliases executeWorkflowJob
#' @title executeWorkflowJob
#' @description \code{executeWorkflowJob} allows to execute a workflow job
#'
#' @usage executeWorkflowJob(config, jobdir)
#'                 
#' @param config a configuration object as read by \code{initWorkflow}
#' @param jobdir the Job directory. Optional, by default inherited with the configuration.
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'    
executeWorkflowJob <- function(config, jobdir = NULL){
  
    if(is.null(jobdir)) jobdir <- config$job
  
    config$logger.info("Executing workflow job...")
    
    #options
    skipFileDownload <- if(!is.null(config$profile$options$skipFileDownload)) config$profile$options$skipFileDownload else FALSE
  
    #Actions onstart
    config$logger.info("---------------------------------------------------------------------------------------")
    config$logger.info("Executing software actions 'onstart' ...")
    
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
    
    #workflow actions
    config$logger.info("---------------------------------------------------------------------------------------")
    config$logger.info("Executing entity actions ...")
    actions <- config$actions
    if(is.null(actions)){
      config$logger.warn("No actions enabled for this workflow!")
    }else{
      if(length(actions)==0){
        config$logger.warn("No actions enabled for this workflow!")
      }else{
        config$logger.info(sprintf("Workflow mode: %s", config$profile$mode))
        config$logger.info(sprintf("Workflow with %s actions", length(actions)))
        for(i in 1:length(actions)){config$logger.info(sprintf("Action %s: %s", i, actions[[i]]$id))}
      }
    }
    
    if(config$profile$mode == "entity"){
     
      #execute entity-based actions
      entities <- config$metadata$content$entities
      if(!is.null(entities)){
        
        #inspect if there are DOI-management related actions
        withZenodo <- any(sapply(actions, function(x){x$id=="zen4R-deposit-record"}))
        withDataverse <- any(sapply(actions, function(x){x$id=="atom4R-dataverse-deposit-record"}))
        
        #cleaning in case Zenodo action is enabled with clean properties 
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
            config$logger.info("Zenodo: sleeping 5 seconds...")
            Sys.sleep(5)    
          }
        }
        
        for(entity in entities){
          
          #create entity jobdir
          entity$prepareEntityJobDir(config, jobdir)
          #let's work in entity jobdir
          setwd(entity$getEntityJobDirPath(config, jobdir))
          config$logger.info(sprintf("Entity working directory: %s", getwd()))
          
          #enrich metadata with dynamic properties
          if(!is.null(entity$data)){
            #data features
            if(!skipFileDownload){
              #we copy data to job data dir
              entity$copyDataToJobDir(config, jobdir)
              #we enrich entity with features
              #control is added in case of entity already enriched with features (when loaded from custom R entity handlers)
              if(is.null(entity$data$features)) entity$enrichWithFeatures(config, jobdir)
              setwd(entity$getEntityJobDirPath(config, jobdir)) #make sure we are in entity jobdir
            }
            #data relations (eg. geosapi & OGC data protocol online resources)
            entity$enrichWithRelations(config)
          }
          
          #enrich entities with metadata (other properties)
          entity$enrichWithMetadata(config)
          
          #run sequence of entity data actions (if any)
          if(!is.null(entity$data)) {
            if(entity$data$run){
              if(length(entity$data$actions)>0){
                for(i in 1:length(entity$data$actions)){
                  entity_action <- entity$data$actions[[i]]
                  config$logger.info(sprintf("Executing entity data action %s: '%s' ('%s')", i, entity_action$id, entity_action$script))
                  entity_action$fun(entity, config, entity_action$options)
                }
                #we trigger entity enrichment (in case entity data action involved modification of entity)
                entity$enrichWithMetadata()
              }
            }else{
              config$logger.info("Execution of entity data actions is disabled")
            }
          }
          
          #run sequence of global actions
          if(length(actions)>0) for(i in 1:length(actions)){
            action <- actions[[i]]
            config$logger.info(sprintf("Executing Action %s: %s - for entity %s", i, action$id, entity$identifiers[["id"]]))
            action$fun(entity = entity, config = config, options = action$options)
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
          
          #if atom4R/dataverse is among actions, file upload (and possibly publish) to be managed here
          if(withDataverse){
            dv_action <- actions[sapply(actions, function(x){x$id=="atom4R-dataverse-deposit-record"})][[1]]
            act_options <- dv_action$options
            act_options$depositWithFiles <- TRUE
            dv_action$fun(entity, config, act_options)
          }
          
          entity$data$features <- NULL
          setwd(jobdir)
        }
        
        #special business logics in case of PID generators (eg. DOIs)
        if(length(actions)>0){
          pid_generators <- actions[sapply(actions, function(x){x$isPIDGenerator()})]
          if(length(pid_generators)>0){
            for(pid_generator in pid_generators){
              pid_generator$exportPIDs(config, entities)
            }
          }
        }
        
        #save entities,contacts and dictionaries used
        if(length(config$src_entities)>0){
          for(i in 1:length(config$src_entities)){
            readr::write_csv(config$src_entities[[i]], file.path(getwd(), sprintf("config_copyof_entities_%s.csv", i)))
          }
        }
        if(length(config$src_contacts)>0){
          for(i in 1:length(config$src_contacts)){
            readr::write_csv(config$src_contacts[[i]], file.path(getwd(), sprintf("config_copyof_contacts_%s.csv", i)))
          }
        }
        if(length(config$src_dictionary)>0){
          for(i in 1:length(config$src_dictionary)){
            readr::write_csv(config$src_dictionary[[i]], file.path(getwd(), sprintf("config_copyof_dictionary_%s.csv", i)))
          }
        }
      }
    }else if(config$profile$mode == "raw"){
      #execute raw actions (--> not based on metadata entities)
      for(i in 1:length(actions)){
        action <- actions[[i]]
        config$logger.info(sprintf("Executing Action %s: %s", i, action$id))
        eval(expr = parse(action$script))
      }
    }
    
    #Actions onend
    config$logger.info("---------------------------------------------------------------------------------------")
    config$logger.info("Executing software actions 'onend' ...")
    #run software 'onend' actions
    runSoftwareActions(config, "input", "onend")
    runSoftwareActions(config, "output", "onend")
}