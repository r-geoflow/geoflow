#' @name executeWorkflowJob
#' @aliases executeWorkflowJob
#' @title executeWorkflowJob
#' @description \code{executeWorkflowJob} allows to execute a workflow job
#'
#' @usage executeWorkflowJob(config, jobdir, queue, monitor)
#'                 
#' @param config a configuration object as read by \code{initWorkflow}
#' @param jobdir the Job directory. Optional, by default inherited with the configuration.
#' @param queue an \pkg{ipc} queue to use geoflow in \pkg{geoflow-shiny}
#' @param monitor a monitor function to increase progress bar 
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'    
executeWorkflowJob <- function(config, jobdir = NULL, queue = NULL, monitor = NULL){
  
    if(is.null(jobdir)) jobdir <- config$job
  
    config$log_separator("=")
    cat("Workflow Execution\n")
    config$log_separator("=")
    config$logger.info("Executing workflow job...")
    
    #options
    skipDataDownload = FALSE
    if(!is.null(config$profile$options[["skipFileDownload"]])){
      config$logger.warn("Global option 'skipFileDownload' is deprecated, use 'skipDataDownload instead!")
      skipDataDownload = config$profile$options[["skipDataDownload"]]
    }
    skipDataDownload <- if(!is.null(config$profile$options[["skipDataDownload"]])) config$profile$options[["skipDataDownload"]] else FALSE
    skipEnrichWithDatatypes <- if(!is.null(config$profile$options[["skipEnrichWithDatatypes"]])) config$profile$options[["skipEnrichWithDatatypes"]] else FALSE
    skipEnrichWithData = if(!is.null(config$profile$options[["skipEnrichWithData"]])) config$profile$options[["skipEnrichWithData"]] else FALSE
    skipEnrichWithDataSubjects = if(!is.null(config$profile$options[["skipEnrichWithDataSubjects"]])) config$profile$options[["skipEnrichWithDataSubjects"]] else FALSE
    dataSubjectsToExclude = if(!is.null(config$profile$options[["dataSubjectsToExclude"]])) config$profile$options[["dataSubjectsToExclude"]] else c()
    
    #Actions onstart
    config$log_separator("-")
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
    config$log_separator("-")
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
        
        #TODO refactor actions in case they have specific behaviors to triggered early before entity iterator (eg. Zenodo cleaning)
        withZenodo <- any(sapply(actions, function(x){x$id=="zen4R-deposit-record"}))
        
        #cleaning in case Zenodo action is enabled with clean properties 
        if(withZenodo){
          ZENODO_CONFIG <- config$software$output$zenodo_config
          clean <- ZENODO_CONFIG$properties$clean
          if(!is.null(clean)) if(!is.null(clean$run)) if(as.logical(clean$run)){
            config$logger.info("Zenodo action 'clean' activated: deleting deposits prior to new deposits!")
            if(is.null(clean$query) && length(clean$doi)==0 && length(clean$community)==0){
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
        
        #Monitor incrementation
        nb_step<-length(entities) * length(config$actions) + sum(sapply(entities, function(entity){length(entity$data$actions)}))
        inc_step<-1/nb_step*100
        step=0
        
        for(entity in entities){
          
          #create entity jobdir
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
              #enrich with data types
              if(!skipEnrichWithDatatypes) entity$enrichWithDatatypes(config, jobdir)
              #vector data: we enrich entity with features
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
            
            #copyStylesToJobDir
            entity$copyStylesToJobDir(config)
            
            #extra identifiers to use in entity identification/actions
            entity$enrichWithIdentifiers(config)
            #data relations (eg. geosapi & OGC data protocol online resources)
            entity$enrichWithRelations(config)
            
            #data formats
            entity$enrichWithFormats(config)
            
            #data subjects
            if(!skipEnrichWithDataSubjects) entity$enrichWithSubjects(config, exclusions = dataSubjectsToExclude)
          }
          
          #enrich entities with metadata (other properties)
          entity$enrichWithMetadata(config)
          
          #enrich entities with vocabularies
          entity$enrichWithVocabularies(config)
          
          #run sequence of entity data actions (if any)
          if(!is.null(entity$data)) {
            if(entity$data$run){
              if(length(entity$data$actions)>0){
                for(i in 1:length(entity$data$actions)){
                  entity_action <- entity$data$actions[[i]]
                  config$logger.info(sprintf("Executing entity data action %s: '%s' ('%s')", i, entity_action$id, entity_action$script))
                  entity_action$run(entity, config)
                  #monitor local action
                  step<-step+inc_step
                  config$logger.info(sprintf("WORKFLOW PROGRESS : ACTION '%s' of ENTITY '%s' ... %s %%",entity_action$id,entity$identifiers[["id"]],step))
                  if(!is.null(monitor)) monitor(step=step,config=config,entity=entity,action=entity_action,queue=queue)
                }
                #we trigger entity enrichment (in case entity data action involved modification of entity)
                entity$enrichWithMetadata()
              }
            }else{
              config$logger.info("Execution of entity data actions is disabled")
            }
          }
          
          #Check existance of Workspace and create it if need
          geosapi <-sapply(actions, function(x){x$id=="geosapi-publish-ogc-services"})
          withGeosapi <- any(geosapi)
          
          if(withGeosapi){
            createWorkspace<-actions[geosapi][[1]]$getOption("createWorkspace")
    
            GS <- config$software$output$geoserver
            if(is.null(GS)){
              errMsg <- "This action requires a GeoServer software to be declared in the configuration"
              config$logger.error(errMsg)
              stop(errMsg)
            }
            
            GS_CONFIG <- config$software$output$geoserver_config
            workspace <- GS_CONFIG$properties$workspace
            if(!is.null(entity$data$workspaces$geoserver)) workspace <- entity$data$workspaces$geoserver
            if(is.null(workspace)){
              errMsg <- "The geoserver configuration requires a workspace for publishing action"
              config$logger.error(errMsg)
              stop(errMsg)
            }
              
            # Check existence of workspace
            ws <- GS$getWorkspace(workspace)
            # If workspace not exist
            # Check if createWorkspace is TRUE
            if(length(ws)==0){
              if(createWorkspace){
                created <- GS$createWorkspace(workspace, paste0("http://",workspace))
                if(created){
                  infoMsg <- sprintf("Successful Geoserver '%s' workspace creaction", workspace)
                  config$logger.info(infoMsg)
                }else{
                  errMsg <- "Error during Geoserver workspace creation. Aborting 'geosapi' action!"
                  config$logger.error(errMsg)
                  stop(errMsg)
                }
              }else{
                # If createWorkspace is FALSE edit ERROR Message
                errMsg <- sprintf("Workspace '%s' don't exist and createWorkspace option = FALSE, please verify config if workspace already exist or change createWorkpace = TRUE to create it",workspace)
                config$logger.error(errMsg)
                stop(errMsg)
              }
            }
          }
          
          #run sequence of global actions
          to_publish = FALSE
          if(length(actions)>0) for(i in 1:length(actions)){
            action <- actions[[i]]$clone(deep = TRUE)
            config$logger.info(sprintf("Executing Action %s: %s - for entity %s", i, action$id, entity$identifiers[["id"]]))
            if(action$id == "zen4R-deposit-record"){
              if(!is.null(action$options$publish)) if(action$options$publish){
                to_publish <- TRUE
                action$options$publish <- FALSE
              }
            }
            action$run(entity = entity, config = config)
            
            #monitor global action
            step<-step+inc_step
            config$logger.info(sprintf("WORKFLOW PROGRESS : ACTION '%s' of ENTITY '%s' ... %s %%",action$id,entity$identifiers[["id"]],step))
            if(!is.null(monitor)) monitor(step=step,config=config,entity=entity,action=action,queue=queue)
          }
          
          #search for generic uploader actions (eg. Zenodo, Dataverse)
          if(length(actions)>0) {
            generic_uploaders <- actions[sapply(actions, function(x){x$isGenericUploader()})]
            if(length(generic_uploaders)>0){
              for(i in 1:length(generic_uploaders)){
                generic_uploader = generic_uploaders[[i]]$clone(deep = TRUE)
                config$logger.info(sprintf("Last trigger for action '%s' (generic upload behavior)", generic_uploader$id))
                #For Zenodo: 
                #if Zenodo is the only action then let's sleep to avoid latence issues when listing depositions
                #if no sleep is specified, getDepositions doesn't list yet the newly deposited recorded with
                #isIdenticalTo relationship
                if(generic_uploader$id == "zen4R-deposit-record" & length(actions)==1){
                  config$logger.info("Zenodo: sleeping 5 seconds...")
                  Sys.sleep(5)
                }
                
                #behavior for generic uploaders, we set depositWithFiles = TRUE and proceed with all resource uploads
                generic_uploader_options <- generic_uploader$options
                generic_uploader_options$depositWithFiles <- TRUE
                if(generic_uploader$id == "zen4R-deposit-record"){
                  generic_uploader_options$publish <- to_publish
                }
                generic_uploader$options <- generic_uploader_options
                generic_uploader$run(entity, config)
              }
            }
          }
          
          entity$data$features <- NULL
          setwd(jobdir)
        }
        
        #special business logics in case of PID generators (eg. DOIs)
        if(length(actions)>0){
          pid_generators <- actions[sapply(actions, function(x){x$isPIDGenerator()})]
          if(length(pid_generators)>0){
            for(i in 1:length(pid_generators)){
              pid_generator = pid_generators[[i]]$clone(deep = TRUE)
              pid_generator$exportPIDs(config, entities)
            }
          }
        }
        
        #save source tabular entities,contacts and dictionaries used
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
        
        #save entities
        entities = config$getEntities()
        if(length(entities)>0){
          entities_df = do.call("rbind", lapply(entities, function(x){x$asDataFrame()}))
          readr::write_csv(entities_df, file.path(getwd(), "config_geoflow_entities.csv"))
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
    config$log_separator("-")
    config$logger.info("Executing software actions 'onend' ...")
    #run software 'onend' actions
    runSoftwareActions(config, "input", "onend")
    runSoftwareActions(config, "output", "onend")
    config$logger.info("Workflow successfully executed ...")
}