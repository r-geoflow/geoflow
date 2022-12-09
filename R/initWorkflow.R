#' @name initWorkflow
#' @aliases initWorkflow
#' @title initWorkflow
#' @description \code{initWorkflow} allows to init a workflow
#'
#' @usage initWorkflow(file, dir, jobDirPath, handleMetadata, session)
#'                 
#' @param file a JSON configuration file
#' @param dir a directory where to execute the workflow
#' @param jobDirPath a directory set-up for the job. Default is \code{NULL} means it will be created
#'  during initialization of the workflow, otherwise the path provided will be used.
#' @param handleMetadata Default is \code{TRUE}. Metadata contacts/entities/dictionary will be handled.
#'   If set to \code{FALSE}, they will not be handled. This is used for example in geoflow Shiny app
#'   where we want to initialize config without handling metadata to inherit software connections and 
#'   test dynamically the metadata validity.
#' @param session a \pkg{shiny} session object (optional) to run geoflow in a \pkg{shiny} context
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
initWorkflow <- function(file, dir = ".", jobDirPath = NULL, handleMetadata = TRUE, session = NULL){

  #optional shiny session object
  if(!is.null(session)) if(!is(session, "ShinySession")){
    stop("The 'session' argument should specify an object of class 'ShinySession'")
  }
  
  #file/config
  file <- tools::file_path_as_absolute(file)
  config <- jsonlite::read_json(file)
  
  #keep the source
  config$src <- file
  config$src_config <- config
  
  #worfklow config$loggers
  config <- add_config_utils(config)
  
  cat("Session info\n")
  config$log_separator("=")
  print(sessionInfo())
  config$log_separator("=")
  cat("Workflow initialization\n")
  config$log_separator("=")
  config$logger.info("Init Workflow configuration")
  
  config_file <- config$src
  #working dir (where jobs will be created)
  if(is.null(config$wd)) config$wd <- tools::file_path_as_absolute(dir)
  if(is.null(jobDirPath)) jobDirPath <- initWorkflowJob(dir = dir)
  config$job <- jobDirPath
  config$logger.info(sprintf("Workflow job directory: %s", jobDirPath))
  
  #copy configuration file
  wd <- getwd()
  setwd(jobDirPath)
  file.copy(from = config_file, to = getwd())
  #rename copied file
  file.rename(from = file.path(getwd(), basename(config_file)), to = "job.json")
  setwd(wd)
  
  #profile
  profile <- NULL
  if(!is.null(config$profile)){
    config$logger.info("Creating workflow profile...")
    profile <- geoflow_profile$new()
    #identifier
    if(!is.null(config$profile$id)){
      profile$setId(config$profile$id)
    }else{
      config$logger.warn("Configuration file TO UPDATE: 'id' should be defined in profile!")
      profile$setId(config$id)
    }
    config$logger.info(sprintf("Workflow ID: %s", profile$id))
    #other workflow metadata
    if(!is.null(config$profile$name)){
      profile$setName(config$profile$name)
      config$logger.info(sprintf("Workflow name: %s", profile$name))
    }
    if(!is.null(config$profile$project)){
      profile$setProject(config$profile$project)
      config$logger.info(sprintf("Workflow project: %s", profile$project))
    }
    if(!is.null(config$profile$organization)){
      profile$setOrganization(config$profile$organization)
      config$logger.info(sprintf("Workflow organization: %s", profile$organization))
    }
    if(!is.null(config$profile$logos)){
      for(logo in config$profile$logos) profile$addLogo(logo)
    }
    #workflow mode
    cfg_mode <- NULL
    if(!is.null(config$profile$mode)){
      cfg_mode <- config$profile$mode
    }else{
      config$logger.warn("Configuration file TO UPDATE: 'mode' should be defined in profile!")
      cfg_mode <- config$mode
    }
    if(!is.null(cfg_mode)){
      allowedModes <- c("raw","entity")
      if(!(cfg_mode %in% allowedModes)) {
        errMsg <- sprintf("The workflow '%s' mode is incorrect. Allowed values are [%s]",
                          cfg_mode, paste(allowedModes, collapse=","))
        config$logger.error(errMsg)
        stop(errMsg)
      }
      profile$mode <- cfg_mode
    }else{
      warnMes <- "No workflow mode specified, 'raw' mode specified by default!"
      config$logger.warn(warnMes)
      profile$mode <- "raw"
    }
    #environment
    if(!is.null(config$profile$environment)) if(!is.null(config$profile$environment$file)){
      config$logger.info(sprintf("Loading environment from env file '%s'", basename(config$profile$environment$file)))
      env_vars_before <- as.list(Sys.getenv())
      config$session_env <- env_vars_before
      loaded <- try(dotenv::load_dot_env(file = config$profile$environment$file))
      if(is(loaded,"try-error")){
        errMsg <- sprintf("Error while trying to load environment from env file '%s'", basename(config$profile$environment$file))
        config$logger.error(errMsg)
        stop(errMsg)
      }else{
        env_vars_after <- as.list(Sys.getenv())
        env_vars <- setdiff(env_vars_after, env_vars_before)
        config$logger.info("Workflow environment:")
        hide_env_vars <- c("PASSWORD", "PWD", "TOKEN")
        if(!is.null(config$profile$environment$hide_env_vars)) hide_env_vars <- unlist(config$profile$environment$hide_env_vars)
        for(env_var_name in names(env_vars)){
          env_var_value <- env_vars[[env_var_name]]
          if(any(sapply(hide_env_vars, regexpr, env_var_name)>0)) env_var_value <- "**********"
          config$logger.info(sprintf("* %s = %s", env_var_name, env_var_value))
        }
      }
    }
    #options
    cfg_options <- NULL
    if(!is.null(config$profile$options)){
      cfg_options <- config$profile$options
    }else{
      config$logger.warn("Configuration file TO UPDATE: 'options' should be defined in profile!")
      cfg_options <- config$options
    }
    config$logger.info("Setting geoflow global options...")
    config$profile$options <- cfg_options
    if(!is.null(config$profile$options$line_separator)){
      config$logger.info(sprintf("Setting option 'line_separator' to '%s'", config$profile$options$line_separator))
      set_line_separator(config$profile$options$line_separator)
    }
    for(option_name in names(config$profile$options)){
      profile$setOption(option_name, config$profile$options[[option_name]])
    }
  }
  
  #session_wd
  config$session_wd <- getwd()

  #load source scripts
  #--------------------
  source_scripts <- config$dependencies$scripts
  if(length(source_scripts)>0){
    config$logger.info("Loading R scripts...")
    invisible(sapply(source_scripts,function(script){
      config$logger.info(sprintf("Loading R script '%s'...", script))
      source(script)
    }))
  }
  
  #load environment
  config <- load_workflow_environment(config, session)
  
  #set profile (R6)
  config$profile_config <- config$profile
  if(!is.null(profile)) config$profile <- profile
  
  #software components
  if(!is.null(config$software)){
    
    supportedSoftware <- list_software(raw = TRUE)
    
    software_configs <- config$software
    
    config$software <- list()
    config$software$input <- list()
    config$software$output <- list()
    
    for(software in software_configs){
      if(is.null(software$id)){
        errMsg <- "Sofware 'id' is missing. Please make sure to give an id to all declared software"
        config$logger.info(errMsg)
        stop(errMsg)
      }
      if(is.null(software$type)){
        errMsg <- "Sofware 'type' is missing. Please make sure to specify a type ('input' or 'output') to all declared software"
        config$logger.info(errMsg)
        stop(errMsg)
      }
      if(!(software$type %in% c("input","output"))){
        errMsg <- sprintf("Incorrect type value (%s') for software '%s'", software$type, software$id)
      }
      
      #embedded software or custom?
      embeddedSoftware <- is.null(software$handler)
      if(embeddedSoftware){
        if(is.null(software$software_type)){
          errMsg <- sprintf("The 'software_type' is missing for software '%s'", software$id)
          config$logger.info(errMsg)
          stop(errMsg)
        }
      }
      
      if(!(software$software_type %in% sapply(supportedSoftware, function(x){x$software_type})) & embeddedSoftware){
        errMsg <- sprintf("Embedded Software type '%s' not supported by geoflow. Check the list of embedded software with R code: list_software()", software$software_type)
        config$logger.error(errMsg)
        stop(errMsg)
      }
      client <- NULL
      if(embeddedSoftware){
        target_software <- supportedSoftware[sapply(supportedSoftware, function(x){x$software_type == software$software_type})][[1]]
        config$logger.info(sprintf("Configuring %s software '%s' (%s)", software$type, software$id, software$software_type))
        target_software$setId(software$id)
        target_software$setType(software$type)
        if(!is.null(software$parameters)) target_software$setParameters(unlist(software$parameters))
        
        #check software dependencies
        target_software$checkPackages()
        
        #get handler instance
        client <- target_software$getHandlerInstance()
        software$actions <- target_software$actions
      }else{
        client_handler <- eval(parse(text=software$handler))
        if(is(client_handler,"try-error")){
          errMsg <- sprintf("Error while evaluating software handler '%s'", software$handler)
          config$logger.error(errMsg)
          stop(errMsg)
        }
        client_params <- unlist(software[names(software)!="handler"])
        client <- client_handler(client_params)
      }
      if(!is.null(config$software[[software$type]][[switch(software$type,"input"=software$id,"output"=software$software_type)]])){
        if(software$type=="input") errMsg <- sprintf("An input software with id '%s' has been already declared!", software$id)
        if(software$type=="output") errMsg <- sprintf("An output software with software type '%s' has been already declared!", software$software_type)
        config$logger.error(errMsg)
        stop(errMsg)
      }
      config$software[[software$type]][[software$software_type]] <- if(!is.null(client)) client else software #return config in case software handler has no return
      config$software[[software$type]][[paste(software$software_type,"config",sep="_")]] <- software
    }
  }
  
  if(length(config$registers)==0) config$registers <- list()
  config_registers <- config$registers #store eventual config$registers
  
  #loading dictionary
  #metadata elements
  if(!is.null(config$metadata)){
    if(is.null(config$metadata$content)) config$metadata$content <- list()
    
    #metadata dictionary
    cfg_md_dictionary <- config$metadata$dictionary
    if(!is.null(cfg_md_dictionary)){
      #manage dictionary handlers as array/object as backward compatibility for object
      isarray_dictionary <- length(names(cfg_md_dictionary))==0
      if(!isarray_dictionary){
        config$metadata$dictionary <- list(config$metadata$dictionary)
        cfg_md_dictionary <- config$metadata$dictionary
      }
      
      #collating data structures (feature types) from handlers
      config$logger.info("Loading dictionary data structures...")
      config$src_dictionary <- list()
      dicts <- lapply(cfg_md_dictionary, function(x){
        config$logger.info(sprintf("Loading data structure definitions from '%s' [with '%s' handler]...", 
                                   x$source, x$handler))
        
        md_dict_handler <- loadMetadataHandler(config, x, type = "dictionary")
        config$logger.info("Execute handler to load dictionary data structures...")
        dict <- md_dict_handler(config, source = x$source)
        
        if(!is(dict, "geoflow_dictionary")){
          errMsg <- "The output of the dictionary handler should return an object of class 'geoflow_dictionary'"
          config$logger.error(errMsg)
          stop(errMsg)
        }
        
        #keep source dictionary part of the config
        config$src_dictionary[[length(config$src_dictionary)+1]] <<- attr(dict, "source")
        return(dict)
      })
      #build single top-level dictionary
      dictionary <- geoflow_dictionary$new()
      for(dict in dicts){
        for(ft in dict$featuretypes){
          if(!ft$id %in% sapply(dictionary$featuretypes,function(x){x$id})) dictionary$addFeatureType(ft)
        }
        for(reg in dict$registers){
          if(!reg$id %in% sapply(dictionary$registers,function(x){x$id})) dictionary$addRegister(reg)
        }
      }
      if(!is(dictionary, "geoflow_dictionary")){
        errMsg <- "The output of the dictionary handler should return an object of class 'geoflow_dictionary'"
        config$logger.error(errMsg)
        stop(errMsg)
      }
      
      config$logger.info("Successfuly fetched dictionary !")
      config$metadata$content$dictionary <- dictionary
      config$registers <- dictionary$getRegisters()
      if(length(config$registers)==0) config$registers <- list()
    }
  }
  
  #registers
  #registers can be configured either through config or through dictionnary
  if(!is.null(config_registers)){
    fetched_registers <- list()
    if(length(config_registers)>0){
      for(reg in config_registers){
        register_to_fetch <- NULL
        isCustom <- FALSE
        if(!is.null(reg$script)){
          isCustom <- TRUE
        }
        
        if(!isCustom){
          if(is.null(reg$id)){
            errMsg <- "An 'register' should have an id. Please check your configuration file. In case of a custom register, the id should be the function name."
            config$logger.error(errMsg)
            stop(errMsg)
          }
          available_registers <- list_registers(raw=TRUE)
          available_register_ids <- sapply(available_registers, function(x){x$id})
          if(!(reg$id %in% available_register_ids)){
            stop(sprintf("The register '%s' is not among available geoflow registers", reg$id))
          }
          register_to_fetch <- available_registers[[1]]
        }else{
          source(reg$script)
          customfun <- eval(parse(text = reg$id))
          if(is(customfun,"try-error")){
            errMsg <- sprintf("Error while trying to evaluate custom function '%s", reg$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          if(!is(customfun,"function")){
            errMsg <- sprintf("'%s' is not a function!", reg$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          register_to_fetch <- geoflow_register$new(
            id = reg$id, 
            def = reg$def, 
            fun = customfun
          )
        }
        if(!is.null(register_to_fetch)) register_to_fetch$fetch(config)
        
        if(!(reg$id %in% sapply(fetched_registers, function(x){x$id}))){
          fetched_registers <- c(fetched_registers, register_to_fetch)
        }
        
      }
      if(all(sapply(config$registers, function(x){class(x)[1] == "geoflow_register"}))){
        config$registers <- c(config$registers, fetched_registers)
      }else{
        config$registers <- fetched_registers
      }
    }
  }
  
  #metadata elements
  if(handleMetadata) if(!is.null(config$metadata)){
    config$logger.info("Loading metadata elements...")
    if(is.null(config$metadata$content)) config$metadata$content <- list()
    
    #metadata contacts
    cfg_md_contacts <- config$metadata$contacts
    if(!is.null(cfg_md_contacts)){
      #manage contact handlers as array/object as backward compatibility for object
      isarray_contacts <- length(names(cfg_md_contacts))==0
      if(!isarray_contacts){
        config$metadata$contacts <- list(config$metadata$contacts)
        cfg_md_contacts <- config$metadata$contacts
      }
      #collating contacts from contact handlers
      config$logger.info("Loading metadata contacts...")
      config$src_contacts <- list()
      contacts <- do.call("c", lapply(cfg_md_contacts, function(x){
        config$logger.info(sprintf("Loading metadata contacts from '%s' [with '%s' handler]...", 
                                   x$source, x$handler))
        md_contact_handler <- loadMetadataHandler(config, x, type = "contacts")
        config$logger.info("Execute contact handler to load contacts...")
        contacts <- md_contact_handler(config, source = x$source)
        
        if(!is(contacts, "list") | !all(sapply(contacts, is, "geoflow_contact"))){
          errMsg <- "The output of the contacts handler should return a list of objects of class 'geoflow_entity_contact'"
          config$logger.error(errMsg)
          stop(errMsg)
        }
        
        #keep source contacts part of the config
        config$src_contacts[[length(config$src_contacts)+1]] <<- attr(contacts, "source")
        return(contacts)
      }))
      
      config$logger.info(sprintf("Successfuly fetched %s contacts!",length(contacts)))
      config$metadata$content$contacts <- contacts
      config$logger.info(sprintf("Successfuly loaded %s contacts!",length(contacts)))
    }
    
    #metadata entities
    cfg_md_entities <- config$metadata$entities
    if(!is.null(cfg_md_entities)){
      #manage entity handlers as array/object as backward compatibility for object
      isarray_entities <- length(names(cfg_md_entities))==0
      if(!isarray_entities){
        config$metadata$entities <- list(config$metadata$entities)
        cfg_md_entities <- config$metadata$entities
      }
      #collating entities from entity handlers
      config$logger.info("Loading metadata entities...")
      config$src_entities <- list()
      entities <- do.call("c", lapply(cfg_md_entities, function(x){
        config$logger.info(sprintf("Loading metadata entities from '%s' [with '%s' handler]...", 
                                   x$source, x$handler))
        md_entity_handler <- loadMetadataHandler(config, x, type = "entities")
        config$logger.info("Execute handler to load entities...")
        entities <- md_entity_handler(config, source = x$source)
        
        if(!is(entities, "list") | !all(sapply(entities, is, "geoflow_entity"))){
          errMsg <- "The output of the entities handler should return a list of objects of class 'geoflow_entity'"
          config$logger.error(errMsg)
          stop(errMsg)
        }
      
        #keep source entities part of the config
        config$src_entities[[length(config$src_entities)+1]] <<- attr(entities, "source")
        return(entities)
      }))
        
      config$logger.info(sprintf("Successfuly fetched %s entities!",length(entities)))
      if(!is.null(config$metadata$content$contacts)){
        config$logger.info("Enrich metadata entities from directory of contacts")
        directory_of_contacts <- config$metadata$content$contacts
        #enrich entity contacts from contacts directory
        entities <- lapply(entities, function(entity){
          newentity <- entity$clone()
          newentity$contacts <- lapply(entity$contacts, function(contact){
            newcontact <- NULL
            if(is(contact,"geoflow_contact")){
              id <- contact$identifiers[["id"]]
              role <- contact$role
              contact_from_directory <- directory_of_contacts[sapply(directory_of_contacts, function(x){id %in% x$identifiers})]
              if(!all(is.null(contact_from_directory))){
                if(length(contact_from_directory)>0){
                  if(length(contact_from_directory)>1 & length(unique(sapply(contact_from_directory, function(x){x$role})))>1){
                    config$logger.warn("Warning: 2 contacts identified with same id/role! Check your contacts")
                  }
                  contact_from_directory <- contact_from_directory[[1]]
                  newcontact <- contact_from_directory$clone(deep=TRUE)
                  newcontact$setIdentifier(key = "id", id)
                  newcontact$setRole(role)
                }
              }else{
                config$logger.warn(sprintf("Warning: contact %s is not registered in directory! Contact will be ignored!", id))
              }
            }
            return(newcontact)
          })
          newentity$contacts <- newentity$contacts[!sapply(newentity$contacts, is.null)]
          
          #we look at data provenance
          if(!is.null(entity$provenance)) if(is(entity$provenance, "geoflow_provenance")){
            newprov <- entity$provenance$clone()
            if(length(entity$provenance$processes)>0){
              newprov$processes <- lapply(entity$provenance$processes, function(process){
                newprocess <- process$clone()
                processor <- process$processor
                if(!is.null(processor)){
                  processor_from_directory <- directory_of_contacts[sapply(directory_of_contacts, function(x){processor$identifiers[["id"]] %in% x$identifiers})]
                  if(length(processor_from_directory)>0){
                    processor_from_directory <- processor_from_directory[[1]]
                    new_processor <- processor_from_directory
                    new_processor$setIdentifier(key = "id", processor$identifiers[["id"]])
                    new_processor$setRole("processor")
                    newprocess$setProcessor(new_processor)
                  }
                }
                return(newprocess)
              })
            }
            newentity$setProvenance(newprov)
          }
          
          return(newentity)
        })
      }
      config$metadata$content$entities <- entities
      config$logger.info(sprintf("Successfuly loaded %s entities!",length(entities)))
    }
    
  }
  
  #add function to get easiy metadata elements
  config$getDictionary <- function(){
    return(config$metadata$content$dictionary)
  }
  config$getEntities <- function(){
    return(config$metadata$content$entities)
  }
  config$getContacts = function(){
    return(config$metadata$content$contacts)
  }
  
  #Actions
  if(!is.null(config$actions)){
    
    config$actions <- lapply(config$actions,function(action){
      if(!action$run) return(NULL)
      
      action_to_trigger <- NULL
      isCustom <- FALSE
      if(!is.null(action$script)){
        isCustom <- TRUE
      }
      if(!isCustom){
        if(is.null(action$id)){
          errMsg <- "An 'action' should have an id. Please check your configuration file. In case of a custom action, the id should be the function name."
          config$logger.error(errMsg)
          stop(errMsg)
        }
        #we try to find it among embedded actions
        available_actions <- list_actions(raw=TRUE)
        available_action_ids <- sapply(available_actions, function(x){x$id})
        if(!(action$id %in% available_action_ids)){
          stop(sprintf("The action '%s' is not among available geoflow actions", action$id))
        }
        action_to_trigger <- .geoflow$actions[sapply(.geoflow$actions, function(x){x$id==action$id})][[1]]$clone(deep=TRUE)
        
        #check action dependencies
        action_to_trigger$checkPackages()
        
        #options
        if(length(action$options)>0) if(!all(names(action$options) %in% names(action_to_trigger$available_options))){
          errMsg <- sprintf("Option(s) [%s] invalid for action '%s'", paste0(setdiff(names(action$options), names(action_to_trigger$available_options)), collapse=","), action$id)
          config$logger.error(errMsg)
          stop(errMsg)
        }
        action_to_trigger$options <- action$options
      }else{
        if(config$profile$mode == "entity"){
          customfun <- source(action$script)$value
          if(is(customfun,"try-error")){
            errMsg <- sprintf("Error while trying to evaluate custom function'%s", action$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          if(!is(customfun,"function")){
            errMsg <- sprintf("'%s' is not a function!", action$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          funparams <- unlist(names(formals(customfun)))
          if(!("entity" %in% funparams)){
            config$logger.warn(sprintf("Action '%s' - Custom action arguments: [%s]", action$id, paste(funparams, collapse=",")))
            errMsg <- sprintf("Missing parameter 'entity' in function '%s'", action$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          if(!("config" %in% funparams)){
            config$logger.warn(sprintf("Custom action arguments: [%s]", paste(funparams, collapse=",")))
            errMsg <- sprintf("Missing parameter 'config' in function '%s'", action$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          if(!("options" %in% funparams)){
            config$logger.warn(sprintf("Custom action arguments: [%s]", paste(funparams, collapse=",")))
            errMsg <- sprintf("Missing parameter 'options' in function '%s'", action$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          action_to_trigger <- geoflow_action$new(
            id = action$id,
            type = action$type,
            def = action$def,
            fun = customfun,
            options = action$options
          )
        }else if(config$profile$mode == "raw"){
          action_to_trigger <- geoflow_action$new(
            id = action$script,
            type = action$type,
            def = action$def,
            script = action$script,
            options = action$options
          )
        }
      }
      return(action_to_trigger)
    })
    config$actions <- config$actions[!sapply(config$actions, is.null)]
    
  }
  
  #create sub directories as listed in the configuration file
  job_targets <- sapply(config$actions, function(x){if(!is.na(x$target)) if(x$target=="job") return(x$target_dir)})
  job_targets <- job_targets[!sapply(job_targets,is.null)]
  directories <- unique(job_targets)
  directories <- directories[!is.na(directories)]
  for(directory in directories){
    if (!file.exists(directory)){
      dir_name <- file.path(config$job, directory)
      config$logger.info(sprintf("Creating '%s' job directory: %s",directory, dir_name))
      dir.create(dir_name)
    }
  }
  
  if(config$profile$mode == "raw"){
    config$logger.info("Copying raw action scripts to job directory")
    for(action in config$actions){
      config$logger.info(sprintf("Copying %s ...", action$script))
      file.copy(from = file.path(config$wd, action$script), to = jobDirPath)
    }
  }

  return(config)
}
