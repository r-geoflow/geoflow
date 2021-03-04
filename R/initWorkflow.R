#' @name initWorkflow
#' @aliases initWorkflow
#' @title initWorkflow
#' @description \code{initWorkflow} allows to init a workflow
#'
#' @usage initWorkflow(file)
#'                 
#' @param file a JSON configuration file
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
initWorkflow <- function(file){
  
  config <- jsonlite::read_json(file)
  config$src <- file
  config$src_config <- config
  
  #worfklow config$loggers
  config$logger <- function(type, text){cat(sprintf("[geoflow][%s][%s] %s \n", config$id, type, text))}
  config$logger.info <- function(text){config$logger("INFO", text)}
  config$logger.warn <- function(text){config$logger("WARN", text)}
  config$logger.error <- function(text){config$logger("ERROR", text)}
  
  config$logger.info("Init Workflow configuration")
  config$logger.info("========================================================================")
  
  #profile
  if(!is.null(config$profile)){
    config$logger.info("Creating workflow profile...")
    profile <- geoflow_profile$new()
    if(!is.null(config$profile$project)) profile$setProject(config$profile$project)
    if(!is.null(config$profile$organization)) profile$setOrganization(config$profile$organization)
    if(!is.null(config$profile$logos)){
      for(logo in config$profile$logos) profile$addLogo(logo)
    }
    config$profile <- profile
  }
  
  #options
  if(!is.null(config$options)){
    config$logger.info("Setting geoflow global options...")
    if(!is.null(config$options$line_separator)){
      config$logger.info(sprintf("Setting option 'line_separator' to '%s'", config$options$line_separator))
      set_line_separator(config$options$line_separator)
    }
  }
  
  #working dir
  if(is.null(config$wd)) config$wd <- getwd()
  
  #type of workflow
  cfg_mode <- config$mode
  if(!is.null(cfg_mode)){
    allowedModes <- c("raw","entity")
    if(!(cfg_mode %in% allowedModes)) {
      errMsg <- sprintf("The workflow '%s' mode is incorrect. Allowed values are [%s]",
                        cfg_mode, paste(allowedModes, collapse=","))
      config$logger.error(errMsg)
      stop(errMsg)
    }
    config$mode <- cfg_mode
  }else{
    warnMes <- "No workflow mode specified, 'raw' mode specified by default!"
    config$logger.warn(warnMes)
    config$mode <- "raw"
  }

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
        if(class(client_handler)=="try-error"){
          errMsg <- sprintf("Error while evaluating software handler '%s'", software$handler)
          config$logger.error(errMsg)
          stop(errMsg)
        }
        client_params <- unlist(software[names(software)!="handler"])
        client <- client_handler(client_params)
      }
      if(!is.null(config$software[[software$type]][[switch(software$type,"input"=software$id,"output"=software$software_type)]])){
        if(software$type=="input") errMsg <- sprinttf("An input software with id '%s' has been already declared!", software$id)
        if(software$type=="output") errMsg <- sprintf("An output software with software type '%s' has been already declared!", software$software_type)
        config$logger.error(errMsg)
        stop(errMsg)
      }
      config$software[[software$type]][[software$software_type]] <- client
      config$software[[software$type]][[paste(software$software_type,"config",sep="_")]] <- software
    }
  }
  
  config_registers <- config$registers #store eventual config$registers
  
  #loading dictionnary
  #metadata elements
  if(!is.null(config$metadata)){
    if(is.null(config$metadata$content)) config$metadata$content <- list()
    
    #metadata dictionary
    cfg_md_dictionary <- config$metadata$dictionary
    if(!is.null(cfg_md_dictionary)){
      config$logger.info("Loading data dictionary...")
      md_dict_handler <- loadHandler(config, "dictionary")
      config$logger.info("Execute handler to load dictionary...")
      dictionary <- md_dict_handler(config, source = cfg_md_dictionary$source)
      
      if(!is(dictionary, "geoflow_dictionary")){
        errMsg <- "The output of the dictionary handler should return an object of class 'geoflow_dictionary'"
        config$logger.error(errMsg)
        stop(errMsg)
      }
      
      #keep source dictionary part of the config
      config$src_dictionary <- dictionary$source
      
      config$logger.info("Successfuly fetched dictionary !")
      config$metadata$content$dictionary <- dictionary
      config$registers <- dictionary$getRegisters()
      
    }
  }
  
  #registers
  #registers can be configured either through config or through dictionnary
  if(!is.null(config_registers)){
    fetched_registers <- list()
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
        if(class(customfun)=="try-error"){
          errMsg <- sprintf("Error while trying to evaluate custom function '%s", reg$id)
          config$logger.error(errMsg)
          stop(errMsg)
        }
        if(class(customfun)!="function"){
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
  
  #metadata elements
  if(!is.null(config$metadata)){
    config$logger.info("Loading metadata elements...")
    if(is.null(config$metadata$content)) config$metadata$content <- list()
    
    #metadata contacts
    cfg_md_contacts <- config$metadata$contacts
    if(!is.null(cfg_md_contacts)){
      config$logger.info("Loading metadata contacts...")
      md_contact_handler <- loadHandler(config, "contacts")
      config$logger.info("Execute contact handler to load contacts...")
      contacts <- md_contact_handler(config, source = cfg_md_contacts$source)
      
      if(!is(contacts, "list") | !all(sapply(contacts, is, "geoflow_contact"))){
        errMsg <- "The output of the contacts handler should return a list of objects of class 'geoflow_entity_contact'"
        config$logger.error(errMsg)
        stop(errMsg)
      }
      
      #keep source contacts part of the config
      config$src_contacts <- attr(contacts, "source")
      
      config$logger.info(sprintf("Successfuly fetched %s contacts!",length(contacts)))
      config$metadata$content$contacts <- contacts
      config$logger.info(sprintf("Successfuly loaded %s contacts!",length(contacts)))
    }
    
    #metadata entities
    cfg_md_entities <- config$metadata$entities
    if(!is.null(cfg_md_entities)){
      config$logger.info("Loading metadata entities...")
      md_entity_handler <- loadHandler(config, "entities")
      config$logger.info("Execute handler to load entities...")
      entities <- md_entity_handler(config, source = cfg_md_entities$source)
      
      if(!is(entities, "list") | !all(sapply(entities, is, "geoflow_entity"))){
        errMsg <- "The output of the entities handler should return a list of objects of class 'geoflow_entity'"
        config$logger.error(errMsg)
        stop(errMsg)
      }
      
      #keep source entities part of the config
      config$src_entities <- attr(entities, "source")
      
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
        action_to_trigger <- .geoflow$actions[sapply(.geoflow$actions, function(x){x$id==action$id})][[1]]
        
        #check action dependencies
        action_to_trigger$checkPackages()
        
        action_to_trigger$options <- action$options
      }else{
        if(config$mode == "entity"){
          source(action$script)
          customfun <- eval(parse(text = action$id))
          if(class(customfun)=="try-error"){
            errMsg <- sprintf("Error while trying to evaluate custom function'%s", action$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          if(class(customfun)!="function"){
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
        }else if(config$mode == "raw"){
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

  return(config)
}

#loadHandler
loadHandler <- function(config, elem){
  md_handler <- NULL
  config_md_elem <- config$metadata[[elem]]
  if(is.null(config_md_elem)) return(md_handler)

  h <- config_md_elem$handler
  if(is.null(h)){
    errMsg <- sprintf("Missing 'handler' for metadata '%s' (default handler id, or function name from custom script)", elem)
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #type of handler
  isHandlerId <- is.null(config_md_elem$script)
  if(isHandlerId){
    config$logger.info("Try to use embedded contacts handler")
    #in case handler id is specified
    md_default_handlers <- switch(elem,
      "contacts" = list_contact_handlers(raw=TRUE),
      "entities" = list_entity_handlers(raw=TRUE),
      "dictionary" = list_dictionary_handlers(raw=TRUE)
    )
    md_default_handler_ids <- sapply(md_default_handlers, function(x){x$id})
    if(!(h %in% md_default_handler_ids)){
      errMsg <- sprintf("Unknown handler '%s'. Available handlers are: %s",
                        h, paste(md_default_handler_ids, collapse=","))
    }
    h_src <- config_md_elem$source
    if(is.null(h_src)){
      errMsg <- sprintf("Missing 'source' for handler '%s'", h)
    }
    
    md_handler <- md_default_handlers[sapply(md_default_handlers, function(x){x$id==h})][[1]]$fun
   
  }else{
    #in case handler is a script
    h_script <- config_md_elem$script
    config$logger.info(sprintf("Try to use custom handler '%s' from script '%s'", h, h_script))
    if(!file.exists(h_script)){
      errMsg <- sprintf("File '%s' does not exist in current directory!", h_script)
      config$logger.error(errMsg)
      stop(errMsg)
    }
    source(h_script) #load script
    md_handler <- try(eval(parse(text = h)))
    if(class(md_handler)=="try-error"){
      errMsg <- sprintf("Failed loading function '%s. Please check the script '%s'", h, h_script)
      config$logger.error(errMsg)
      stop(errMsg)
    }
    
    #check custom handler arguments
    args <- names(formals(md_handler))
    if(!all(c("config", "source") %in% args)){
      errMsg <- "The handler function should at least include the parameters (arguments) 'config' and 'source'"
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }
  return(md_handler)
}