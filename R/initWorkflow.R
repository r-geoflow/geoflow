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
  
  #worfklow config$loggers
  config$logger <- function(type, text){cat(sprintf("[geoflow][%s][%s] %s \n", config$id, type, text))}
  config$logger.info <- function(text){config$logger("INFO", text)}
  config$logger.warn <- function(text){config$logger("WARNING", text)}
  config$logger.error <- function(text){config$logger("ERROR", text)}
  
  config$logger.info("Init Workflow configuration")
  config$logger.info("========================================================================")
  
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
  
  #load packages
  #-------------
  #from CRAN
  config$logger.info("Loading R CRAN packages...")
  cran_pkgs = c("devtools", config$dependencies$packages$cran)
  invisible(sapply(cran_pkgs, function(pkg){
    if(config$dependencies$packages$cran_force_install){
      config$logger.info(sprintf("Reinstalling R CRAN package '%s'", pkg))
      eval(parse(text = sprintf("try(detach(\"package:%s\", unload=TRUE, force = TRUE))", pkg)))
      install.packages(pkg)
    }
    config$logger.info(sprintf("Loading R CRAN package '%s'", pkg))
    eval(parse(text = sprintf("require(%s)", pkg)))
  }))
  #from Github
  config$logger.info("Loading R GitHub packages...")
  github_pkgs = config$dependencies$packages$github
  invisible(sapply(github_pkgs, function(pkg){
    pkgname <- unlist(strsplit(pkg, "/"))[2]
    if(config$dependencies$packages$github_force_install){
      config$logger.info(sprintf("Reinstalling R GitHub package '%s'", pkgname))
      eval(parse(text = sprintf("try(detach(\"package:%s\", unload=TRUE, force = TRUE))", pkgname)))
      devtools::install_github(pkg, force = TRUE)
    }
    config$logger.info(sprintf("Loading R GitHub package '%s'", pkgname))
    eval(parse(text = sprintf("require(%s)", pkgname)))
  }))
  #load source scripts
  #--------------------
  config$logger.info("Loading R scripts...")
  source_scripts <- config$dependencies$scripts
  invisible(sapply(source_scripts,function(script){
    config$logger.info(sprintf("Loading R script '%s'...", script))
    source(script)
  }))
  
  #software components
  if(!is.null(config$software)){
    
    supportedSoftware <- c(
      "geoserver", "geonetwork", "wfs", "csw",
      "zenodo"
    )
    
    software_configs <- config$software
    config$software <- list()
    for(software in software_configs){
      if(is.null(software$id)){
        errMsg <- "Sofware 'id' is missing. Please make sure to give an id to all declared software"
        config$logger.info(errMsg)
        stop(errMsg)
      }
      embeddedSoftware <- is.null(software$handler)
      if(!(software$id %in% supportedSoftware) & embeddedSoftware){
        errMsg <- sprintf("Embedded Software with id '%s' not supported by geoflow", software$id)
        config$logger.error(errMsg)
        stop(errMsg)
      }
      client <- NULL
      if(embeddedSoftware){
        client <- switch(tolower(software$id),
          "geoserver" = geosapi::GSManager$new(url = software$url, user = software$user, pwd = software$pwd, logger = software$logger),
          "geonetwork" = geonapi::GNManager$new(url = software$url, user = software$user, pwd = software$pwd, version = software$version, logger = software$logger),
          "wfs" = ows4R::WFSClient$new(url = software$url, user = software$user, pwd = software$pwd, serviceVersion = software$version, logger = software$logger),
          "csw" = ows4R::CSWClient$new(url = software$url, user = software$user, pwd = software$pwd, serviceVersion = software$version, logger = software$logger),
          "zenodo" = {
            if(!is.null(software$url))
              zen4R::ZenodoManager$new(url = software$url, access_token = software$token, logger = software$logger)
            else
              zen4R::ZenodoManager$new(access_token = software$token, logger = software$logger)
          },
          NULL
        )
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
      config$software[[software$id]] <- client
      config$software[[paste(software$id,"config",sep="_")]] <- software
    }
  }
  
  #metadata elements
  if(!is.null(config$metadata)){
    config$logger.info("Loading metadata elements...")
    config$metadata$content <- list()
    
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
              id <- contact$id
              role <- contact$role
              contact_from_directory <- directory_of_contacts[sapply(directory_of_contacts, function(x){x$id == id})]
              if(!is.null(contact_from_directory)){
                if(length(contact_from_directory)>0){
                  if(length(contact_from_directory)>1){
                    config$logger.warn("Warning: 2 contacts identified with same id! Check your contacts")
                  }
                  contact_from_directory <- contact_from_directory[[1]]
                  newcontact <- contact_from_directory
                  newcontact$setId(id)
                  newcontact$setRole(role)
                }
              }
            }
            return(newcontact)
          })
          return(newentity)
        })
      }
      config$metadata$content$entities <- entities
      config$logger.info(sprintf("Successfuly loaded %s entities!",length(entities)))
    }
    
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
        if(!(action$id %in% list_geoflow_actions(raw=TRUE))){
          stop(sprintf("The action '%s' is not among available geoflow actions", action$id))
        }
        action_to_trigger <- .geoflow$actions[sapply(.geoflow$actions, function(x){x$id==action$id})][[1]]
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
          funparams <- formals(customfun)
          if(!("entity" %in% funparams)){
            errMsg <- sprintf("Missing parameter 'entity' in function '%s'", action$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          if(!("config" %in% funparams)){
            errMsg <- sprintf("Missing parameter 'config' in function '%s'", action$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          if(!("opts" %in% funparams)){
            errMsg <- sprintf("Missing parameter 'opts' in function '%s'", action$id)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          action_to_trigger <- geoflow_action$new(
            id = action$id,
            type = action$type,
            def = action$def,
            fun = customfun
          )
        }else if(config$mode == "raw"){
          action_to_trigger <- geoflow_action$new(
            id = action$script,
            type = action$type,
            def = action$def,
            script = action$script
          )
        }
      }
      return(action_to_trigger)
    })
    
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
    errMsg <- "Missing 'handler' for metadata contacts (default handler id, or function name from custom script)"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #type of handler
  isHandlerId <- is.null(config_md_elem$script)
  if(isHandlerId){
    config$logger.info("Try to use embedded contacts handler")
    #in case handler id is specified
    md_default_handlers <- c("gsheet", "csv")
    if(!(h %in% md_default_handlers)){
      errMsg <- sprintf("Unknown handler '%s'. Available handlers are: %s",
                        h, paste(md_default_handlers, collapse=","))
    }
    h_src <- config_md_elem$source
    if(is.null(h_src)){
      errMsg <- sprintf("Missing 'source' for handler '%s'", h)
    }
    if(elem == "contacts"){
      md_handler <- switch(h,
         "gsheet" = handle_contacts_gsheet,
         "csv"    = handle_contacts_csv
      )
    }else if(elem == "entities"){
      md_handler <- switch(h,
         "gsheet" = handle_entities_gsheet,
         "csv"    = handle_entities_csv
      )      
    } 
   
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