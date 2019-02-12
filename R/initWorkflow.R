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
  
  #metadata elements
  config$metadata$content <- list()
  if(!is.null(config$metadata)){
    config$logger.info("Loading metadata elements...")
    
    #metadata contacts
    cfg_md_contacts <- config$metadata$contacts
    if(!is.null(cfg_md_contacts)){
      config$logger.info("Loading metadata contacts...")
      md_contact_handler <- loadHandler(config, "contacts")
      config$logger.info("Execute contact handler to load contacts...")
      contacts <- md_contact_handler(config, source = cfg_md_contacts$source)
      
      if(!is(contacts, "list") | !all(sapply(contacts, is, "geoflow_entity_contact"))){
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
        invisible(lapply(entities, function(entity){
          entity$contacts <- lapply(entity$contacts, function(contact){
            if(is(contact,"geoflow_entity_contact")){
              id <- contact$id
              role <- contact$role
              contact_from_directory <- directory_of_contacts[sapply(directory_of_contacts, function(x){x$id == id})]
              if(!is.null(contact_from_directory)){
                if(length(contact_from_directory)>0){
                  if(length(contact_from_directory)>1){
                    config$logger.warn("Warning: 2 contacts identified with same id! Check your contacts")
                  }
                  contact_from_directory <- contact_from_directory[[1]]
                  contact <- contact_from_directory
                  contact$setRole(role)
                }
              }
            }
            return(contact)
          })
        }))
      }
      config$metadata$content$entities <- entities
      config$logger.info(sprintf("Successfuly loaded %s entities!",length(entities)))
    }
    
  }
  
  #SDI components
  if(!is.null(config$sdi)){
  
	  #connect to database
	  #--------------------
      db <- config$sdi$db
	  if(!is.null(db)){
  		config$logger.info(sprintf("Connect to database '%s'...", db$name))
  		config$db[["con"]] <- try(dbConnect(db$drv, dbname=db$name, user=db$user, password=db$pwd, host=db$host))
  		  
  		#specific to PG for now
  		if(db$drv == "PostgresSQL"){
  			config$db[["dbpath"]] <- paste("PG:", paste(lapply(names(db), function(x){return(paste(x,db[[x]],sep="="))}), collapse=" "), sep="")
  		}
    }
	  #Geoserver API manager
	  #--------------------
	  gs <- config$sdi$geoserver 
	  if(!is.null(gs)){
  		config$logger.info("Connect to GeoServer API...")
  		config$sdi$geoserver_config <- config$sdi$geoserver
  		config$sdi$geoserver <- geosapi::GSManager$new(url = gs$url, user = gs$user, pwd = gs$pwd, logger = config$sdi$loggerLevel)
	  }
	  #Geonetwork API manager
	  #--------------------
	  gn <- config$sdi$geonetwork
	  if(!is.null(gn)){
  		config$logger.info("Connect to GeoNetwork API...")
  		config$sdi$geonetwork_config <- config$sdi$geonetwork
  		config$sdi$geonetwork <- geonapi::GNManager$new(url = gn$url, user = gn$user, pwd = gn$pwd, version = gn$version, logger = config$sdi$loggerLevel)
	  }
	  #WFS Client
	  #--------------------
	  wfs <- config$sdi$wfs
	  if(!is.null(wfs)){
  		config$logger.info("Connect to OGC Web Feature Server (WFS)...")
  		config$sdi$wfs_config <- config$sdi$wfs
  		config$sdi$wfs <- ows4R::WFSClient$new(url = wfs$url, user = wfs$user, pwd = wfs$pwd, serviceVersion = wfs$version, logger = config$sdi$loggerLevel)
	  }
	  #CSW Client
	  #---------------------
	  csw <- config$sdi$csw
	  if(!is.null(csw)){
	    config$logger.info("Connect to OGC Catalogue Service (CSW)...")
  		config$sdi$csw_config <- config$sdi$csw
  		config$sdi$csw <- ows4R::CSWClient$new(url = csw$url, user = csw$user, pwd = csw$pwd, serviceVersion = csw$version, logger = config$sdi$loggerLevel)
	  }
  }
  
  #Actions
  if(!is.null(config$actions)){
    config$tasks <- names(config$actions)[unlist(config$actions)]
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
    md_default_handlers <- c("gsheet")
    if(!(h %in% md_default_handlers)){
      errMsg <- sprintf("Unknown handler '%s'. Available handlers are: %s",
                        h, paste(md_default_handlers, collapse=","))
    }
    h_src <- config_md_elem$source
    if(is.null(h_src)){
      errMsg <- sprintf("Missing 'source' for handler '%s'", h)
    }
    md_contact_handler <- switch(h,
                                 "gsheet" = geoflow_handler_gsheet_contacts
    )
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
    md_contact_handler <- try(eval(parse(text = h)))
    if(class(md_contact_handler)=="try-error"){
      errMsg <- sprintf("Failed loading function '%s. Please check the script '%s'", h, h_script)
      config$logger.error(errMsg)
      stop(errMsg)
    }
    
    #check custom handler arguments
    args <- names(formals(md_contact_handler))
    if(!all(c("config", "source") %in% args)){
      errMsg <- "The handler function should at least include the parameters (arguments) 'config' and 'source'"
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }
  return(md_contact_handler)
}