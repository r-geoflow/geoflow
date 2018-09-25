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
  config$logger <- function(type, text){cat(sprintf("[%s][%s] %s \n", config$id, type, text))}
  config$logger.info <- function(text){config$logger("INFO", text)}
  config$logger.warn <- function(text){config$logger("WARNING", text)}
  config$logger.error <- function(text){config$logger("ERROR", text)}
  
  config$logger.info("Init Workflow configuration")
  config$logger.info("========================================================================")
  
  #working dir
  if(!is.null(config$wd)) config$wd <- getwd()
  
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