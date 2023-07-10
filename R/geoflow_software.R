#' geoflow_software
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_software
#' @title Geoflow software class
#' @description This class models a software to be used by geoflow
#' @keywords software
#' @return Object of \code{\link{R6Class}} for modelling a software
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   software<- geoflow_software$new(
#'    id = "some-id",
#'    type = "output",
#'    software_type = "software",
#'    definition = "definition",
#'    packages = list(),
#'    handler = function(){},
#'    arguments = list(
#'      url = list(def = "the software url")
#'    ),
#'    attributes = list(
#'      workspace = list(def = "a workspace name in the software")
#'    )
#'  )
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_software <- R6Class("geoflow_software",
  inherit = geoflowLogger,
  public = list(
    #'@field id software id
    id = NULL,
    #'@field type software I/O type ("input" or "output")
    type = NULL,
    #'@field software_type type of software
    software_type = NULL,
    #'@field definition definition
    definition = NULL,
    #'@field packages list of packages required for the software functioning
    packages = list(),
    #'@field handler software handler function
    handler = NULL,
    #'@field arguments software arguments
    arguments = list(),
    #'@field parameters software parameters
    parameters = list(),
    #'@field attributes software attributes
    attributes = list(),
    #'@field properties software properties
    properties = list(),
    #'@field actions actions associated with the software
    actions = list(),
    
    #'@description Initializes a software
    #'@param id id
    #'@param type type "input" or "output"
    #'@param software_type software type
    #'@param packages list of packages required for the software functioning
    #'@param definition software definition
    #'@param handler software handler \code{function}
    #'@param arguments software handler arguments
    #'@param attributes software attributes
    #'@param actions software actions
    initialize = function(id = NULL, type = NULL, software_type, 
                          packages = list(), definition, handler, 
                          arguments, attributes = list(),
                          actions = list()){
      self$setId(id)
      if(!is.null(type)) self$setType(type)
      self$setSoftwareType(software_type)
      self$setPackages(packages)
      self$setDefinition(definition)
      self$setHandler(handler)
      self$setArguments(arguments)
      self$setAttributes(attributes)
      self$setActions(actions)
    },
    
    #'@description Sets software ID
    #'@param id id
    setId = function(id){
      self$id <- id
    },
    
    #'@description Set type. Either "input" or "output"
    #'@param type software I/O type
    setType = function(type){
      if(!(type %in% c("input","output"))){
        stop("The type should be either an 'input' or 'output'!")
      }
      self$type <- type
    },
    
    #'@description Set software type
    #'@param software_type software type
    setSoftwareType = function(software_type){
      self$software_type <- software_type
    },
    
    #'@description Set software required packages
    #'@param packages list of package names
    setPackages = function(packages){
      self$packages <- packages
    },
    
    #'@description Set software definition
    #'@param definition software definition
    setDefinition = function(definition){
      self$definition <- definition
    },
    
    #'@description Set attributes. Function to call when creating an instance of \code{geoflow_software}
    #'@param attributes named list of attributes
    setAttributes = function(attributes){
      self$attributes <- attributes
    },
    
    #'@description Set properties. Function to call to pass argument values for a given \code{geoflow_software}
    #'@param ... named list of properties
    setProperties = function(...){
      props <- list(...)[[1]]
      propNames <- names(props)
      if(length(propNames)>0){
        for(propName in propNames){
          if(!(propName %in% names(self$properties))){
            stop(sprintf("The property '%s' is not a valid property for software '%s'. The parameter should be among values [%s]. To see a comprehensive properties list, use the following the code: list_software_properties(\"%s\")",
                         paramName, self$software_type, paste(names(self$attributes), collapse=","), self$software_type))
          }
          self$properties[[propName]] <- props[[propName]]
        }
      }
    },
    
    #'@description Set software arguments. Function to call when creating an instance of \code{geoflow_software}
    #'@param arguments list of software arguments
    setArguments = function(arguments){
      self$arguments <- arguments
    },
    
    #'@description Set parameters. Function to call to pass argument values for a given \code{geoflow_software}
    #'@param ... named list of parameters
    setParameters = function(...){
      params <- list(...)[[1]]
      paramNames <- names(params)
      if(length(paramNames)>0){
        for(paramName in paramNames){
          if(!(paramName %in% names(self$arguments))){
            stop(sprintf("The parameter '%s' is not a valid parameter for software '%s'. The parameter should be among values [%s]. To see a comprehensive parameters list, use the following the code: list_software_parameters(\"%s\")", 
                         paramName, self$software_type, paste(names(self$arguments), collapse=","), self$software_type))
          }
          self$parameters[[paramName]] <- params[[paramName]]
        }
      }
    },
    
    #'@description Set software actions
    #'@param actions a list of \code{geoflow_action}
    setActions = function(actions){
      self$actions <- actions
    },
    
    #'@description Set the software handler function
    #'@param handler object of class \code{function}
    setHandler = function(handler){
      self$handler <- handler
    },
    
    #'@description Check that all packages required for the software are available, if yes,
    #'    import them in the R session, and return a \code{data.frame} giving the 
    #'    packages names and version. If one or more packages are unavailable,
    #'    an error is thrown and user informed of the missing packages.
    checkPackages = function(){
      self$INFO(sprintf("Check package dependencies for software '%s' (%s)", self$id, self$software_type))
      out_pkgs <- try(check_packages(self$packages))
      if(is(out_pkgs,"try-error")){
        errMsg <- sprintf("One or more packages are not imported although required for software '%s' (%s)", 
                          self$id, self$software_type)
        self$ERROR(errMsg)
        stop(errMsg)
      }else{
        if(is.null(out_pkgs)){
          self$INFO(sprintf("No additional package required for software '%s' (%s):", 
                            self$id, self$software_type))
        }else{
          self$INFO(sprintf("The following packages have been imported for software '%s' (%s):", 
                            self$id, self$software_type))
          print(out_pkgs)
        }
      }
    },
    
    #'@description Get the software handler instance
    #'@return an object instance of the software handler
    getHandlerInstance = function(){
      
      #get handler
      handler_params = paste(sapply(names(self$parameters), function(paramName){
        paramValue <- self$parameters[[paramName]]
        if(is.character(paramValue)) paramValue <- paste0("\"",paramValue,"\"")
        #manage argument handler (if defined)
        argIdx = which(paramName == names(self$arguments))
        if(length(argIdx)>0){
          if(!is.null(self$arguments[[argIdx]]$handler)){
            paramValue <- paste0("self$arguments[[",argIdx,"]]$handler", "(", paramValue, ")")
          }
        }
        return(paste(paramName," = ",paramValue))
      }),collapse=", ")
      instance <- eval(parse(text=paste0("self$handler(",handler_params,")")))
      return(instance)
    }
    
  )
)

#' @name register_software
#' @aliases register_software
#' @title register_software
#' @description \code{register_software} registers default geoflow software
#'
#' @usage register_software()
#' 
#' @note Function called on load by geoflow
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
register_software <- function(){
  
  software <- list(
    
    #-------------------------------------------------------------------------------------------------------
    #DBI
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "dbi",
      definition = "Data Base Interface powered by 'DBI' package",
      packages = list("DBI", "RSQLite", "RPostgres"),
      handler = try(DBI::dbConnect, silent = TRUE),
      arguments = list(
        drv = list(label = "DBI driver", def = "DBI driver name", class = "character", handler = try(DBI::dbDriver, silent = TRUE)),
        user = list(label = "Username", def = "Username", class = "character"),
        password = list(label = "Password", def = "Password", class = "character"),
        host = list(label = "Hostname", def = "Hostname", class = "character"),
        port = list(label = "Port number", def = "Port number", class = "integer"),
        dbname = list(label = "Database name", def = "Database name", class = "character")
      ),
      attributes = list(
        onstart_sql = list(label = "SQL 'on-start' script", def = "An SQL script to be run on workflow start", class = "character"),
        onstart_r = list(label = "R 'on-start' SQL generating script", def = "R instructions to generate a SQL. It should be made of 2 properties 'script' (name
                         of the R script) that should include a function standardized with parameter config (being the
                         DBI software config) and will output a character representing the SQL. The name of the function 
                         is to specify in 'fun' property."),
        onend_sql = list(label = "SQL 'on-end' script", def = "An SQL script to be run on workflow end", class = "character"),
        onend_r = list(label = "R 'on-end' SQL generating script", def = "R instructions to generate a SQL. It should be made of 2 properties 'script' (name of the R script) 
                       that should include a function standardized with parameter config (being the DBI software config) and will output 
                       a character representing the SQL. The name of the function is to specify in 'fun' property.")
      ),
      actions = list(
        onstart = function(config, software, software_config){
          if(!is.null(software_config$properties$onstart_sql) || !is.null(software_config$properties$onstart_r)){
            config$logger.info(sprintf("DBI [id='%s'] Execute 'onstart' action",software_config$id))
            
            sql <- NULL
            if(!is.null(software_config$properties$onstart_sql)){
              config$logger.info(sprintf("SQL script = %s", software_config$properties$onstart_sql))
              sql <- paste0(readLines(get_config_resource_path(config, software_config$properties$onstart_sql)),collapse="\n")
              
            }else if(!is.null(software_config$properties$onstart_r)){
              if(is.null(software_config$properties$onstart_r$script)){
                errMsg <- sprintf("DBI [id='%s'] Error to init 'onstart' from R - Missing 'script'",software_config$id)
                config$logger.error(errMsg)
                stop(errMsg)
              }
              if(is.null(software_config$properties$onstart_r$fun)){
                errMsg <- sprintf("DBI [id='%s'] Error to init 'onstart' from R - Missing 'fun'",software_config$id)
                config$logger.error(errMsg)
                stop(errMsg)
              }
              src <- try(source(get_config_resource_path(config, software_config$properties$onstart_r$script)))
              if(is(src,"try-error")){
                errMsg <- sprintf("DBI [id='%s'] Error to init 'onstart' from R - Error while sourcing script '%s'",
                                  software_config$id, software_config$properties$onstart_r$script)
                config$logger.error(errMsg)
                stop(errMsg)
              }
              onstart_r_fun <- eval(parse(text=software_config$properties$onstart_r$fun))
              sql <- try(onstart_r_fun(config, software, software_config))
              if(is(sql,"try-error")){
                errMsg <- sprintf("DBI [id='%s'] Error to init 'onstart' from R - Error while executing function '%s'",
                                  software_config$id, software_config$properties$onstart_r$fun)
                config$logger.error(errMsg)
                stop(errMsg)
              }
            }
            config$logger.info(sprintf("DBI [id='%s'] Executing SQL",software_config$id))
            config$logger.info(paste0("\n", sql))
            
            #write sql to file
            if (!dir.exists("sql")){
              config$logger.info(sprintf("Creating 'sql' directory: %s", file.path(getwd(), "sql")))
              dir.create(file.path(getwd(), "sql"))
            }
            sqlfilename <- paste0(software_config$id, "_onstart.sql")
            config$logger.info(sprintf("DBI [id='%s'] Writing SQL file '%s' to job directory",software_config$id, sqlfilename))
            writeChar(sql, file.path(getwd(), "sql", sqlfilename), eos = NULL)
            
            #send sql to dB
            out <- try(DBI::dbSendQuery(software, sql))
            if(is(out,"try-error")){
              errMsg <- sprintf("DBI [id='%s'] Error while executing SQL",software_config$id)
              config$logger.error(errMsg)
              stop(errMsg)
            }
            config$logger.info(sprintf("DBI [id='%s'] Successful SQL execution!",software_config$id))
          }else{
            config$logger.info(sprintf("DBI [id='%s'] No 'sqlonstart' property. Skipping 'onstart' action",software_config$id))
          }
        },
        onend = function(config, software, software_config){
          if(!is.null(software_config$properties$onend_sql) || !is.null(software_config$properties$onend_r)){
            config$logger.info(sprintf("DBI [id='%s'] Execute 'onend' action",software_config$id))
            
            sql <- NULL
            if(!is.null(software_config$properties$onend_sql)){
              config$logger.info(sprintf("SQL script = %s", software_config$properties$onend_sql))
              sql <- paste0(readLines(get_config_resource_path(config, software_config$properties$onend_sql)),collapse="\n")
              
            }else if(!is.null(software_config$properties$onend_r)){
              if(is.null(software_config$properties$onend_r$script)){
                errMsg <- sprintf("DBI [id='%s'] Error to init 'onend' from R - Missing 'script'",software_config$id)
                config$logger.error(errMsg)
                stop(errMsg)
              }
              if(is.null(software_config$properties$onend_r$fun)){
                errMsg <- sprintf("DBI [id='%s'] Error to init 'onend' from R - Missing 'fun'",software_config$id)
                config$logger.error(errMsg)
                stop(errMsg)
              }
              src <- try(source(get_config_resource_path(config, software_config$properties$onend_r$script)))
              if(is(src,"try-error")){
                errMsg <- sprintf("DBI [id='%s'] Error to init 'onend' from R - Error while sourcing script '%s'",
                                  software_config$id, software_config$properties$onend_r$script)
                config$logger.error(errMsg)
                stop(errMsg)
              }
              onend_r_fun <- eval(parse(text=software_config$properties$onend_r$fun))
              print(class(onend_r_fun))
              sql <- try(onend_r_fun(config, software, software_config))
              if(is(sql,"try-error")){
                errMsg <- sprintf("DBI [id='%s'] Error to init 'onend' from R - Error while executing function '%s'",
                                  software_config$id, software_config$properties$onend_r$fun)
                config$logger.error(errMsg)
                stop(errMsg)
              }
            }
            config$logger.info(sprintf("DBI [id='%s'] Executing SQL",software_config$id))
            config$logger.info(paste0("\n", sql))
            
            #write sql to file
            if (!dir.exists("sql")){
              config$logger.info(sprintf("Creating 'sql' directory: %s", file.path(getwd(), "sql")))
              dir.create(file.path(getwd(), "sql"))
            }
            sqlfilename <- paste0(software_config$id, "_onend.sql")
            config$logger.info(sprintf("DBI [id='%s'] Writing SQL file '%s' to job directory",software_config$id, sqlfilename))
            writeChar(sql, file.path(getwd(), "sql", sqlfilename), eos = NULL)
            
            #send sql to dB
            out <- try(DBI::dbSendQuery(software, sql))
            if(is(out,"try-error")){
              errMsg <- sprintf("DBI [id='%s'] Error while executing SQL",software_config$id)
              config$logger.error(errMsg)
              stop(errMsg)
            }
            config$logger.info(sprintf("DBI [id='%s'] Successful SQL execution!",software_config$id))
          }else{
            config$logger.info(sprintf("DBI [id='%s'] No 'onend_sql' property. Skipping 'onend' action",software_config$id))
          }
        }
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #GOOGLE DRIVE
    #-------------------------------------------------------------------------------------------------------    
    geoflow_software$new(
      software_type = "googledrive",
      definition = "Google Drive access powered by 'googledrive' package",
      packages = list("googledrive"),
      handler = try(googledrive::drive_auth, silent = TRUE),
      arguments = list(
        email = list(label = "Email", def = "User email to authenticate in Google Drive", class = "character"),
        path = list(label = "Path", def = "An optional path within the Google drive repository. Default will be the root", class = "character"),
        token = list(label = "User token", def = "The user authentication token. To get your token in R: gargle::token_fetch()$credentials$access_token", class = "character")
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #INSPIRE METADATA VALIDATOR
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "inspire",
      definition = "INSPIRE Metadata validator, powered by 'geometa' package",
      packages = list("geometa"),
      handler = try(geometa::INSPIREMetadataValidator$new, silent = TRUE),
      arguments = list(
        url = list(label = "INSPIRE Metadata validator URL", def = "URL of the INSPIRE metadata validator instance. By default use 'https://inspire.ec.europa.eu/validator/v2'", class = "character", default = "https://inspire.ec.europa.eu/validator/v2"),
        apiKey = list(label = "API Key", def = "API user key to authenticate to INSPIRE API gateway", class = "character")
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #OGC CSW
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "csw",
      definition = "OGC Catalogue Service for the Web (CSW) client powered by 'ows4R' package",
      packages = list("ows4R"),
      handler = try(ows4R::CSWClient$new, silent = TRUE),
      arguments = list(
        url = list(label = "URL", def = "CSW service endpoint URL", class = "character"),
        serviceVersion = list(label = "Service version", def = "CSW service version ('2.0.2' or '3.0')", class = "character"),
        user = list(label = "Username", def = "Username for CSW authentication (optional)", class = "character"),
        pwd = list(label = "Password", def = "Password for CSW authentication (optional)", class = "character"),
        logger = list(label = "Logger", def = "Level for 'ows4R' logger messages (NULL,INFO or DEBUG)", class = "character", choices = c("INFO", "DEBUG"))
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #OGC WFS
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "wfs",
      definition = "OGC Web Feature Service (WFS) client powered by 'ows4R' package",
      packages = list("ows4R"),
      handler = try(ows4R::WFSClient$new, silent = TRUE),
      arguments = list(
        url = list(label = "URL", def = "WFS service endpoint URL", class = "character"),
        serviceVersion = list(label = "Service version", def = "WFS service version ('1.0.0', '1.1.1', '2.0')", class = "character"),
        user = list(label = "Username", def = "Username for WFS authentication (optional)", class = "character"),
        pwd = list(label = "Password", def = "Password for WFS authentication (optional)", class = "character"),
        logger = list(label = "Logger", def = "Level for 'ows4R' logger messages (NULL, 'INFO' or 'DEBUG')", class = "character", choices = c("INFO", "DEBUG"))
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #OGC WPS
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "wps",
      definition = "OGC Web Processing Service (WPS) client powered by 'ows4R' package",
      packages = list("ows4R"),
      handler = try(ows4R::WPSClient$new, silent = TRUE),
      arguments = list(
        url = list(label = "URL", def = "WPS service endpoint URL", class = "character"),
        serviceVersion = list(label = "Service version", def = "WPS service version (limited to '1.0.0')", class = "character"),
        user = list(label = "Username", def = "Username for WPS authentication (optional)", class = "character"),
        pwd = list(label = "Password", def = "Password for WPS authentication (optional)", class = "character"),
        logger = list(label = "Logger", def = "Level for 'ows4R' logger messages (NULL, 'INFO' or 'DEBUG')", class = "character", choices = c("INFO", "DEBUG"))
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #GEONETWORK API
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "geonetwork",
      definition = "GeoNetwork API Client, powered by 'geonapi' package",
      packages = list("geonapi"),
      handler = try(geonapi::GNManager$new, silent = TRUE),
      arguments = list(
        url = list(label = "URL", def = "GeoNetwork catalogue URL"),
        version = list(label = "Geonetwork version", def = "Geonetwork catalogue version", class = "character"),
        user = list(label = "Username", def = "Username for GeoNetwork authentication", class = "character"),
        pwd = list(label = "Password", def = "Password for GeoNetwork authentication", class = "character"),
        logger = list(label = "Logger", def = "Level for 'geonapi' logger messages (NULL, 'INFO' or 'DEBUG')", class = "character", choices = c("INFO", "DEBUG"))
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #GEOSERVER API
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "geoserver",
      definition = "GeoServer REST API Client, powered by 'geosapi' package",
      packages = list("geosapi"),
      handler = try(geosapi::GSManager$new, silent = TRUE),
      arguments = list(
        url = list(label = "URL", def = "GeoServer application URL", class = "character"),
        user = list(label = "Username", def = "Username for GeoServer authentication", class = "character"),
        pwd = list(label = "Password", def = "Password for GeoServer authentication", class = "character"),
        logger = list(label = "Logger", def = "Level for 'geosapi' logger messages (NULL, 'INFO' or 'DEBUG')", class = "character", choices = c("INFO", "DEBUG"))    
      ),
      attributes = list(
        workspace = list(label = "Workspace", def = "GeoServer workspace name", class = "character"),
        store = list(label = "Store", def = "GeoServer data/coverage store name", class = "character"),
        publicUrl = list(label = "Public URL", def = "Geoserver public URL", class = "character")
      ),
      actions = list(
        onstart = function(config, software, software_config){
          config$logger.info("Executing GeoServer 'onstart' action")
          if(!is.null(config$properties$workspace)){
            ws <- software$getWorkspace(config$properties$workspace)
            if(is.null(ws)){
              software$createWorkspace(config$properties$workspace, paste0("http://",config$properties$workspace))
            }
          }
          #TODO to be completed with store creation cases
        },
        onend = function(config, software, software_config){
          config$logger.info("Executing GeoServer 'onend' action")
          software$reload()
        }
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #ZENODO
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "zenodo",
      definition = "Zenodo client powered by 'zen4R' package",
      packages = list("zen4R"),
      handler = try(zen4R::ZenodoManager$new, silent = TRUE),
      arguments = list(
        url = list(label = "URL", def = "Zenodo API URL. For sandbox tests, use 'https://sandbox.zenodo.org/api'", class = "character", choices = c("https://zenodo.org/api","https://sandbox.zenodo.org/api")),
        token = list(label = "User token", def = "Zenodo user authentication token.", class = "character"),
        logger = list(label = "Logger", def = "Level for 'zen4R' logger messages (NULL, 'INFO' or 'DEBUG')", class = "character", choices = c("INFO","DEBUG"))
      ),
      attributes = list(
        clean = list(label = "Clean", def = "An option, to clean draft Zenodo deposits prior to any new deposit. To clean deposits, enable 'run', 
                     and optionally specify either a 'query' (ElasticSearch Zenodo query), a list of 'doi', or 'community' for which
                     you want to restrain the cleaning operation.")
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #DATAVERSE SWORD CLIENT
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "sword_for_dataverse",
      definition = "Dataverse SWORD API Client powered by 'atom4R' package",
      packages = list("atom4R"),
      handler = try(atom4R::SwordDataverseClient$new, silent = TRUE),
      arguments = list(
        hostname = list(label = "URL", def = "Dataverse base URL", class = "character"),
        token = list(label = "User token", def = "Dataverse user authentication token", class = "character"),
        logger = list(label = "Logger", def = "Level for 'atom4R' logger messages (NULL, 'INFO' or 'DEBUG')", class = "character", choices = list("INFO","DEBUG"))
      ),
      attributes = list(
        dataverse = list(label = "Dataverse ID", def = "Dataverse id where to deposit/publish records", class = "character")
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #DATAVERSE NATIVE API CLIENT
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "dataverse",
      definition = "Dataverse Native API Client powered by 'dataverse' package",
      packages = list("dataverse"),
      handler = list,
      arguments = list(
        server = list(label = "URL", def = "Dataverse server URL", class = "character")
      ),
      attributes = list(
        dataverse = list(label = "Dataverse ID", def = "Dataverse id where to deposit/publish records", class = "character")
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #DATAONE CLIENT
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "dataone",
      definition = "DataONe API Client powered by 'dataone' package",
      packages = list("dataone"),
      handler = try(dataone::D1Client, silent = TRUE),
      arguments = list(
        x = list(label = "Contributing Node URL", def = "Contributing Node URL", handler = try(dataone::CNode, silent = TRUE), class = "character"),
        y = list(label = "Member Node URL", def = "Member Node URL", handler = try(dataone::MNode, silent = TRUE), class = "character"),
        token = list(label = "User token", def = "User Authorization token")
      ),
      attributes = list(),
      actions = list(
        onstart = function(config, software, software_config){
          config$logger.info("Executing DataOne 'onstart' action")
          options(dataone_test_token = software_config$parameters$token)
          options(dataone_token = software_config$parameters$token)
        },
        onend = function(config, software, software_config){
          config$logger.info("Executing DataOne 'onend' action")
          options(dataone_test_token = NULL)
          options(dataone_token = NULL)
        }
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #D4SCIENCE STORAGE HUB CLIENT
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "d4storagehub",
      definition = "D4science storage hub API Client powered by 'd4storagehub4R' package",
      packages = list("d4storagehub4R"),
      handler = try(d4storagehub4R::StoragehubManager$new, silent = TRUE),
      arguments = list(
        token = list(label = "User token", def = "D4Science storage hub user authentication token", class = "character"),
        logger = list(label = "Logger", def = "Level for 'd4storagehub4R' logger messages (NULL, 'INFO' or 'DEBUG')", class = "character", choices = list("INFO","DEBUG"))
      ),
      attributes = list(
        workspace = list(label = "Workspace", def = "D4Science storage hub workspace name", class = "character")
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #GBIF CLIENT
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "gbif",
      definition = "Gbif API Client powered by 'rgbif' package",
      packages = list("rgbif"),
      handler = list,
      arguments = list(
        user = list(label = "Username", def = "Username for Gbif authentication", class = "character"),
        pwd = list(label = "Password", def = "Password for Gbif authentication", class = "character"),
        email = list(label = "Email", def = "Email address for sending notification ", class = "character")
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #THREDDS CLIENT
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "thredds",
      definition = "Thredds data server API Client powered by 'thredds' package",
      packages = list("thredds"),
      handler = try(thredds::CatalogNode$new, silent = TRUE),
      arguments = list(
        x = list(label = "Catalog URL", def = "url of top level catalog request", class = "character"),
        prefix = list(label = "Namespace", def = "the namespace to examine", class = "character")
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #OPENAPI CLIENT
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "openapi",
      definition = "OpenAPI client powered by 'rapiclient' package",
      packages = list("rapiclient"),
      handler = try({
        openapi_handler <- function(url, api_key_name = "", api_key_value = ""){
          api <- rapiclient::get_api(url)
          api_headers <- list()
          api_headers[[api_key_name]] <- api_key_value
          api_ops <- rapiclient::get_operations(api, .headers = unlist(api_headers))
          return(api_ops)
        }
        openapi_handler
      }, silent = TRUE),
      arguments = list(
        url = list(label = "OpenAPI URL", def = "url of the OpenAPI JSON definition (now limited to OpenAPI v2)", class = "character"),
        api_key_name = list(label = "Open API key name", def = "Name of the API key for registered uses", class = "character", default = ""),
        api_key_value = list(label = "Open API key value", def = "Value of the API key for registered uses (typically a token)", class = "character", default = "")
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #OCS CLIENT
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "ocs",
      definition = "Open Collaboration Services (OCS) client powered by 'ocs4R' package",
      packages = list("ocs4R"),
      handler = try(ocs4R::ocsManager$new, silent = TRUE),
      arguments = list(
        url = list(label = "URL", def = "OCS service endpoint URL", class = "character"),
        user = list(label = "Username", def = "Username for user authentication", class = "character"),
        pwd = list(label = "Password", def = "Password for user authentication", class = "character"),
        logger = list(label = "Logger", def = "Level for 'ows4R' logger messages (NULL,INFO or DEBUG)", class = "character", choices = c("INFO", "DEBUG"))
      ),
      attributes = list(
        cloud_path = list(label = "Cloud path", def = "Cloud path", class = "character")
      )
    ),
    #-------------------------------------------------------------------------------------------------------
    #GEONODE CLIENT
    #-------------------------------------------------------------------------------------------------------
    geoflow_software$new(
      software_type = "geonode",
      definition = "GeoNode client powered by 'geonode4R' package",
      packages = list("geonode4R"),
      handler = try(geonode4R::GeoNodeManager$new, silent = TRUE),
      arguments = list(
        url = list(label = "URL", def = "GeoNode endpoint URL", class = "character"),
        user = list(label = "Username", def = "Username for user authentication", class = "character"),
        pwd = list(label = "Password", def = "Password for user authentication", class = "character"),
        logger = list(label = "Logger", def = "Level for 'geonode4R' logger messages (NULL,INFO or DEBUG)", class = "character", choices = c("INFO", "DEBUG"))
      )
    )
  )
  .geoflow$software <- software
}

#' @name list_software
#' @aliases list_software
#' @title list_software
#' @description \code{list_software} lists the software supported by geoflow.
#'
#' @usage list_software(raw)
#' 
#' @param raw Default value is \code{FALSE}, meaning the software will be listed as
#' \code{data.frame}. The output If \code{TRUE} the raw list of \link{geoflow_software} 
#' is returned.
#' 
#' @return an object of class \code{data.frame} (or \code{list} of \link{geoflow_software} if raw = FALSE)
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
list_software <- function(raw = FALSE){
  software <- .geoflow$software
  if(raw){
    return(software)
  }else{
    software <- do.call("rbind", lapply(software, function(obj){
      obj.out <- data.frame(
        software_type = obj$software_type,
        definition = obj$definition,
        packages = paste(obj$packages, collapse=","),
        stringsAsFactors = FALSE
      )
      return(obj.out)
    }))
  }
  return(software)
}

#' @name list_software_parameters
#' @aliases list_software_parameters
#' @title list_software_parameters
#' @description \code{list_software_parameters} lists the parameters of a given software supported by geoflow.
#'
#' @usage list_software_parameters(software_type, raw)
#' 
#' @param software_type A software type
#' @param raw if raw list should be returned
#' 
#' @return an object of class \code{data.frame} (or \code{list} if raw is TRUE) listing the software parameters
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
list_software_parameters <- function(software_type, raw = FALSE){
  out <- NULL
  software <- .geoflow$software[sapply(.geoflow$software, function(x){x$software_type == software_type})]
  if(length(software)==0) stop(sprintf("No software '%s'!", software_type))
  software <- software[[1]]
  if(raw) return(software$arguments)
  if(length(software$arguments)>0){
    out <- data.frame(
      name = names(software$arguments),
      label = sapply(software$arguments, function(x){x$label}),
      definition = sapply(software$arguments, function(x){x$def}),
      stringsAsFactors = FALSE
    )
    row.names(out) <- 1:nrow(out)
  }else{
    out <- data.frame(name = character(0), label = character(0), definition = character(0))
  }
  return(out)
}

#' @name list_software_properties
#' @aliases list_software_properties
#' @title list_software_properties
#' @description \code{list_software_properties} lists the properties of a given software supported by geoflow.
#'
#' @usage list_software_properties(software_type, raw)
#' 
#' @param software_type A software type
#' @param raw if raw list should be returned
#' 
#' @return an object of class \code{data.frame} listing the software properties
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
list_software_properties <- function(software_type, raw = FALSE){
  out <- NULL
  software <- .geoflow$software[sapply(.geoflow$software, function(x){x$software_type == software_type})]
  if(length(software)==0) stop(sprintf("No software '%s'!", software_type))
  software <- software[[1]]
  if(raw) return(software$attributes)
  if(length(software$attributes)>0){
    out <- data.frame(
      name = names(software$attributes),
      label = sapply(software$attributes, function(x){x$label}),
      definition = sapply(software$attributes, function(x){x$def}),
      stringsAsFactors = FALSE
    )
    row.names(out) <- 1:nrow(out)
  }else{
    out <- data.frame(name = character(0), label = character(0), definition = character(0))
  }
  return(out)
}
