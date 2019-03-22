#'geoflow_software
#'@export
geoflow_software <- R6Class("geoflow_software",
  public = list(
    id = NULL,
    type = NULL,
    software_type = NULL,
    definition = NULL,
    handler = NULL,
    arguments = list(),
    parameters = list(),
    initialize = function(id = NULL, type = NULL, software_type, definition, handler, arguments){
      self$setId(id)
      if(!is.null(type)) self$setType(type)
      self$setSoftwareType(software_type)
      self$setDefinition(definition)
      self$setHandler(handler)
      self$setArguments(arguments)
    },
    
    #setId
    setId = function(id){
      self$id <- id
    },
    
    #setType
    setType = function(type){
      if(!(type %in% c("input","output"))){
        stop("The type should be either an 'input' or 'output'!")
      }
      self$type <- type
    },
    
    #setSoftwareType
    setSoftwareType = function(software_type){
      self$software_type <- software_type
    },
    
    #setDefinition
    setDefinition = function(definition){
      self$definition <- definition
    },
    
    #setHandler
    setHandler = function(handler){
      self$handler <- handler
    },
    
    #setArguments
    setArguments = function(arguments){
      self$arguments <- arguments
    },
    
    #setParameters
    setParameters = function(...){
      params <- list(...)[[1]]
      paramNames <- names(params)
      if(length(paramNames)>0){
        for(paramName in paramNames){
          if(!(paramName %in% names(self$arguments))){
            stop(sprintf("The parameter '%s' is not a valid parameter for software '%s'. The parameter should be among values [%s]. To see a comprehensive parameters list, use the following the code: list_software_parameter(\"%s\")", 
                         paramName, self$software_type, paste(names(self$arguments), collapse=","), self$software_type))
          }
          self$parameters[[paramName]] <- params[[paramName]]
        }
      }
    },
    
    #getHandlerInstance
    getHandlerInstance = function(){
      handler_params = paste(sapply(names(self$parameters), function(paramName){
        paramValue <- self$parameters[[paramName]]
        if(is.character(paramValue)) paramValue <- paste0("\"",paramValue,"\"")
        return(paste(paramName," = ",paramValue))
      }),collapse=", ")
      instance <- eval(parse(text=paste0("self$handler(",handler_params,")")))
      return(instance)
    }
    
  )
)

#'register_software
#'@export
register_software <- function(){
  
  software <- list(
    #OGC WFS
    geoflow_software$new(
      software_type = "csw",
      definition = "OGC Catalogue Service for the Web (CSW) client powered by 'ows4R' package",
      handler = ows4R::CSWClient$new,
      arguments = list(
        url = list(def = "CSW service endpoint URL"),
        version = list(def = "CSW service version ('2.0.2' or '3.0')"),
        user = list(def = "Username for CSW authentication"),
        pwd = list(def = "Password for CSW authentication"),
        logger = list(def = "Level for 'ows4R' logger messages (NULL,INFO or DEBUG)")
      )
    ),
    #OGC WFS
    geoflow_software$new(
      software_type = "wfs",
      definition = "OGC Web Feature Service (WFS) client powered by 'ows4R' package",
      handler = ows4R::WFSClient$new,
      arguments = list(
        url = list(def = "WFS service endpoint URL"),
        version = list(def = "WFS service version ('1.0.0', '1.1.1', '2.0'"),
        user = list(def = "Username for WFS authentication"),
        pwd = list(def = "Password for WFS authentication"),
        logger = list(def = "Level for 'ows4R' logger messages (NULL, 'INFO' or 'DEBUG')")
      )
    ),
    #GEONETWORK API
    geoflow_software$new(
      software_type = "geonetwork",
      definition = "GeoNetwork API Client, powered by 'geonapi' package",
      handler = geonapi::GNManager$new,
      arguments = list(
        url = list(def = "GeoNetwork catalogue URL"),
        version = list(def = "Geonetwork catalogue version"),
        user = list(def = "Username for GeoNetwork authentication"),
        pwd = list(def = "Password for GeoNetwork authentication"),
        logger = list(def = "Level for 'geonapi' logger messages (NULL, 'INFO' or 'DEBUG')")
      )
    ),
    #GEOSERVER API
    geoflow_software$new(
      software_type = "geoserver",
      definition = "GeoServer REST API Client, powered by 'geosapi' package",
      handler = geosapi::GSManager$new,
      arguments = list(
        url = list(def = "GeoServer application URL"),
        user = list(def = "Username for GeoServer authentication"),
        pwd = list(def = "Password for GeoServer authentication"),
        logger = list(def = "Level for 'geosapi' logger messages (NULL, 'INFO' or 'DEBUG')")    
      )
    ),
    #ZENODO
    geoflow_software$new(
      software_type = "zenodo",
      definition = "Zenodo client powered by 'zen4R' package",
      handler = zen4R::ZenodoManager$new,
      arguments = list(
        url = list(def = "Zenodo API URL. For sandbox tests, use 'https://sandbox.zenodo.org/api', otherwise provided by zen4R by default"),
        token = list(def = "Zenodo user authentication token."),
        logger = list(def = "Level for 'zen4R' logger messages (NULL, 'INFO' or 'DEBUG')")
      )
    )
  )
  .geoflow$software <- software
}

#'list_software
#'@export
list_software <- function(raw = FALSE){
  software <- .geoflow$software
  if(raw){
    return(software)
  }else{
    software <- do.call("rbind", lapply(software, function(obj){
      obj.out <- data.frame(
        software_type = obj$software_type,
        definition = obj$definition,
        stringsAsFactors = FALSE
      )
      return(obj.out)
    }))
  }
  return(software)
}

#'list_software_parameters
#'@export
list_software_parameters <- function(software_type){
  out <- NULL
  software <- .geoflow$software[sapply(.geoflow$software, function(x){x$software_type == software_type})]
  if(length(software)==0) stop(sprintf("No software '%s'!", software_type))
  software <- software[[1]]
  if(length(software$arguments)>0){
    out <- data.frame(
      name = names(software$arguments),
      definition = sapply(software$arguments, function(x){x$def}),
      stringsAsFactors = FALSE
    )
    row.names(out) <- 1:nrow(out)
  }else{
    out <- data.frame(name = character(0), definition = character(0))
  }
  return(out)
}

#'list_software_properties
#'@export
list_software_properties <- function(software_type){
  out <- NULL
  software <- .geoflow$software[sapply(.geoflow$software, function(x){x$software_type == software_type})]
  if(length(software)==0) stop(sprintf("No software '%s'!", software_type))
  software <- software[[1]]
  if(length(software$properties)>0){
    out <- data.frame(
      name = names(software$properties),
      definition = sapply(software$properties, function(x){x$def}),
      stringsAsFactors = FALSE
    )
    row.names(out) <- 1:nrow(out)
  }else{
    out <- data.frame(name = character(0), definition = character(0))
  }
  return(out)
}