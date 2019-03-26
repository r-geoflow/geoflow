#'geoflow_data
#'@export
geoflow_data <- R6Class("geoflow_data",
  public = list(
    identifier = NULL,
    type = NULL,
    source = NULL,
    upload = FALSE,
    cqlfilter = NULL,
    geometryField = NULL,
    geometryType = NULL,
    parameters = list(),
    styles = list(),
    workspace = NULL,
    datastore = NULL,
    initialize = function(str = NULL){
      if(!is.null(str)){
        data_props <-  unlist(strsplit(sanitize_str(str), ";"))
        data_props <- lapply(data_props, function(data_prop){
          return(extract_kvp(data_prop))
        })
        names(data_props) <- sapply(data_props, function(x){x$key})
        if(!any(sapply(data_props, function(x){x$key=="type"}))){
          stop("The data source 'type' is mandatory")
        }
        self$setType(data_props$type$values[[1]])
        if(!any(sapply(data_props, function(x){x$key=="source"}))){
          stop("The data 'source' is mandatory")
        }
        self$setSource(data_props$source$values[[1]])
        
        if(!is.null(data_props$identifier)){
          self$setIdentifier(data_props$identifier$values[[1]])
        }
        
        if(!is.null(data_props$upload)){
          upload <- as.logical(data_props$upload$values[[1]])
          if(is.na(upload)){
            stop("Incorrect value for 'upload'. Expected boolean value (true/false")
          }
          self$setUpload(upload)
        }
        if(!is.null(data_props$cqlfilter)){
          self$setCqlFilter(data_props$cqlfilter$values[[1]])
        }
        params <- data_props[sapply(data_props, function(x){x$key=="parameter"})]
        if(length(params)>0){
          if(self$type != "dbquery"){
            stop("The specification of service parameters is only possible for a 'dbquery' type!")
          }
          #check and set parameter
          for(param in params){
            if(length(param$values)!=3){
              stop("Parameter definition should be compound by 3 elements: fieldname (with alias), regexp and default value")
            }
            fieldname <- param$values[[1]]
            param_alias <- attr(fieldname, "description")
            attr(fieldname, "description") <- NULL
            if(is.null(param_alias)) param_alias <- fieldname
            regexp <- param$values[[2]]
            defaultvalue <- param$values[[3]]
            self$setParameter(param_alias, fieldname, regexp, defaultvalue)
          }
          #check compliance of dbquery
          sqlquery <- self$source
          #with fieldnames
          if(!all(sapply(self$parameters, function(x){regexpr(x$fieldname,sqlquery)>0}))){
            stop("At least one parameter fieldname declared is not used in the data source query!")
          }
          #with param aliases
          if(!all(sapply(self$parameters, function(x){regexpr(paste0("%",x$name,"%"),sqlquery)>0}))){
            stop("At least one parameter name declared is not used in the data source query!")
          }
        }
        
        #geometry field & type (required for virtual spatial tables)
        geoms <- data_props[sapply(data_props, function(x){x$key=="geometry"})]
        if(length(geoms)>0){
          geom <- geoms[[1]]
          self$setGeometryField(geom$values[[1]])
          self$setGeometryType(geom$values[[2]])
        }
        
        #styles
        styles <- data_props[sapply(data_props, function(x){x$key=="style"})]
        if(length(styles)>0){
          for(style in styles){
            self$addStyle(style$values[[1]])
          }
        }
        
        #workspace
        workspaces <- data_props[sapply(data_props, function(x){x$key=="workspace"})]
        if(length(workspaces)>0) self$setWorkspace(workspaces[[1]]$values[[1]])
        #datastore
        datastores <- data_props[sapply(data_props, function(x){x$key=="datastore"})]
        if(length(datastores)>0) self$setDatastore(datastores[[1]]$values[[1]])
      }
    },
    
    #setIdentifier
    setIdentifier = function(identifier){
      self$identifier <- identifier
    },
    
    #setType
    setType = function(type){
      allowedTypes <- c("dbtable", "dbview", "dbquery","shp")
      if(!(type %in% allowedTypes)){
        errMsg <- sprintf("Type should be among values [%s]", paste0(allowedTypes, collapse=","))
        stop(errMsg)
      }
      self$type <- type
    },
    
    #setSource
    setSource = function(source){
      self$source <- source
    },
    
    #setUpload
    setUpload = function(upload){
      self$upload <- upload
    },
    
    #setCqlFilter
    setCqlFilter = function(cqlfilter){
      self$cqlfilter <- cqlfilter
    },
    
    #setParameter
    setParameter = function(name, fieldname, regexp, defaultvalue){
      self$parameters[[name]] <- list(
        name = name,
        fieldname = fieldname,
        regexp = regexp,
        defaultvalue = defaultvalue
      )
    },
    
    #setGeometryField
    setGeometryField = function(geometryField){
      self$geometryField <- geometryField
    },
    
    #setGeometryType
    setGeometryType = function(geometryType){
      self$geometryType <- geometryType
    },
    
    #addStyle
    addStyle = function(style){
      self$styles <- c(self$styles, style)
    },
    
    #setWorkspace
    setWorkspace = function(workspace){
      self$workspace <- workspace
    },
    
    #setDatastore
    setDatastore = function(datastore){
      self$datastore <- datastore
    }
    
  )
)