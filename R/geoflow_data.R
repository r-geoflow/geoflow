#'geoflow_data
#'@export
geoflow_data <- R6Class("geoflow_data",
  private = list(
    supportedUploadTypes = c("dbtable", "dbview", "dbquery","shp", "other")
  ),
  public = list(
    source = NULL,
    sql = NULL,
    upload = TRUE,
    uploadType = "other",
    uploadZip = FALSE,
    uploadZipOnly = FALSE,
    layername = NULL,
    cqlfilter = NULL,
    geometryField = NULL,
    geometryType = NULL,
    parameters = list(),
    styles = list(),
    workspace = NULL,
    datastore = NULL,
    features = NULL,
    attributes = NULL,
    variables = NULL,
    initialize = function(str = NULL){
      if(!is.null(str)){
        data_props <-  extract_cell_components(sanitize_str(str))
        data_props <- lapply(data_props, function(data_prop){
          return(extract_kvp(data_prop))
        })
        
        #uploadType
        names(data_props) <- sapply(data_props, function(x){x$key})
        if(!any(sapply(data_props, function(x){x$key=="uploadType"}))){
          self$setUploadType("other")
        }else{
          self$setUploadType(data_props$uploadType$values[[1]])
        }
        
        
        #upload
        if(!is.null(data_props$upload)){
          upload <- as.logical(tolower(data_props$upload$values[[1]]))
          if(!is.na(upload)){
            self$setUpload(upload) 
          }
        }else{
          self$setUpload(TRUE) 
        }
        
        #uploadZip
        if(!is.null(data_props$uploadZip)){
          uploadZip <- as.logical(tolower(data_props$uploadZip$values[[1]]))
          if(!is.na(uploadZip)){
            self$setUploadZip(uploadZip) 
          }
        }else{
          self$setUploadZip(FALSE) 
        }
        
        #uploadZipOnly
        if(!is.null(data_props$uploadZipOnly)){
          uploadZipOnly <- as.logical(tolower(data_props$uploadZipOnly$values[[1]]))
          if(!is.na(uploadZipOnly)){
            self$setUploadZipOnly(uploadZipOnly) 
          }
        }else{
          self$setUploadZipOnly(FALSE) 
        }
        
        #source
        if(!any(sapply(data_props, function(x){x$key=="source"}))){
          stop("The data 'source' is mandatory")
        }
        self$setSource(data_props$source$values)
        
        #sql
        if(!is.null(data_props$sql)){
          self$setSql(data_props$sql$values[[1]])
        }
        
        #layername (if any)
        #not mandatory, can be used for subset layers
        if(!is.null(data_props$layername)){
          self$setLayername(data_props$layername$values[[1]])
        }
        #cql filter
        if(!is.null(data_props$cqlfilter)){
          self$setCqlFilter(data_props$cqlfilter$values[[1]])
        }
        #params
        params <- data_props[sapply(data_props, function(x){x$key=="parameter"})]
        if(length(params)>0){
          if(self$type != "dbquery"){
            stop("The specification of service parameters is only possible for a 'dbquery' upload type!")
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
        
        #layer styles
        styles <- data_props[sapply(data_props, function(x){x$key=="style"})]
        if(length(styles)>0){
          for(style in styles){
            self$addStyle(style$values[[1]])
          }
        }
        
        #geoserver targets
        #workspace
        workspaces <- data_props[sapply(data_props, function(x){x$key=="workspace"})]
        if(length(workspaces)>0) self$setWorkspace(workspaces[[1]]$values[[1]])
        #datastore
        datastores <- data_props[sapply(data_props, function(x){x$key=="datastore"})]
        if(length(datastores)>0) self$setDatastore(datastores[[1]]$values[[1]])
        
        #attributes
        attributes <- data_props[sapply(data_props, function(x){x$key=="attribute"})]
        get_attributes = list()
        invisible(lapply(attributes, function(attribute){
          for(value in attribute$values){
            if(!(value %in% get_attributes)) get_attributes[[length(get_attributes)+1]] <<- value
          }
        }))
        if(length(get_attributes)>0) self$attributes <- get_attributes
        
        #variables
        variables <- data_props[sapply(data_props, function(x){x$key=="variable"})]
        get_variables = list()
        invisible(lapply(variables, function(variable){
          for(value in variable$values){
            if(!(value %in% get_variables)) get_variables[[length(get_variables)+1]] <<- value
          }
        }))
        if(length(get_variables)>0) self$variables <- get_variables
        
      }
    },
    
    #setLayername
    setLayername = function(layername){
      self$layername <- layername
    },
    
    #setUploadType
    setUploadType = function(uploadType){
      if(!(uploadType %in% private$supportedUploadTypes)){
        errMsg <- sprintf("Upload type should be among values [%s]", paste0(private$supportedUploadTypes, collapse=","))
        stop(errMsg)
      }
      self$uploadType <- uploadType
    },
    
    #setUpload
    setUpload = function(upload){
      self$upload <- upload
    },
    
    #setUploadZip
    setUploadZip = function(uploadZip){
      self$uploadZip <- uploadZip
    },
    
    #setUploadZipOnly
    setUploadZipOnly = function(uploadZipOnly){
      self$uploadZipOnly <- uploadZipOnly
    },
    
    #setSource
    setSource = function(source){
      if(!is(source, "list")) source <- list(source)
      self$source <- source
    },
    
    #setSql
    setSql = function(sql){
      self$sql <- sql
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
    },
    
    #setFeatures
    setFeatures = function(features){
      self$features <- features
    }
    
  )
)