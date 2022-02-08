#' geoflow_data
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_data
#' @title Geoflow data class
#' @description This class models a data object
#' @keywords data
#' @return Object of \code{\link{R6Class}} for modelling a data object
#' @format \code{\link{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_data <- R6Class("geoflow_data",
  private = list(
    supportedSourceTypes = c("dbtable", "dbview", "dbquery","shp", "csv", "gpkg", "other","nc"),
    supportedUploadTypes = c("dbtable", "dbview", "dbquery","shp", "gpkg", "other","nc"),
    supportedGeomPossibleNames = c("the_geom", "geom", "wkt", "geom_wkt", "wkb", "geom_wkb"),
    supportedXPossibleNames = c("x","lon","long","longitude","decimalLongitude"),
    supportedYPossibleNames = c("y","lat","lati","latitude","decimalLatitude"),
    supportedSpatialRepresentationTypes = c("vector","grid")

  ),
  public = list(
    #'@field access accessor key for accessing sources. Default is 'default'
    access = "default",
    #'@field source source
    source = NULL,
    #'@field sourceSql sourceSql
    sourceSql = NULL,
    #'@field sourceType source type
    sourceType = "other",
    #'@field sourceZip create a zip for the sources
    sourceZip = FALSE,
    #'@field sourceZipOnly create a zip only for the sources, remove source files
    sourceZipOnly = FALSE,
    #'@field sql sql
    sql = NULL,
    #'@field upload upload
    upload = TRUE,
    #'@field uploadSource upload source name
    uploadSource = NULL,
    #'@field uploadType upload type
    uploadType = "other",
    #'@field cqlfilter CQL filter for filtering data
    cqlfilter = NULL,
    #'@field features features
    features = NULL,
    #'@field workspaces workspaces
    workspaces = list(),
    #'@field datastore datastore
    datastore = NULL,
    #'@field layername layer name
    layername = NULL,
    #'@field styles styles
    styles = list(),
    #'@field parameters parameters
    parameters = list(),
    #'@field geometryField geometry field
    geometryField = NULL,
    #'@field geometryType geometry type
    geometryType = NULL,
    #'@field featureType feature type name
    featureType = NULL,
    #'@field featureTypeObj feature type object
    featureTypeObj = NULL,
    #'@field attributes attributes
    attributes = NULL,
    #'@field variables variables
    variables = NULL,
    #'@field ogc_dimensions OGC dimensions
    ogc_dimensions = list(),
    #'@field dimensions dimensions
    dimensions = list(),
    #'@field spatialRepresentationType spatial representation type eg. "vector", "grid"
    spatialRepresentationType = "vector",
    #'@field actions local actions
    actions = list(),
    #'@field run whether to run local actions
    run = TRUE,
    
    #'@description Initializes an object of class \link{geoflow_data}
    #'@param str character string to initialize from, using key-based syntax
    initialize = function(str = NULL){
      if(!is.null(str)){
        data_props <-  extract_cell_components(sanitize_str(str))
        data_props <- lapply(data_props, function(data_prop){
          return(extract_kvp(data_prop))
        })
        names(data_props) <- sapply(data_props, function(x){x$key})
        
        #access to use for reaching sources
        if(!is.null(data_props$access)){
          access <- data_props$access$values[[1]]
          if(!access %in% list_data_accessors()$id){
            stop(sprintf("Value '%s' does not match any valid data accessor id. 
                         See valid values with geoflow::list_data_accessors()", access))
          }
          self$setAccess(access) 
        }
        
        #source
        if(!any(sapply(data_props, function(x){x$key=="source"}))){
          stop("The data 'source' is mandatory")
        }
        self$setSource(data_props$source$values)
        
        #sourceSql
        if(!is.null(data_props$sourceSql)){
          self$setSourceSql(data_props$sourceSql$values[[1]])
        }
        
        #sourceZip
        if(!is.null(data_props$sourceZip)){
          sourceZip <- as.logical(tolower(data_props$sourceZip$values[[1]]))
          if(!is.na(sourceZip)){
            self$setSourceZip(sourceZip) 
          }
        }else{
          self$setSourceZip(FALSE) 
        }
        
        #sourceZipOnly
        if(!is.null(data_props$sourceZipOnly)){
          sourceZipOnly <- as.logical(tolower(data_props$sourceZipOnly$values[[1]]))
          if(!is.na(sourceZipOnly)){
            self$setSourceZipOnly(sourceZipOnly) 
          }
        }else{
          self$setSourceZipOnly(FALSE) 
        }
        
        #sourceType
        if(!any(sapply(data_props, function(x){x$key=="sourceType"}))){
          self$setSourceType("other")
        }else{
          self$setSourceType(data_props$sourceType$values[[1]])
        }
        
        #uploadSource
        if(any(sapply(data_props, function(x){x$key=="uploadSource"}))){
          self$setUploadSource(data_props$uploadSource$values)
        }
        
        #uploadType
        names(data_props) <- sapply(data_props, function(x){x$key})
        if(any(sapply(data_props, function(x){x$key=="uploadType"}))){
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
        #20211130 this prevents from uploading data resources in generic uploaders (eg. zenodo)
        #if(startsWith(self$uploadType, "db")) self$setUpload(FALSE)
        
        #layername (if any)
        #not mandatory, can be used for subset layers
        if(!is.null(data_props$layername)){
          self$setLayername(data_props$layername$values[[1]])
        }
        
        #sql
        if(!is.null(data_props$sql)){
          sql <- paste(data_props$sql$values, collapse=",")
          self$setSql(sql)
        }
        
        #cql filter
        if(!is.null(data_props$cqlfilter)){
          self$setCqlFilter(data_props$cqlfilter$values[[1]])
        }
        #params
        params <- data_props[sapply(data_props, function(x){x$key=="parameter"})]
        if(length(params)>0){
          if(self$uploadType != "dbquery"){
            stop("The specification of service parameters is only possible for a 'dbquery' upload type!")
          }
          #check and set parameter
          for(param in params){
            if(!length(param$values) %in% c(2,3)){
              stop("Parameter definition should be compound by 3 elements: fieldname, alias, regexp and default value")
            }
            if(length(param$values)==2) param$values[[3]] <- ""
            fieldname <- param$values[[1]]
            param_alias <- attr(fieldname, "description")
            attr(fieldname, "description") <- NULL
            if(is.null(param_alias)) param_alias <- fieldname
            regexp <- param$values[[2]]
            defaultvalue <- param$values[[3]]
            self$setParameter(param_alias, fieldname, regexp, defaultvalue)
          }
          #check compliance of dbquery
          sqlquery <- self$sql
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
        if(length(workspaces)>0){
          for(value in workspaces[[1]]$values){
            software_type = attr(value, "uri")
            attributes(value) <- NULL
            self$setWorkspace(software_type, value)
          }
        }
        #datastore
        datastores <- data_props[sapply(data_props, function(x){x$key=="datastore"})]
        if(length(datastores)>0) self$setDatastore(datastores[[1]]$values[[1]])
        
        #feature type
        fts <- data_props[sapply(data_props, function(x){x$key=="featureType"})]
        if(length(fts)>0) self$setFeatureType(fts[[1]]$values[[1]])
        
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
        
        #spatialRepresentationType
        if(!any(sapply(data_props, function(x){x$key=="spatialRepresentationType"}))){
          self$setSpatialRepresentationType("vector")
        }else{
          self$spatialRepresentationType(data_props$spatialRepresentationType$values[[1]])
        }
        
        #run entity actions
        runs <- data_props[sapply(data_props, function(x){x$key=="run"})]
        if(length(runs)>0) self$run = as.logical(runs[[1]]$values[[1]])
        #entity actions
        actions <- data_props[sapply(data_props, function(x){x$key=="action"})]
        #entity action options (common to all local entity actions if more than one action)
        action_options <- data_props[sapply(data_props, function(x){startsWith(x$key, "action_option")})]
        action_options_names <- sapply(names(action_options), function(x){unlist(strsplit(x,"action_option_"))[2]})
        action_options <- lapply(action_options, function(x){
          out_opt <- unlist(x$values)
          if(length(out_opt)==1){
            if(!is.na(as.logical(out_opt))) out_opt <- as.logical(out_opt)
          }else{
            out_opt <- sapply(out_opt, function(el){
              if(!is.na(as.logical(el))) el <- as.logical(el)
              return(el)
            })
            names(out_opt) <- NULL
          }
          return(out_opt)
        })
        names(action_options) <- action_options_names
        
        if(length(actions)>0){
          for(action in actions){
            action <- action$values[[1]]
            script <- attr(action, "uri")
            
            isSourceUrl <- regexpr('(http|https)[^([:blank:]|\\\'|<|&|#\n\r)]+', script) > 0
            if(!isSourceUrl){
              if(!is_absolute_path(script)){
                script_to_source<-paste0("file.path(config$session_wd,\"",script,"\")")
              }else{
                script_to_source<-paste0("\"",script,"\"")
              }
            }else{
              script_to_source<-paste0("\"",script,"\"")
            }
              
            desc <- attr(action, "description")
            attributes(action) <- NULL
            entity_action <- geoflow_action$new(
              id = action,
              scope = "local",
              types = c("Entity data action"),
              def = desc,
              fun = eval(expr = parse(text = paste0("function(entity, config, options){
                source(",script_to_source,", local = TRUE)
              }"))),
              script = script,
              options = action_options
            )
            self$addAction(entity_action)
          }
        }
      }
    },
    
    #'@description Get allowed source values
    #'@return a vector of class \code{character}
    getAllowedSourceValues = function(){
      return(private$supportedAccessValues)
    },
    
    #'@description Set accessor id. See \code{list_data_accessors()} for available accessors
    #'@param access a data data accessor id
    setAccess = function(access){
      self$access <- access
    },
    
    #'@description Get allowed source types
    #'@return a vector of class \code{character}
    getAllowedSourceTypes = function(){
      return(private$supportedSourceTypes)
    },
    
    #'@description Set the source type.
    #' The source type is a simplification of the data mime type and is used to identify the type of source
    #' set for the data object. By default it is assumed to be "other" (undefined). The source types currently
    #' allowed in geoflow can be listed with \code{$getAllowedSourcedTypes()}.
    #'@param sourceType source type
    setSourceType = function(sourceType){
      if(!(sourceType %in% private$supportedSourceTypes)){
        errMsg <- sprintf("Source type should be among values [%s]", paste0(private$supportedSourceTypes, collapse=","))
        stop(errMsg)
      }
      self$sourceType <- sourceType
    },
    
    #'@description Get allowed Geometry possible names for coercing data to \pkg{sf} objects
    #'@param list of geom possible names
    getAllowedGeomPossibleNames = function(){
      return(private$supportedGeomPossibleNames)
    },
    
    #'@description Get allowed X possible names for coercing data to \pkg{sf} objects
    #'@param list of X possible names
    getAllowedXPossibleNames = function(){
      return(private$supportedXPossibleNames)
    },
    
    #'@description Get allowed Y possible names for coercing data to \pkg{sf} objects
    #'@param list of Y possible names
    getAllowedYPossibleNames = function(){
      return(private$supportedYPossibleNames)
    },
    
    #'@description Set source, object of class \code{"character"} (single source), or \code{list}.
    #'    For spatial source, a single source will be used, while for sources of type 'other'
    #'    (eg PDF files), multiple sources can be specified
    #' @param source source
    setSource = function(source){
      if(!is(source, "list")) source <- list(source)
      self$source <- source
    },
    
    #'@description This is a convenience method for users that want to specify directly
    #'    a SQL source. This method is called internally when a source SQL file has been set using
    #'    \code{setSource}
    #'@param sourceSql a source SQL query
    setSourceSql = function(sourceSql){
      self$sourceSql <- sourceSql
    },
    
    #'@description Sets whether a zipped version of the data file(s) should be created from source files. Default is \code{FALSE}
    #'@param sourceZip zip sources, object of class \code{logical}
    setSourceZip = function(sourceZip){
      self$sourceZip <- sourceZip
    },
    
    #'@description Sets whether a zipped version of the data file(s) only should be created from source files. Default is \code{FALSE}
    #'@param sourceZipOnly zip sources only, object of class \code{logical}
    setSourceZipOnly = function(sourceZipOnly){
      self$sourceZipOnly <- sourceZipOnly
    },
    
    #'@description Set the source to upload in output software, alternative to the source. If leave empty, the source will be used
    #'    as uploadSource. A typical use case is when we want to get a CSV source to import in a database, and use the
    #'    dbtable (or view/query) as upload source for publication in software like geoserver.
    #'@param uploadSource upload source
    setUploadSource = function(uploadSource){
      if(!is(uploadSource, "list")) uploadSource <- list(uploadSource)
      self$uploadSource <- uploadSource
    },
    
    #'@description Get allowed upload types
    #'@return the list of allowed upload types
    getAllowedUploadTypes = function(){
      return(private$supportedUploadTypes)
    },
    
    #'@description The upload type is a simplification of the data mime type and is used to identify the type of data uploaded. 
    #'    By default it is assumed to be "other" (undefined). The upload types currently allowed in geoflow can be 
    #'    listed with \code{$getAllowedUploadTypes()}.
    #'@param uploadType upload type
    setUploadType = function(uploadType){
      if(!(uploadType %in% private$supportedUploadTypes)){
        errMsg <- sprintf("Upload type should be among values [%s]", paste0(private$supportedUploadTypes, collapse=","))
        stop(errMsg)
      }
      self$uploadType <- uploadType
    },
    
    #'@description Set whether the source data should be uploaded to the sofware output declared in the geoflow
    #'    configuration or not. By default it is assumed that upload should be performed (upload \code{TRUE}).
    #'@param upload upload
    setUpload = function(upload){
      self$upload <- upload
    },
    
    #'@description Sets SQL for publication purpose. 
    #'@param sql sql
    setSql = function(sql){
      self$sql <- sql
    },
    
    #'@description Sets a CQL filter. In case of spatial data, once the data is read by geoflow (during initialization phase),
    #' the CQL filter will be applied to the data.
    #'@param cqlfilter CQL filter
    setCqlFilter = function(cqlfilter){
      self$cqlfilter <- cqlfilter
    },

    #'@description Set data features
    #'@param features features
    setFeatures = function(features){
      self$features <- features
    },
        
    #'@description Sets a workspace name, object of class \code{character}. A workspace must target a valid software type, object of
    #'    class \code{character}, to be declared as first argument of this function, assuming the corresponding software is
    #'    declared in the geoflow configuration.
    #'@param software_type sotware type where the workspace is identifier
    #'@param workspace workspace name
    setWorkspace = function(software_type, workspace){
      self$workspaces[[software_type]] <- workspace
    },
    
    #'@description Sets a datastore name, object of class \code{character}. Used as target datastore name for GeoServer action.
    #'@param datastore datastore
    setDatastore = function(datastore){
      self$datastore <- datastore
    },
    
    #'@description Sets a layername, object of class \code{character}. Used as target layer name for Geoserver action.
    #'@param layername layername
    setLayername = function(layername){
      self$layername <- layername
    },
    
    #'@description Adds a style name, object of class \code{character}. Used as layer style name(s) for GeoServer action.
    #'@param style style
    addStyle = function(style){
      self$styles <- c(self$styles, style)
    },
    
    #'@description Set virtual parameter definition for setting virtual SQL view parametized layers in Geoserver, when \code{uploadType} is
    #'    set to \code{dbquery}.The arguments here follow the definition of virtual parameters in GeoServer, ie a name (alias),
    #'    the corresponding field name in the DBMS, a regular expression for validation of parameter values (required to 
    #'    prevent SQL injection risks), and a default value.
    #'@param name name
    #'@param fieldname fieldname
    #'@param regexp regexp
    #'@param defaultvalue default value
    setParameter = function(name, fieldname, regexp, defaultvalue){
      self$parameters[[name]] <- list(
        name = name,
        fieldname = fieldname,
        regexp = regexp,
        defaultvalue = defaultvalue
      )
    },
    
    #'@description Sets the name of the geometry field in the target GeoServer SQL view parametrized layer
    #'@param geometryField geometry field
    setGeometryField = function(geometryField){
      self$geometryField <- geometryField
    },
    
    #'@description Sets the name of the geometry field in the target GeoServer SQL view parametrized layer
    #'@param geometryType geometry type
    setGeometryType = function(geometryType){
      self$geometryType <- geometryType
    },
    
    #'@description Sets a feature type (ID) to link data with a dictionnary
    #'@param featureType feature type name
    setFeatureType = function(featureType){
      self$featureType = featureType
    },
    
    #'@description Sets a feature type object
    #'@param featureTypeObj feature type object of class \code{geoflow_featuretype}
    setFeatureTypeObj = function(featureTypeObj){
      self$featureTypeObj = featureTypeObj
    },
    
    #'@description Set attributes, as simple way to describe attributes without binding to a proper \link{geoflow_dictionnary}.
    #'@param attributes attributes
    setAttributes = function(attributes){
      self$attributes <- attributes
    },
    
    #'@description Set variables, as simple way to describe variables without binding to a proper \link{geoflow_dictionnary}.
    #'@param variables variables
    setVariables = function(variables){
      self$variables <- variables
    },
    
    #'@description Adds a local action
    #'@param action object of class \link{geoflow_action}
    addAction = function(action){
      self$actions[[length(self$actions)+1]] <- action
    },
    
    #'@description Set OGC dimensions
    #'@param name dimension name
    #'@param values dimension values
    setOgcDimensions = function(name, values){
      self$ogc_dimensions[[name]] <- values
    },
    
    #'@description Adds a dimension
    #'@param name dimension name
    #'@param dimension object of class \link{geoflow_dimension}
    addDimension = function(name,dimension){
      if(!is(dimension, "geoflow_dimension")){
        stop("The argument should be an object of class 'geoflow_dimension'")
      }
      self$dimensions[[name]] <- dimension
    },
    
    #'@description Get allowed spatial representation types, typically "vector" and "grid"
    #'@return an object of class \code{character}
    getAllowedSpatialRepresentationTypes = function(){
      return(private$supportedSpatialRepresentationTypes)
    },
    
    #'@description Set spatial representation type for the data considered
    #'@param spatialRepresentationType spatial representation type
    setSpatialRepresentationType = function(spatialRepresentationType){
      if(!(spatialRepresentationType %in% private$supportedSpatialRepresentationTypes)){
        errMsg <- sprintf("Spatial representation type should be among values [%s]", paste0(private$supportedSpatialRepresentationTypes, collapse=","))
        stop(errMsg)
      }
      self$spatialRepresentationType <- spatialRepresentationType
    },
    
    #'@description A function triggered when loading a data object to check eventual software dependent properties, to make sure
    #'    the corresponding software are declared in the config.
    #'@param config geoflow config object
    checkSoftwareProperties = function(config){
      
      #check workspace related software
      software_types <- names(self$workspaces)
      for(software_type in software_types){
        workspace <- self$workspaces[[software_type]]
        config$logger.info(sprintf("Check '%s' software availability for workspace '%s'", software_type, workspace))
        if(!software_type %in% names(config$software$input) &&
           !software_type %in% names(config$software$output)){
          errMsg <- sprintf("No software '%s' declared as input/output for workspace '%s'.", software_type, workspace)
          config$logger.error(errMsg)
          stop(errMsg)
        }
      }
      
    }
    
  )
)