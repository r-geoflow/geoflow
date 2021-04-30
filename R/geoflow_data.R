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
#' @section Methods:
#' \describe{
#'  \item{\code{new(str)}}{
#'    This method is used to instantiate a geoflow_data object
#'  }
#'  \item{\code{getAllowedSourceTypes()}}{
#'    get the allowed source types
#'  }
#'  \item{\code{setSourceType(sourceType)}}{
#'    The source type is a simplification of the data mime type and is used to identify the type of source
#'    set for the data object. By default it is assumed to be "other" (undefined). The source types currently
#'    allowed in geoflow can be listed with \code{$getAllowedSourcedTypes()}. Those are: "other", "shp" (for zipped
#'    ESRI shapefiles), "dbtable", "dbview", "dbquery".
#'  }
#'  \item{\code{setSource(source)}}{
#'    Set source, object of class \code{"character"} (single source), or \code{list}.
#'    For spatial source, a single source will be used, while for sources of type 'other'
#'    (eg PDF files), multiple sources can be specified
#'  }
#'  \item{\code{setSourcedZip(sourceZip)}}{
#'    Sets whether a zipped version of the data file(s) should be created from source files. Default is \code{FALSE}
#'  }
#'  \item{\code{setSourceZipOnly(sourceZipOnly)}}{
#'    Sets whether a zipped version of the data file(s) only should be created from source files. Default is \code{FALSE}
#'  }
#'  \item{setUpload(upload)}{
#'    Set whether the source data should be uploaded to the sofware output declared in the geoflow
#'    configuration or not. By default it is assumed that upload should be performed (upload \code{TRUE}).
#'  }
#'  \item{setUploadSource(uploadSource)}{
#'    Set the source to upload in output software, alternative to the source. If leave empty, the source will be used
#'    as uploadSource. A typical use case is when we want to get a CSV source to import in a database, and use the
#'    dbtable (or view/query) as upload source for publication in software like geoserver.
#'  }
#'  \item{\code{getAllowedUploadTypes()}}{
#'    get the allowed upload types
#'  }
#'  \item{\code{setUploadType(uploadType)}}{
#'    The upload type is a simplification of the data mime type and is used to identify the type of data uploaded. 
#'    By default it is assumed to be "other" (undefined). The upload types currently allowed in geoflow can be 
#'    listed with \code{$getAllowedUploadTypes()}. Those are: "other", "shp" (for zipped ESRI shapefiles), "dbtable", 
#'    "dbview", "dbquery".
#'  }
#'  \item{\code{setSql(sql)}}{
#'    Sets SQL source. This is a convenience method for users that want to specify directly
#'    a SQL source. This method is called internally when a source SQL file has been set using
#'    \code{setSource}
#'  }
#'  \item{\code{setCqlFilter(cqlfilter)}}{
#'    Sets a CQL filter. In case of spatial data, once the data is read by geoflow (during initialization phase),
#'    the CQL filter will be applied to the data.
#'  }
#'  \item{\code{setWorkspace(software_type, workspace)}}{
#'    Sets a workspace name, object of class \code{character}. A workspace must target a valid software type, object of
#'    class \code{character}, to be declared as first argument of this function, assuming the corresponding software is
#'    declared in the geoflow configuration.
#'  }
#'  \item{\code{setDatastore(datastore)}}{
#'    Sets a datastore name, object of class \code{character}. Used as target datastore name for GeoServer action.
#'  }
#'  \item{\code{setLayername(layername)}}{
#'    Sets a layername, object of class \code{character}. Used as target layer name for Geoserver action.
#'  }
#'  \item{\code{addStyle(style)}}{
#'    Adds a style name, object of class \code{character}. Used as layer style name(s) for GeoServer action.
#'  }
#'  \item{\code{setParameter(name, fieldname, regexp, defaultvalue)}}{
#'    Set virtual parameter definition for setting virtual SQL view parametized layers in Geoserver, when \code{uploadType} is
#'    set to \code{dbquery}.The arguments here follow the definition of virtual parameters in GeoServer, ie a name (alias),
#'    the corresponding field name in the DBMS, a regular expression for validation of parameter values (required to 
#'    prevent SQL injection risks), and a default value.
#'  }
#'  \item{\code{setGeometryField(geometryField)}}{
#'    Sets the name of the geometry field in the target GeoServer SQL view parametrized layer
#'  }
#'  \item{\code{setGeometryType(geometryType)}}{
#'    Sets the geometry type in the target GeoServer SQL view parametrized layer
#'  }
#'  \item{\code{setAttributes(attributes)}}{
#'    Sets attributes definition.
#'  }
#'  \item{\code{setVariables(variables)}}{
#'    Sets variables definition.
#'  }
#'  \item{\code{addAction(action)}}{
#'    Adds an entity local action to be run
#'  }
#'  \item{\code{checkSoftwareProperties(config)}}{
#'    A function triggered when loading a data object to check eventual software dependent properties, to make sure
#'    the corresponding software are declared in the config.
#'  }
#'  
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_data <- R6Class("geoflow_data",
  private = list(
    supportedSourceTypes = c("dbtable", "dbview", "dbquery","shp", "csv", "gpkg", "other"),
    supportedUploadTypes = c("dbtable", "dbview", "dbquery","shp", "gpkg", "other"),
    supportedGeomPossibleNames = c("the_geom", "geom", "wkt", "geom_wkt", "wkb", "geom_wkb"),
    supportedXPossibleNames = c("x","lon","long","longitude","decimalLongitude"),
    supportedYPossibleNames = c("y","lat","lati","latitude","decimalLatitude")

  ),
  public = list(
    access = "default",
    source = NULL,
    sourceSql = NULL,
    sourceType = "other",
    sourceZip = FALSE,
    sourceZipOnly = FALSE,
    sql = NULL,
    upload = TRUE,
    uploadSource = NULL,
    uploadType = "other",
    cqlfilter = NULL,
    features = NULL,
    workspaces = list(),
    datastore = NULL,
    layername = NULL,
    styles = list(),
    parameters = list(),
    geometryField = NULL,
    geometryType = NULL,
    featureType = NULL,
    featureTypeObj = NULL,
    attributes = NULL,
    variables = NULL,
    actions = list(),
    run = TRUE,
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
        if(startsWith(self$uploadType, "db")) self$setUpload(FALSE)
        
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
            desc <- attr(action, "description")
            attributes(action) <- NULL
            entity_action <- geoflow_action$new(
              id = action,
              type = "Entity data action",
              def = desc,
              fun = eval(expr = parse(text = paste0("function(entity, config, options){
                source(\"",script,"\", local = TRUE)
              }"))),
              script = script,
              options = action_options
            )
            self$addAction(entity_action)
          }
        }
      }
    },
    
    #getAllowedSourceValues
    getAllowedSourceValues = function(){
      return(private$supportedAccessValues)
    },
    
    #setAccess
    setAccess = function(access){
      self$access <- access
    },
    
    #getAllowedSourceTypes
    getAllowedSourceTypes = function(){
      return(private$supportedSourceTypes)
    },
    
    #setSourceType
    setSourceType = function(sourceType){
      if(!(sourceType %in% private$supportedSourceTypes)){
        errMsg <- sprintf("Source type should be among values [%s]", paste0(private$supportedSourceTypes, collapse=","))
        stop(errMsg)
      }
      self$sourceType <- sourceType
    },
    
    #getAllowedGeomPossibleNames
    getAllowedGeomPossibleNames = function(){
      return(private$supportedGeomPossibleNames)
    },
    
    #getAllowedXPossibleNames
    getAllowedXPossibleNames = function(){
      return(private$supportedXPossibleNames)
    },
    #getAllowedYPossibleNames
    getAllowedYPossibleNames = function(){
      return(private$supportedYPossibleNames)
    },
    
    #setSource
    setSource = function(source){
      if(!is(source, "list")) source <- list(source)
      self$source <- source
    },
    
    #setSourceSql
    setSourceSql = function(sourceSql){
      self$sourceSql <- sourceSql
    },
    
    #setSourceZip
    setSourceZip = function(sourceZip){
      self$sourceZip <- sourceZip
    },
    
    #setSourceZipOnly
    setSourceZipOnly = function(sourceZipOnly){
      self$sourceZipOnly <- sourceZipOnly
    },
    
    #setUploadSource
    setUploadSource = function(uploadSource){
      if(!is(uploadSource, "list")) uploadSource <- list(uploadSource)
      self$uploadSource <- uploadSource
    },
    
    #getAllowedUploadTypes
    getAllowedUploadTypes = function(){
      return(private$supportedUploadTypes)
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
    
    #setSql
    setSql = function(sql){
      self$sql <- sql
    },
    
    #setCqlFilter
    setCqlFilter = function(cqlfilter){
      self$cqlfilter <- cqlfilter
    },

    #setFeatures
    setFeatures = function(features){
      self$features <- features
    },
        
    #setWorkspace
    setWorkspace = function(software_type, workspace){
      self$workspaces[[software_type]] <- workspace
    },
    
    #setDatastore
    setDatastore = function(datastore){
      self$datastore <- datastore
    },
    
    #setLayername
    setLayername = function(layername){
      self$layername <- layername
    },
    
    #addStyle
    addStyle = function(style){
      self$styles <- c(self$styles, style)
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
    
    #setFeatureType
    setFeatureType = function(featureType){
      self$featureType = featureType
    },
    
    #setFeatureTypeObj
    setFeatureTypeObj = function(featureTypeObj){
      self$featureTypeObj = featureTypeObj
    },
    
    #setAttributes
    setAttributes = function(attributes){
      self$attributes <- attributes
    },
    
    #setVariables
    setVariables = function(variables){
      self$variables <- variables
    },
    
    #addAction
    addAction = function(action){
      self$actions[[length(self$actions)+1]] <- action
    },
    
    #checkSoftwareProperties
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