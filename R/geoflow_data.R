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
    supportedSourceTypes = c("dbtable", "dbview", "dbquery", "zip", "shp", "csv", "gpkg", "other","nc", "geotiff","parquet"),
    supportedUploadTypes = c("dbtable", "dbview", "dbquery", "shp", "gpkg", "other","nc", "geotiff", "parquet"),
    supportedGeomPossibleNames = c("the_geom", "geom", "wkt", "geom_wkt", "wkb", "geom_wkb"),
    supportedXPossibleNames = c("x","lon","long","longitude","decimalLongitude"),
    supportedYPossibleNames = c("y","lat","lati","latitude","decimalLatitude"),
    supportedSpatialRepresentationTypes = c("vector","grid"),
    supportedEnvelopeCompositionTypes = c("UNION", "INTERSECTION")
  ),
  public = list(
    
    #'@field dir an object of class \code{character} giving a data directory
    dir = NULL,
    #'@field data list of object of class \link{geoflow_data} in case we point to a data directory
    data = list(),
    
    #ACCESS / SOURCE related fields
    #----------------------------------------------------------------------------
    #'@field access accessor key for accessing sources. Default is 'default'
    access = "default",
    #'@field source source
    source = NULL,
    #'@field sourceSql sourceSql
    sourceSql = NULL,
    #'@field sourceType source type
    sourceType = "other",
    #'@field sourceZip create a zip for the sources (DEPRECATED with #344)
    sourceZip = FALSE,
    #'@field sourceZipOnly create a zip only for the sources, remove source files (DEPRECATED with #344)
    sourceZipOnly = FALSE,
    
    #UPLOAD related fields
    #----------------------------------------------------------------------------
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
    #'@field workspaces workspaces
    workspaces = list(),
    #'@field store store
    store = NULL,
    #'@field layername layer name
    layername = NULL,
    #'@field layertitle layer title
    layertitle = NULL,
    #'@field layerdesc layer description
    layerdesc = NULL,
    #'@field layeruri layer URI
    #'layeruri layer URI
    layeruri = NULL,
    #'@field styles styles
    styles = list(),
    #'@field styleUpload upload styles
    styleUpload = TRUE,
    #'@field dimensions dimensions
    dimensions = list(),
    #'@field cloud_path a relative path in a cloud storage (e.g., OCS)
    cloud_path = "/",
    
    #resource generic fields
    #----------------------------------------------------------------------------
    #'@field spatialRepresentationType spatial representation type eg. "vector", "grid"
    spatialRepresentationType = "vector",
    #'@field ogc_dimensions OGC dimensions
    ogc_dimensions = list(),
    
    #featuretype fields
    #----------------------------------------------------------------------------
    #'@field features features
    features = NULL,
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
    
    #coverage fields 
    #----------------------------------------------------------------------------
    #'@field coverages coverages
    coverages = NULL,
    #'@field envelopeCompositionType envelope composition type (for coverages)
    envelopeCompositionType = NULL,
    #'@field selectedResolution selected resolution (for coverages)
    selectedResolution = NULL,
    #'@field selectedResolutionIndex selected resolution index (for coverages)
    selectedResolutionIndex = NULL,
    #'@field bands list of bands
    bands = list(),
    
    #geoflow execution related fields
    #----------------------------------------------------------------------------
    #'@field actions local actions
    actions = list(),
    #'@field run whether to run local actions
    run = TRUE,
    
    #'@description Initializes an object of class \link{geoflow_data}
    #'@param str character string to initialize from, using key-based syntax
    #'@param config a geoflow config, if available and needed
    initialize = function(str = NULL, config = NULL){
      if(!is.null(str)){
        data_props <-  extract_cell_components(sanitize_str(str))
        data_props <- lapply(data_props, function(data_prop){
          return(extract_kvp(data_prop))
        })
        names(data_props) <- sapply(data_props, function(x){x$key})
        
        #spatialRepresentationType
        if(!any(sapply(data_props, function(x){x$key=="spatialRepresentationType"}))){
          self$setSpatialRepresentationType("vector")
        }else{
          self$setSpatialRepresentationType(data_props$spatialRepresentationType$values[[1]])
        }
        
        #access to use for reaching sources
        if(!is.null(data_props$access)){
          access <- data_props$access$values[[1]]
          if(!access %in% list_data_accessors()$id){
            stop(sprintf("Value '%s' does not match any valid data accessor id. 
                         See valid values with geoflow::list_data_accessors()", access))
          }
          self$setAccess(access) 
        }
        
        
        #sourceType
        if(!any(sapply(data_props, function(x){x$key=="sourceType"}))){
          self$setSourceType("other")
        }else{
          self$setSourceType(data_props$sourceType$values[[1]])
        }
        
        #source
        if(!self$sourceType %in% c("dbtable", "dbquery", "dbview")){
          if(!any(sapply(data_props, function(x){x$key=="source"})) && !any(sapply(data_props, function(x){x$key=="dir"}))){
            stop("One or more data 'source' (or 'dir', as directory for sources) is mandatory")
          }
        }
        if(any(sapply(data_props, function(x){x$key=="source"}))) self$setSource(data_props$source$values)
          
        #sourceSql
        if(!is.null(data_props$sourceSql)){
          sourceSql <- paste(data_props$sourceSql$values, collapse=",")
          self$setSourceSql(sourceSql)
        }
        
        #sourceZip (DEPRECATED with #344)
        #if(!is.null(data_props$sourceZip)){
        #  sourceZip <- as.logical(tolower(data_props$sourceZip$values[[1]]))
        #  if(!is.na(sourceZip)){
        #    self$setSourceZip(sourceZip) 
        #  }
        #}else{
        #  self$setSourceZip(FALSE) 
        #}
        
        #sourceZipOnly (DEPRECATED with #344)
        #if(!is.null(data_props$sourceZipOnly)){
        #  sourceZipOnly <- as.logical(tolower(data_props$sourceZipOnly$values[[1]]))
        #  if(!is.na(sourceZipOnly)){
        #    self$setSourceZipOnly(sourceZipOnly) 
        #  }
        #}else{
        #  self$setSourceZipOnly(FALSE) 
        #}
        
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
        
        #styles upload
        if(!is.null(data_props$styleUpload)){
          styleUpload <- as.logical(tolower(data_props$styleUpload$values[[1]]))
          if(!is.na(styleUpload)){
            self$setStyleUpload(styleUpload) 
          }
        }else{
          self$setStyleUpload(TRUE)
        }
        
        #layername (if any)
        if(!is.null(data_props$layername)){
          self$setLayername(data_props$layername$values[[1]])
        }
        #layertitle (if any)
        if(!is.null(data_props$layertitle)){
          self$setLayertitle(data_props$layertitle$values[[1]])
        }
        #layerdesc (if any)
        if(!is.null(data_props$layerdesc)){
          self$setLayerdesc(data_props$layerdesc$values[[1]])
        }
        #layeruri (if any)
        if(!is.null(data_props$layeruri)){
          self$setLayeruri(data_props$layeruri$values[[1]])
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
        
        #coverage view properties
        if(!is.null(data_props$envelopeCompositionType)){
          self$setEnvelopeCompositionType(data_props$envelopeCompositionType$values[[1]])
        }
        if(!is.null(data_props$selectedResolution)){
          self$setSelectedResolution(data_props$selectedResolution$values[[1]])
        }
        if(!is.null(data_props$selectedResolutionIndex)){
          self$setSelectedResolutionIndex(data_props$selectedResolutionIndex$values[[1]])
        }
        bands <- data_props[sapply(data_props, function(x){x$key=="band"})]
        if(length(bands)>0){
          if(self$spatialRepresentationType != "grid"){
            stop("The specification of bands is only possible for a grid spatial representation!")
          }
          if(self$uploadType != "geotiff"){ #TODO to extend to other coverage formats
            stop(" The specification of bands is only possible for a 'geotiff' upload type")
          }
          #check and set parameter
          for(band in bands){
            if(length(band$values) != 2){
              stop("Band definition should be compound by 2 elements: name (coverage name), index")
            }
            covname <- band$values[[1]]
            index <- band$values[[2]]
            self$setBand(covname, index)
          }
        }
        
        #layer styles
        styles <- data_props[sapply(data_props, function(x){x$key=="style"})]
        if(length(styles)>0){
          for(style in styles){
            for(style_val in style$values){
              self$addStyle(style_val)
            }
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
        #store
        stores <- data_props[sapply(data_props, function(x){x$key %in% c("datastore","store")})] #keep datastore for backward compatibility
        if(length(stores)>0) self$setDatastore(stores[[1]]$values[[1]])
        
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
              fun = eval(expr = parse(text = paste0("function(action, entity, config){
                act_fun <- source(",script_to_source,", local = TRUE)$value
                if(!is(act_fun, \"function\")) stop(\"Script for entity action ",action," is not a function!\")
                act_fun(action, entity, config)
              }"))),
              script = script,
              options = action_options
            )
            self$addAction(entity_action)
          }
        }
        
        #datadir
        if(any(sapply(data_props, function(x){x$key=="dir"}))){
          accessor <- NULL
          accessor_software <- NULL
          data_dir <- data_props$dir$values[[1]]
          self$dir <- data_dir
          ext_data_files <- list()
          ext_sld_files <- list()
          all_data_files = list.files(data_dir, full.names = T)
          if(self$access == "default"){
            if(!is_absolute_path(data_dir) && !is.null(config)) data_dir <- file.path(config$session_wd, datasource_uri)
            if(!dir.exists(data_dir)){
              config$logger.error("Data dir doesn't exist!")
            }
            #local access
            #TODO remote access
            if(!is.null(self$sourceType) && self$sourceType != "other"){
              ext <- switch(self$sourceType,
                "shp" = "zip",
                "gpkg" = "gpkg",
                "geotiff" = "tif",
                self$sourceType
              )
              ext_data_files <- list.files(data_dir, pattern = paste0(".", ext), full.names = T)
            }else{
              ext_data_files <- all_data_files
            }
            #exclude dirs
            ext_data_files = ext_data_files[!dir.exists(ext_data_files)]
          }else{
            if(!is.null(config)){
              #remote access through custom API accessors
              accessors <- list_data_accessors(raw = TRUE)
              accessor <- accessors[sapply(accessors, function(x){x$id == self$access})][[1]]
              
              config$logger.info(sprintf("Copying data to job data directory from remote file(s) using accessor '%s'", accessor$id))
              access_software <- NULL
              if(!is.na(accessor$software_type)){
                config$logger.info(sprintf("Accessor '%s' seems to require a software. Try to locate 'input' software", accessor$id))
                accessor_software <- config$software$input[[accessor$software_type]]
                if(is.null(accessor_software)){
                  config$logger.info(sprintf("Accessor '%s' doesn't seem to have the required 'input' software. Try to locate 'output' software", accessor$id))
                  accessor_software <- config$software$output[[accessor$software_type]]
                }
              }
              if(!is.null(accessor$list)){
                ext_data_files <- accessor$list(resource = data_dir, software = accessor_software)
              }else{
                errMsg <- sprintf("No data access 'list' method for accessor '%s'", )
                if(!is.null(config)) config$logger.error(errMsg)
                stop(errMsg)
              }
            }else{
              errMsg <- sprintf("No config available to invoke data accessor '%s'", )
              if(!is.null(config)) config$logger.error(errMsg)
              stop(errMsg)
            }
          }
          
          if(length(all_data_files)>0){
            ext_sld_files <- all_data_files[endsWith(all_data_files,".sld")]
            ext_data_files <-ext_data_files[!endsWith(ext_data_files,".sld")]
          }
 
          #detect presence of data files register
          data_files_register <- NULL
          data_files_register_file = all_data_files[basename(all_data_files) == "register.csv"]
          if(length(data_files_register_file)>0){
            data_files_register_file = data_files_register_file[1]
            target_register_file = data_files_register_file
            if(!is.null(accessor)){
              target_register_file <- file.path(tempdir(), "register.csv")
              accessor$download(
                resource = data_files_register_file,
                file = "register.csv", 
                path = target_register_file,
                software = accessor_software,
                unzip = FALSE
              )
            }
            data_files_register = as.data.frame(readr::read_csv(target_register_file))
            register_colnames = c("code","uri","label","definition")
            if(!all(register_colnames %in% colnames(data_files_register))){
             stop("A data files register has been found but doesn't follow the standard structure (code,uri,label,definition)") 
            }
          }
          
          #geoflow build children
          ext_data_files = ext_data_files[basename(ext_data_files) != "register.csv"]
          if(length(ext_data_files)>0){
            self$data <- lapply(ext_data_files, function(data_file){
              ext_data <- self$clone(deep = TRUE) #clone parent geoflow_data to inherit all needed properties
              ext_data$dir <- NULL
              ext_data_src <- basename(data_file)
              ext_data_name <- unlist(strsplit(ext_data_src, "\\."))[1]
              ext_data_extension <- unlist(strsplit(ext_data_src, "\\."))[2]
              attr(ext_data_src, "uri") <- data_file
              ext_data$addSource(ext_data_src)
              uploadSource = paste0(ext_data_name, ".", ext_data_extension)
              if(ext_data_extension %in% c("shp","gpkg")) uploadSource = paste0(ext_data_name, ".zip")
              ext_data$setUploadSource(uploadSource)
              sourceType <- self$sourceType
              if(is.null(self$sourceType) || self$sourceType == "other"){
                sourceType <- switch(ext_data_extension,
                  "shp" = "shp",
                  "gpkg" = "gpkg",
                  "tif" = "geotiff",
                  "csv" = "csv",
                  "parquet" = "parquet",
                  "other" #including zip that will be resolved later when entity is enriched with data
                )
              }
              if(!is.null(sourceType)){
                ext_data$setSourceType(sourceType)
              }
              if((is.null(self$uploadType) || self$uploadType == "other") && !is.null(sourceType)){
                print(sourceType)
                if(sourceType != "zip") ext_data$setUploadType(sourceType)
                if(!is.null(ext_data$uploadType)) if(ext_data$uploadType == "geotiff") ext_data$setSpatialRepresentationType("grid")
              }
              
              hasStoreDeclared <- FALSE
              if(!is.null(config$software$output$geoserver_config$properties$store)) hasStoreDeclared <- TRUE
              if(!is.null(self$store)) hasStoreDeclared <- TRUE
              if(!hasStoreDeclared) ext_data$setStore(ext_data_name)
              ext_data$setLayername(ext_data_name)
              
              #inherit layer metadata from data file register (if any)
              if(!is.null(data_files_register)){
                register_entry = data_files_register[data_files_register$code == ext_data_name,]
                if(nrow(register_entry)>0){
                  register_entry = register_entry[1L,]
                  if(!is.na(register_entry$uri)) ext_data$setLayeruri(register_entry$uri)
                  if(!is.na(register_entry$label)) ext_data$setLayertitle(register_entry$label)
                  if(!is.na(register_entry$definition)) ext_data$setLayerdesc(register_entry$definition)
                }
              }
              
              if(self$styleUpload){
                #we add all sld files to each child so they can be downloaded if needed
                if(length(ext_sld_files)>0) for(ext_sld_file in ext_sld_files) {
                  exists <- TRUE
                  if(self$access == "default") exists <- file.exists(ext_sld_file)
                  if(exists){
                    ext_sld_src <- basename(ext_sld_file)
                    ext_sld_name <- unlist(strsplit(ext_sld_src, "\\."))[1]
                    ext_sld_extension <- unlist(strsplit(ext_sld_src, "\\."))[2]
                    attr(ext_sld_src, "uri") <- ext_sld_file
                    ext_data$addSource(ext_sld_src)
                    ext_data$addStyle(ext_sld_name)
                  }
                }
              }
              
              return(ext_data)
            })
          }
        }
        
        #cloud_path
        cloud_path <- data_props[sapply(data_props, function(x){x$key=="cloud_path"})]
        if(length(cloud_path)>0) self$setCloudPath(cloud_path[[1]]$values[[1]])
        
      }
    },
    
    #'@description Get data directory where datasets are scanned to build \code{geoflow_data} objects
    #'@return an object of class \code{character}
    getDir = function(){
      return(self$dir)
    },
    
    #'@description Get a lis tof \code{geoflow_data} objects built from a directory
    #'@return a list of objects of class \code{geoflow_data}
    getData = function(){
      return(self$data)
    },
    
    #ACCESS / SOURCE related methods
    #----------------------------------------------------------------------------
    
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
    
    #'@description Add source, object of class \code{"character"} (single source)
    #'@param source source
    addSource = function(source){
      if(is.null(self$source)) self$source <- list()
      self$source[[length(self$source)+1]] <- source
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
    
    #UPLOAD related methods
    #----------------------------------------------------------------------------
    
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
    
    #'@description Set whether styles in source data should be uploaded, by default \code{TRUE}
    #'@param styleUpload style upload
    setStyleUpload = function(styleUpload){
      self$styleUpload <- styleUpload
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
        
    #'@description Sets a workspace name, object of class \code{character}. A workspace must target a valid software type, object of
    #'    class \code{character}, to be declared as first argument of this function, assuming the corresponding software is
    #'    declared in the geoflow configuration.
    #'@param software_type sotware type where the workspace is identifier
    #'@param workspace workspace name
    setWorkspace = function(software_type, workspace){
      self$workspaces[[software_type]] <- workspace
    },
    
    #'@description Sets a data/coverage store name, object of class \code{character}. Used as target data/coverage store name for GeoServer action.
    #'@param store store
    setStore = function(store){
      self$store <- store
    },
    
    #'@description Sets a datastore name, object of class \code{character}. Used as target datastore name for GeoServer action. DEPRECATED, use \code{setStore}
    #'@param datastore datastore
    setDatastore = function(datastore){
      self$setStore(store = datastore)
    },
    
    #'@description Sets a layername, object of class \code{character}. Used as target layer name for Geoserver action.
    #'@param layername layername
    setLayername = function(layername){
      self$layername <- layername
    },
    
    #'@description Sets a layer title, object of class \code{character}. If available, used as target layer title in SDI-related action.
    #'@param layertitle layertitle
    setLayertitle = function(layertitle){
      self$layertitle = layertitle
    },
    
    #'@description Sets a layer description, object of class \code{character}. If available, used as target layer description/abstract in SDI-related actions.
    #'@param layerdesc layerdesc
    setLayerdesc = function(layerdesc){
      self$layerdesc = layerdesc
    },
    
    #'@description Sets a layer URI, object of class \code{character}. If available, used as annotating URI for layer metadata (eg. in ISO 19115 action).
    #'@param layeruri layeruri
    setLayeruri = function(layeruri){
      self$layeruri = layeruri
    },
    
    #'@description Adds a style name, object of class \code{character}. Used as layer style name(s) for GeoServer action.
    #'@param style style
    addStyle = function(style){
      self$styles[[length(self$styles)+1]] <- style
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
    
    #'@description Set cloud path
    #'@param cloud_path cloud path
    setCloudPath = function(cloud_path){
      self$cloud_path = cloud_path
    },
    
    #GENERIC RESOURCE related methods
    #----------------------------------------------------------------------------
    
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
    
    #'@description Set OGC dimensions
    #'@param name dimension name
    #'@param values dimension values
    setOgcDimensions = function(name, values){
      self$ogc_dimensions[[name]] <- values
    },
    
    #FeatureType related methods
    #----------------------------------------------------------------------------
    
    #'@description Set data features
    #'@param features features
    setFeatures = function(features){
      self$features <- features
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
    
    #'@description Set attributes, as simple way to describe attributes without binding to a proper \link{geoflow_dictionary}.
    #'@param attributes attributes
    setAttributes = function(attributes){
      self$attributes <- attributes
    },
    
    #'@description Set variables, as simple way to describe variables without binding to a proper \link{geoflow_dictionary}.
    #'@param variables variables
    setVariables = function(variables){
      self$variables <- variables
    },
    
    #Coverage related methods
    #----------------------------------------------------------------------------
    
    #'@description Set coverages
    #'@param coverages coverages
    setCoverages = function(coverages){
      self$coverages <- coverages
    },
    
    #'@description Get allowed envelope composition types
    #'@return an object of class \code{character}
    getAllowedEnvelopeCompositionTypes = function(){
      return(private$supportedEnvelopeCompositionTypes)
    },
    
    #'@description Set envelope composition type
    #'@param envelopeCompositionType envelope composition type, either 'UNION' or 'INTERSECTION'
    setEnvelopeCompositionType = function(envelopeCompositionType){
      if(!(envelopeCompositionType %in% self$getAllowedEnvelopeCompositionTypes())){
        errMsg <- sprintf("Envelope composition type should be among values [%s]", paste0(self$getAllowedEnvelopeCompositionTypes(), collapse=","))
        stop(errMsg)
      }
      self$envelopeCompositionType <- envelopeCompositionType
    },
    
    #'@description Set selected resolution
    #'@param selectedResolution selected resolution
    setSelectedResolution = function(selectedResolution){
      self$selectedResolution <- selectedResolution
    },
    
    #'@description Set selected resolution index
    #'@param selectedResolutionIndex selected resolution index
    setSelectedResolutionIndex = function(selectedResolutionIndex){
      self$selectedResolutionIndex <- selectedResolutionIndex
    },
    
    #'@description Set band
    #'@param name band name
    #'@param index band index
    setBand = function(name, index){
      self$bands[[paste0(name,"@",index)]] <- list(
        name = name,
        index = index
      )
    },
    
    #geoflow execution related methods
    #----------------------------------------------------------------------------
    
    #'@description Adds a local action
    #'@param action object of class \link{geoflow_action}
    addAction = function(action){
      self$actions[[length(self$actions)+1]] <- action
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