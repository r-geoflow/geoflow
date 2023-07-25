#' geoflow_entity
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_entity
#' @title Geoflow entity class
#' @description This class models a entity object
#' @keywords entity
#' @return Object of \code{\link{R6Class}} for modelling a entity object
#' @format \code{\link{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_entity <- R6Class("geoflow_entity",
  private = list(
    #TODO manage these allowed key values in class definitions (eg. geoflow_format)
    allowedKeyValuesFor = list(
      identifiers = c("id", "id_version", "uuid", "doi", "packageId"),
      titles = c("title", "alternative"),
      descriptions = c("abstract", "purpose", "credit", "info", "edition", "status"),
      spatialCoverage = c("ewkt", "wkt", "srid"),
      formats = c("resource","distribution")
    ) 
  ),
  public = list(
    #'@field identifiers entity identifiers
    identifiers = list(),
    #'@field dates entity dates
    dates = list(),
    #'@field language entity language
    language = "eng",
    #'@field types entity types
    types = list(generic = "dataset"),
    #'@field titles entity titles
    titles = list(),
    #'@field descriptions entity descriptions
    descriptions = list(),
    #'@field subjects entity subjects
    subjects = list(),
    #'@field formats entity formats
    formats = list(),
    #'@field contacts entity contacts
    contacts = list(),
    #'@field relations entity relations
    relations = list(),
    #'@field rights entity rights
    rights = list(),
    #'@field spatial_extent spatial extent
    spatial_extent = NULL,
    #'@field spatial_bbox spatial bounding box
    spatial_bbox = NULL,
    #'@field geo_bbox geographic bounding box (in EPSG:4326 / WGS 84)
    geo_bbox = NULL,
    #'@field srid entity srid
    srid = NULL,
    #'@field temporal_extent entity temporal extent
    temporal_extent = NULL,
    #'@field provenance entity provenance
    provenance = NULL,
    #'@field data entity data
    data = NULL,
    #'@field status entity status
    status = list(),
    #'@field resources entity resources
    resources = list(),
    #'@field locales entity locales
    locales = list(),
    
    #'@description Initializes an object of class \link{geoflow_entity}
    initialize = function(){
    },
    
    #'@description Retrieves keys allowed for a given tabular field name. eg. "Identifier"
    #'@param field field name
    #'@return the list of valid keys for the field considered
    getAllowedKeyValuesFor = function(field){
      clazz <- eval(parse(text = paste0("geoflow_validator_entity_",field)))
      clazz_obj <- clazz$new(0,0,"")
      return(clazz_obj$getValidKeys())
    },
    
    #'@description Adds locales to entity from kvp values
    #'@param values values
    addLocalesFromValues = function(values){
      locales <- unlist(lapply(names(attributes(values)), function(x){unlist(strsplit(x,"locale#"))[2]}))
      self$locales <- unique(c(self$locales, locales))
    },
    
    #'@description Set an identifier given a key. Default key is "id", but others can be specified, eg "doi".
    #'@param key identifier key. Default is "id"
    #'@param id identifier value
    setIdentifier = function(key = "id", id){
      if(!key %in% self$getAllowedKeyValuesFor("Identifier")){
        stop(sprintf("Identifier Key should be among the following allowed keys",
                     paste0(self$getAllowedKeyValuesFor("Identifier"), collapse=",")))
      }
      self$identifiers[[key]] <- id
    },
    
    #'@description Adds a date
    #'@param dateType date type, object of class \code{character}
    #'@param date date, object of class \code{Date} or \code{POSIXt}
    addDate = function(dateType, date){
      date_obj <- geoflow_date$new()
      date_obj$setKey(dateType)
      date_obj$setValue(date)
      self$dates[[length(self$dates)+1]] <- date_obj
    },
    
    #'@description Set the language used for the entity description (metadata). Default is "eng".
    #'@param language language
    setLanguage = function(language){
      self$language <- language
    },
    
    #'@description writes a data resource. Deprecrated Note: TODO to review in line with 'writeWorkflowJobDataResource
    #'@param obj object
    #'@param resourcename resource name
    #'@param type type of resosurce
    writeDataResource = function(obj=NULL, resourcename, type="shp"){
      if(is.null(obj)) obj=self$data$features
      resourcename_parts <- unlist(strsplit(resourcename, "\\.(?=[^\\.]+$)", perl=TRUE))
      if(length(resourcename_parts)>1) resourcename <- resourcename_parts[1]
      switch(type,
        "shp"={
         st_write(obj = obj, paste0("./data/",resourcename,".shp"), delete_layer = TRUE)
         zip::zipr(zipfile = paste0("./data/",resourcename, ".zip"), files = paste0(getwd(),"./data/",list.files(path="./data",pattern = resourcename)))
         df<-st_read(paste0("./data/",resourcename,".shp"), quiet=TRUE)
         self$data$features<-df
        }
      )
    },
    
    #'@description Set the type of description. By default a generic type (key = "generic") is defined to "dataset", and
    #'    will be used as default type for actions that perform metadata production / publication.
    #'@param key type key. Default is "generic"
    #'@param type type value
    setType = function(key = "generic", type){
      self$types[[key]] <- type
    },
    
    #'@description Sets title
    #'@param key title key. Default is "title"
    #'@param title title value
    setTitle = function(key = "title", title){
      if(!key %in% self$getAllowedKeyValuesFor("Title")){
        stop(sprintf("Title Key should be among the following allowed keys",
                     paste0(self$getAllowedKeyValuesFor("Title"), collapse=",")))
      }
      self$titles[[key]] <- title
      self$addLocalesFromValues(title)
    },
    
    #'@description Sets description
    #'@param key description key. Default is "abstract"
    #'@param description description value
    setDescription = function(key, description){
      if(!key %in% self$getAllowedKeyValuesFor("Description")){
        stop(sprintf("Description Key should be among the following allowed keys",
                     paste0(self$getAllowedKeyValuesFor("Description"), collapse=",")))
      }
      self$descriptions[[key]] <- description
    },
    
    #'@description Adds a subject
    #'@param subject object of class \link{geoflow_subject}
    addSubject = function(subject){
      if(!is(subject, "geoflow_subject")){
        stop("The argument should be an object of class 'geoflow_subject'")
      }
      self$subjects <- c(self$subjects, subject)
    },
    
    #'@description Adds a format
    #'@param format object of class \link{geoflow_format}
    addFormat = function(format){
      if(!is(format, "geoflow_format")){
        stop("The argument should be an object of class 'geoflow_format'")
      }
      self$formats <- c(self$formats, format)
    },
    
    #'@description Adds a contact
    #'@param contact object of class \link{geoflow_contact}
    addContact = function(contact){
      if(!is(contact, "geoflow_contact")){
        stop("The argument should be an object of class 'geoflow_contact'")
      }
      self$contacts <- c(self$contacts, contact)
    },
    
    #'@description Adds a relation
    #'@param relation object of class \link{geoflow_relation}
    addRelation = function(relation){
      if(!is(relation, "geoflow_relation")){
        stop("The argument should be an object of class 'geoflow_relation'")
      }
      self$relations <- c(self$relations, relation)
    },
    
    #'@description Adds a right
    #'@param right object of class \link{geoflow_right}
    addRight = function(right){
      if(!is(right, "geoflow_right")){
        stop("The argument should be an object of class 'geoflow_right'")
      }
      self$rights <- c(self$rights, right)
    },
    
    #'@description Set spatial extent. Various ways can be used to set the spatial extent 1) with a WKT string,
    #'    2) with a bbox, object of class \code{matrix}, or 3) specifying a data object (from \pkg{sf}).
    #'    The \code{crs} (coordinate reference system) should be specified with the crs SRID (number).
    #'    The spatial extent is not necessarily a bounding box but can be one or more geometries.
    #'@param wkt a WKT string
    #'@param bbox a bbox 
    #'@param data an object of class \pkg{sf} 
    #'@param crs crs
    setSpatialExtent = function(wkt = NULL, bbox = NULL, data = NULL, crs = NA){
      if(is.null(wkt) & is.null(bbox) & is.null(data)){
        stop("At least one of the arguments 'wkt' (WKT string) or 'bbox' should be provided!")
      }
      spatial_extent <- NULL
      if(!is.null(wkt)) spatial_extent <- sf::st_as_sfc(wkt, crs = crs)
      if(!is.null(bbox)) spatial_extent <- bbox
      if(!is.null(data)) {
        if(!is(data, "sf")) return(NULL)
        spatial_extent <- data
      }
        
      if(class(spatial_extent)[1]=="try-error"){
        stop("The spatial extent is invalid!")
      }
      self$spatial_extent <- spatial_extent
      self$setSpatialBbox(wkt, bbox, data, crs)
    },
    
    #'@description Set spatial bbox. Various ways can be used to set the spatial extent 1) with a WKT string,
    #'    2) with a bbox, object of class \code{matrix}, or 3) specifying a data object (from \pkg{sf}).
    #'    The \code{crs} (coordinate reference system) should be specified with the crs SRID (number).
    #'@param wkt a WKT string
    #'@param bbox a bbox 
    #'@param data an object of class \pkg{sf} 
    #'@param crs crs
    setSpatialBbox = function(wkt = NULL, bbox = NULL, data = NULL, crs = NA){
      if(is.null(wkt) & is.null(bbox) & is.null(data)){
        stop("At least one of the arguments 'wkt' (WKT string) or 'bbox' should be provided!")
      }
      spatial_bbox  <- NULL
      if(!is.null(wkt)) spatial_bbox <- sf::st_bbox(sf::st_as_sfc(wkt, crs = crs))
      if(!is.null(bbox)) spatial_bbox <- bbox
      if(!is.null(data)){
        if(is(data, "sf")){
          #vector
          spatial_bbox <- sf::st_bbox(data)
        }else if(is(data, "SpatRaster")){
          #grid
          vec = data@ptr$extent$vector
          spatial_bbox <- c(xmin = vec[1], ymin = vec[3], xmax = vec[2], ymax = vec[4])
          class(spatial_bbox) <- "bbox"
        }else{
          return(NULL)
        }
      }
      
      if(class(spatial_bbox)[1]=="try-error"){
        stop("The spatial bbox is invalid!")
      }
      
      self$spatial_bbox <- spatial_bbox
      self$setGeographicBbox()
    },
    
    #'@description Set geographic bbox (in EPGS:4326 / WGS 84), by converting (if needed) the spatial bbox
    setGeographicBbox = function(){
      #convert spatial_bbox in case srid != 4326 (WGS 84)
      if(is.null(self$spatial_bbox)) return(NULL)
      geo_bbox <- self$spatial_bbox
      if(!is.null(self$srid)) if(self$srid != 4326){
        #transform min coords
        sp_bbox_min <- sf::st_sf(sf::st_sfc(sf::st_point(c(self$spatial_bbox$xmin, self$spatial_bbox$ymin)), crs = self$srid))
        sp_bbox_min_new <- sf::st_transform(sp_bbox_min, crs = 4326)
        sp_bbox_min_coords <- sf::st_coordinates(sp_bbox_min_new[[1]])
        #transform max coords
        sp_bbox_max <- sf::st_sf(sf::st_sfc(sf::st_point(c(self$spatial_bbox$xmax, self$spatial_bbox$ymax)), crs = self$srid))
        sp_bbox_max_new <- sf::st_transform(sp_bbox_max, crs = 4326)
        sp_bbox_max_coords <- sf::st_coordinates(sp_bbox_max_new[[1]])
        #compound trasnformed bbox
        geo_bbox <- c(xmin = sp_bbox_min_coords[[1]], ymin = sp_bbox_min_coords[[2]], 
                          xmax = sp_bbox_max_coords[[1]], ymax = sp_bbox_max_coords[[2]])
        class(geo_bbox) <- "bbox"
      }
      
      self$geo_bbox <- geo_bbox
    },
    
    #'@description Sets entity SRID
    #'@param srid srid
    setSrid = function(srid){
      self$srid <- srid
    },
    
    #'@description Sets temporal extent. The temporal extent can be a year, date instant or interval
    #'@param str object of class \code{numeric} (case of year) or \code{character}
    setTemporalExtent = function(str){
      if(is.null(str)){
        self$temporal_extent = NULL
      }else{
        isInstant <- FALSE
        if(is(str,"character")){
          str <- unlist(strsplit(str,"/"))
          if(length(str)==1) isInstant <- TRUE
        }else if(is(str,"Date")||is(str,"POSIXct")){
          isInstant <- TRUE
        }else{
          isInstant <- TRUE
          class(str) <- "character"
        }
        if(isInstant){
          self$temporal_extent <- list(instant = str_to_posix(str))
        }else{
          self$temporal_extent <- list(
            start = str_to_posix(gsub(" ","T",str[1])),
            end = str_to_posix(gsub(" ","T",str[2]))
          )
        }
      }
    },
    
    #'@description Sets entity provenance
    #'@param provenance object of class \link{geoflow_provenance}
    setProvenance = function(provenance){
      if(!is(provenance, "geoflow_provenance")){
        stop("The provenance should be an object of class 'geoflow_provenance'")
      }
      self$provenance <- provenance
    },
    
    #'@description Sets entity data object
    #'@param data object of class \link{geoflow_data}
    setData = function(data){
      if(!is(data,"geoflow_data")){
        stop("Data should be an object of class 'geoflow_data'")
      }
      self$data <- data
    },
    
    #'@description Adds entity data object
    #'@param data object of class \link{geoflow_data}
    addData = function(data){
      if(!is(data,"geoflow_data")){
        stop("Data should be an object of class 'geoflow_data'")
      }
      self$data <- c(self$data, data)
    },
    
    #'@description Gets entity job directory name. In case entity is identified with a DOI, the '/' (slash) 
    #'will be replaced by '_' (underscore) to make sure directory is created.
    #'@return get the name of entity job directory that will be created for the entity
    getEntityJobDirname = function(){
      id <- self$identifiers[["id"]]
      id <- gsub("/","_", id)
      return(id)
    },
    
    #'@description Gets entity job directory path. In the job directory, all entities subdirs will be created within a 'entities' directory.
    #'@param config geoflow configuration object
    #'@param jobdir relative path of the job directory
    #'@return the entity job directory path
    getEntityJobDirPath = function(config, jobdir = NULL){
      if(is.null(jobdir)) jobdir <- config$job
      path <- file.path(jobdir, "entities", self$getEntityJobDirname())
      return(path)
    },
    
    #'@description Function called internally by \pkg{geoflow} that creates the entity directory and relevant sub-directories.
    #'    The default sub-directories will include 'data' and 'metadata'. Other sub-directories may be created depnding 
    #'    on the actions enabled in the workflow (and if their target directory is different from 'data'/'metadata').
    #'@param config geoflow config object
    #'@param jobdir relative path of the job directory
    prepareEntityJobDir = function(config, jobdir = NULL){
      if(is.null(jobdir)) jobdir <- config$job
      #create entity jobdir
      config$logger.info(sprintf("Create entity job dir at '%s'", self$getEntityJobDirPath(config, jobdir)))
      dir.create(self$getEntityJobDirPath(config, jobdir), recursive = TRUE)
      
      #create sub directories as listed in the configuration file
      entity_targets <- sapply(config$actions, function(x){if(!is.na(x$target)) if(x$target=="entity") return(x$target_dir)})
      entity_targets <- entity_targets[!sapply(entity_targets,is.null)]
      directories <- unique(c("data","metadata", unlist(entity_targets)))
      directories <- directories[!is.na(directories)]
      for(directory in directories){
        if (!file.exists(directory)){
          dir_name <- file.path(self$getEntityJobDirPath(config, jobdir), directory)
          config$logger.info(sprintf("Creating '%s' directory: %s",directory, dir_name))
          dir.create(dir_name)
        }
      }
    },
    
    #'@description This function will look at data object(s) associated to the entity (previously set with 
    #' \code{setData} or added with \code{addData}), and will try to (download)/copy the data source to the 
    #' geoflow job directory.
    #'@param config geoflow config object
    #'@param jobdir relative path of the job directory
    copyDataToJobDir = function(config, jobdir = NULL){
      
      if(is.null(jobdir)) jobdir <- config$job
      wd <- getwd()
      setwd("./data")
      
      #get accessors
      accessors <- list_data_accessors(raw = TRUE)
      
      data_objects <- list()
      if(is.null(self$data$dir)){
        data_objects <- list(self$data)
      }else{
        data_objects <- self$data$getData()
      }
      if(length(data_objects)>0) for(k in 1:length(data_objects)){
        
        data_object <- data_objects[[k]]
      
        accessor <- accessors[sapply(accessors, function(x){x$id == data_object$access})][[1]]
      
        config$logger.info(sprintf("Copying data to entity job data directory '%s'", getwd()))
        
        if(!data_object$sourceType %in% c("dbtable", "dbquery", "dbview")) for(i in 1:length(data_object$source)){
        
          datasource <- data_object$source[[i]]
          datasource_parts <- unlist(strsplit(datasource, "\\.(?=[^\\.]+$)", perl=TRUE))
          if(length(datasource_parts)<2)if(data_object$sourceType != "nc") stop("Source data file should include a file extension")
          datasource_name <- datasource_parts[1]
          datasource_ext <- datasource_parts[2]
          datasource_uri <- attr(datasource, "uri")
          attributes(datasource) <- NULL
          if(is.null(datasource_uri)) datasource_uri <- datasource
          
          #in case of a datasource type requiring a file we check its presence
          #if absent we abort the function enrich With features
          types_without_file <- c("dbtable","dbview","dbquery")
          datasource_file_needed <- !(data_object$sourceType %in% types_without_file)
          if(datasource_file_needed && is.null(datasource_uri)){
            warnMsg <- sprintf("No source file/URL for datasource '%s'. Data source copying aborted!", datasource_name)
            config$logger.warn(warnMsg)
          }else{
            config$logger.info(sprintf("Copying data source %s '%s' (%s) to entity job data directory '%s'",
                                       i, datasource, datasource_uri, getwd()))
            
            #basefilename <- paste0(self$identifiers$id, "_", self$data$sourceType,"_",datasource_name)
            basefilename <- datasource_name
          
            #here either we only pickup zipped files and re-distribute them in job data directory
            #or we write it from data_object$features if the latter is not NULL and if writer available (for now only shp)
            #The latter allows to push features filtered by cqlfilter (matching an eventual geoserver layer) to Zenodo
            #instead of the complete dataset
        
            #copy data
            isSourceUrl <- regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", datasource_uri) > 0
            if(isSourceUrl || accessor$id != "default"){
              #case where data is remote and there was no data enrichment in initWorkflow
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
              accessor$download(
                resource = datasource_uri,
                file = datasource, 
                path = file.path(getwd(), paste(basefilename, datasource_ext, sep=".")),
                software = accessor_software
              )
            }else{
              if(!is.null(datasource_uri)){
                if(!is_absolute_path(datasource_uri)) datasource_uri <- file.path(config$session_wd,datasource_uri)
              }
              config$logger.info("Copying data to Job data directory from local file(s)")
              data.files <- list.files(path = dirname(datasource_uri), pattern = basename(datasource_uri))
              if(length(data.files)>0){
                isZipped <- any(sapply(data.files, endsWith, ".zip"))
                if(!isZipped){
                  config$logger.info("Copying data local file(s): copying also unzipped files to job data directory")
                  for(data.file in data.files){
                    file.copy(from = file.path(dirname(datasource_uri), data.file), to = getwd())
                  }
                  config$logger.info("Copying data local file(s): zipping files as archive into job data directory")
                  data.files <- list.files(pattern = basefilename)
                  if(length(data.files)>0) zip::zipr(zipfile = paste0(basefilename,".zip"), files = data.files)
                }else{
                  config$logger.info("Copying data local file(s): copying unzipped files to job data directory")
                  data.files <- utils::unzip(zipfile = datasource_uri, unzip = getOption("unzip"))
                  if(length(data.files)>0) for(data.file in data.files){
                    file.copy(from = file.path(dirname(datasource_uri), data.file), to = getwd())
                  }
                  if(length(data.files)>0) zip::zipr(zipfile = file.path(getwd(), paste0(basefilename,".zip")), files = data.files)
                }
              }else{
                errMsg <- sprintf("Copying data local file(s): no files found for source '%s' (%s)", datasource_uri, datasource_name)
                config$logger.error(errMsg)
                stop(errMsg)
              }
            }
          }
        
          #rename unzipped files (generic behavior)
          #data.files <- list.files(path = getwd(), pattern = datasource_name)
          #data.files <- data.files[!endsWith(data.files, ".zip")]
          #if(length(data.files)>0){
          #  for(data.file in data.files){
          #    if(data.file %in% list.dirs(getwd(), recursive = F, full.names = F)) next
          #    fileparts <- unlist(strsplit(data.file,"\\.(?=[^\\.]+$)", perl=TRUE))
          #    fileext <- fileparts[length(fileparts)]
          #    file.rename(from = data.file, to = paste0(basefilename, ".", fileext))
          #  }
          #  unlink(paste0(basefilename, ".zip"))
          #  data.files <- list.files(pattern = basefilename)
          #  if(length(data.files)>0) zip::zipr(zipfile = paste0(basefilename,".zip"), files = data.files)
          #}
          #special case of shapefile (we keep the source naming)
          #if(data_object$sourceType == "shp"){
          #  data.files <- list.files(path = getwd(), pattern = datasource_name)
          #  data.files <- data.files[!endsWith(data.files, "zip")]
          #  if(length(data.files)>0) for(data.file in data.files){
          #    fileparts <- unlist(strsplit(data.file,"\\.(?=[^\\.]+$)", perl=TRUE))
          #    fileext <- fileparts[length(fileparts)]
          #    file.copy(from = file.path(dirname(data.file), data.file), to = file.path(getwd(), paste0(datasource_name, ".", fileext)))
          #  }
          #  data.files <- list.files(path = getwd(), pattern = paste0("^",datasource_name))
          #  if(length(data.files)>0) zip::zipr(zipfile = paste0(datasource_name,".zip"), files = data.files)
          #}
        }
      
        #special case of other types to zip all into a single file
        if(data_object$sourceType == "other" & data_object$sourceZip){
          config$logger.info("sourceZip = TRUE: Zip sources into single data file")
          data.files <- list.files()
          print(data.files)
          zip::zipr(zipfile = paste0(self$identifiers$id, "_files_for_source_", k, ".zip"), files = data.files)
          if(data_object$sourceZipOnly){
            config$logger.info("sourceZipOnly = TRUE: deleting zipped, they will not be uploaded")
            for(data.file in data.files){
              unlink(data.file, force = TRUE)
            }
          }else{
            config$logger.info("sourceZipOnly = FALSE: both zip and zipped files will be uploaded")
          }
        }else{
          config$logger.info("sourceZip = FALSE: source files will be uploaded")
        }
      }
      
      setwd("..")
      
    },
    
    #'@description This function will enrich the entity data objects with data features (vector data) or coverages (grid data). This method will overwrite 
    #' spatial metadata such as the bounding box (unless global option \code{skipDynamicBbox} is enabled). Note that the user spatial extent is not overwriten 
    #' since it may contain finer geometries than a bounding box.
    #'@param config geoflow config object
    #'@param jobdir relative path of the job directory
    enrichWithData = function(config, jobdir = NULL){
      
      if(is.null(jobdir)) jobdir <- config$job
      wd <- getwd()
      setwd("./data")
      
      skipDynamicBbox <- if(!is.null(config$profile$options$skipDynamicBbox)) config$profile$options$skipDynamicBbox else FALSE
      enrichDataStrategy <- if(!is.null(config$profile$options$enrichDataStrategy)) config$profile$options$enrichDataStrategy else "first"
      #TODO enrichDataSourceStrategy <- if(!is.null(config$profile$options$enrichDataSourceStrategy)) config$profile$options$enrichDataSourceStrategy else "first"
      
      data_objects <- list()
      if(is.null(self$data$dir)){
        data_objects <- list(self$data)
      }else{
        data_objects <- self$data$getData()
      }
      
      srid <- if(!is.null(self$srid)) self$srid else ""
      data_srids <- c()
      
      if(length(data_objects)>0){
        
        data_objects <- lapply(1:length(data_objects), function(k){
      
          data_object = data_objects[[k]]
        
          datasource <- data_object$source[[1]] #TODO we still look at first source
          datasource_name = NULL
          datasource_ext = NULL
          datasource_file = NULL
          if(!is.null(datasource)){
            datasource_parts <- unlist(strsplit(datasource, "\\.(?=[^\\.]+$)", perl=TRUE))
            datasource_name <- datasource_parts[1]
            datasource_ext <- datasource_parts[2]
            datasource_file <- attr(datasource, "uri")
            attributes(datasource) <- NULL
            if(is.null(datasource_file)) datasource_file <- datasource
          }
          
          if(data_object$sourceType == "other"){
            config$logger.warn("Metadata dynamic handling based on 'data' not implemented for source type 'other'")
            #setwd(wd)
            #return(NULL)
          }
        
          #in case of a datasource type requiring a file we check its presence
          #if absent we abort the function enrich With features
          types_without_file <- c("dbtable","dbview","dbquery")
          datasource_file_needed <- !(data_object$sourceType %in% types_without_file)
          if(datasource_file_needed && is.null(datasource_file)){
            warnMsg <- sprintf("No source file/URL for datasource '%s'. Data source copying aborted!", datasource_name)
            config$logger.warn(warnMsg)
            #setwd(wd)
            #return(NULL)
          }
          
          #basefilename
          basefilename <- datasource_name
        
          #inherit sourceType for source
          if(datasource_file_needed){
            data_object$sourceType = switch(datasource_ext,
              "zip" = {
                srcType = "other"
                zip_files = zip::zip_list(file.path(getwd(), paste0(basefilename,".zip")))
                if(any(endsWith(zip_files$filename, ".gpkg"))){
                  srcType = "gpkg" 
                }else if(any(endsWith(zip_files$filename, ".shp"))){
                  srcType = "shp"
                }else if(any(endsWith(zip_files$filename, ".csv"))){
                  srcType = "csv"
                }else if(any(endsWIth(zip_files$filename, ".tif"))){
                  srcType = "tif"
                }
                config$logger.info(sprintf("Resolving sourceType from zip list: '%s'", srcType))
                srcType
              },
              "shp" = "shp",
              "gpkg" = "gpkg",
              "csv" = "csv",
              "tif" = "tif",
              "other"
            )
            #additional rule for uploadType
            if(datasource_ext == "zip") if(!is.null(data_object$uploadType)) if(data_object$uploadType == "other"){
              data_object$uploadType = data_object$sourceType
            }
            #overwrite top sourceType
            if(is.null(self$data$dir)){
              self$data$sourceType = data_object$sourceType 
              self$data$uploadType = data_object$uploadType
            }else{
              self$data$data[[k]]$sourceType = data_object$sourceType
              self$data$data[[k]]$uploadType = data_object$uploadType
            }
          }
          
          #encoding mappings
          st_encoding <- switch(options("encoding")[[1]],
            "UTF-8" = "UTF-8",
            "latin1" = "WINDOWS-1252",
            "native.enc" = "WINDOWS-1252",
            "UTF-8"
          )
        
          switch(data_object$sourceType,
               
             #shp - ESRI Shapefile (if remote, shapefiles should be zipped)
             #---------------------------------------------------------------------------------
             "shp" = {
               trgShp <- file.path(getwd(), paste0(basefilename,".shp"))
               if(!file.exists(trgShp)){
                 shps <- list.files(path = getwd(), pattern = ".shp", full.names = T)
                 if(length(shps)==0){
                   warnMsg <- "No readable Shapefile source"
                   config$logger.warn(warnMsg)
                 }
                 trgShp <- shps[1]
               }
               if(file.exists(trgShp)){
                 #read shapefile
                 config$logger.info("Read Shapefiles from geoflow temporary data directory")
                 sf.data <- sf::st_read(trgShp, options = sprintf("ENCODING=%s",st_encoding))
                 if(!is.null(sf.data)){
                   #we try to apply the cql filter specified as data property
                   #TODO cqlfilter to dismiss in favour of a sourceFilter property
                   if(!is.null(data_object$cqlfilter)){
                     sf.data <- filter_sf_by_cqlfilter(sf.data, data_object$cqlfilter)
                   }
                   if(attr(sf.data, "sf_column")== "geometry"){
                     sf.data$the_geom <- st_geometry(sf.data)
                     attr(sf.data, "sf_column") <- "the_geom"
                     sf.data$geometry <- NULL
                   }
                   data_object$setFeatures(sf.data)
                   
                   #dynamic srid
                   if(is(sf.data, "sf")){
                     epsgcode = get_epsg_code(sf.data)
                     if(!is.na(epsgcode)){
                       data_srids <<- c(data_srids, epsgcode)
                     }
                     sf.crs = sf::st_crs(sf.data)
                     if(is.na(sf.crs)){
                       #in case data features are not geo-referenced we check availability of self$srid and apply it to data features
                       if(!is.null(self$srid)) sf::st_crs(data_object$features) <- self$srid 
                     }
                   }
  
                 }else{
                   warnMsg <- sprintf("Cannot read Shapefile data source '%s'. Dynamic metadata computation aborted!", trgShp)
                   config$logger.warn(warnMsg)
                 }
               }else{
                 warnMsg <- sprintf("No readable Shapefile source '%s'. Dynamic metadata computation aborted!", datasource_file)
                 config$logger.warn(warnMsg)
               }
             },
             #csv - CSV file - operated through 
             # * sf package / OGR CSV driver https://gdal.org/drivers/vector/csv.html) for geometry guess/fetch
             # * combined with readr for a proper guess of column definitions
             #---------------------------------------------------------------------------------
             "csv" = {
               trgCsv <- file.path(getwd(), paste0(basefilename,".csv"))
               if(file.exists(trgCsv)){
                 #read CSV
                 config$logger.info("Read CSV file from geoflow temporary data directory")
                 
                 sf.data <- sf::st_read(trgCsv, options = c(sprintf("GEOM_POSSIBLE_NAMES=%s", paste0(data_object$getAllowedGeomPossibleNames(),collapse=",")),
                                                            sprintf("X_POSSIBLE_NAMES=%s", paste0(data_object$getAllowedXPossibleNames(),collapse=",")),
                                                            sprintf("Y_POSSIBLE_NAMES=%s", paste0(data_object$getAllowedYPossibleNames(),collapse=","))))
                 if(!is.null(sf.data)){
                   tbl.spec <- readr::spec_csv(trgCsv)
                   tbl.spec[1]$cols = sapply(tbl.spec[1]$cols, function(x){spec = x;if(is(x, "collector_logical")){spec = readr::col_character()}; return(spec)})
                   tbl.data <- as.data.frame(readr::read_csv(trgCsv, col_types = tbl.spec))
                   if(is(sf.data,"sf")){
                     if(nrow(tbl.data)==nrow(sf.data)){
                       sf.data <- st_set_geometry(tbl.data, st_geometry(sf.data))
                     }else if(nrow(sf.data)==0){
                       if(any(colnames(tbl.data)%in% data_object$getAllowedGeomPossibleNames())){
                         geom_column_name = colnames(tbl.data)[colnames(tbl.data)%in% data_object$getAllowedGeomPossibleNames()][1]
                         sf.data <- sf::st_as_sf(tbl.data, wkt = geom_column_name)
                       }
                     }
                     if(!"geometry" %in% colnames(tbl.data)){
                       if("geom" %in% colnames(sf.data)) sf.data$geom <- NULL
                       colnames(sf.data)[colnames(sf.data)=="geometry"] <- "geom"
                       st_geometry(sf.data) <- "geom" #default in spatial DBIs if data imported through sf
                     }
                   }else{
                     sf.data <- tbl.data
                   }
                   
                   #we try to apply the cql filter specified as data property
                   #TODO cqlfilter to dismiss in favour of a sourceFilter property
                   if(!is.null(data_object$cqlfilter)){
                     sf.data <- filter_sf_by_cqlfilter(sf.data, data_object$cqlfilter)
                   }
                   data_object$setFeatures(sf.data)
                   
                   #dynamic srid
                   if(is(sf.data, "sf")){
                     epsgcode = get_epsg_code(sf.data)
                     if(!is.na(epsgcode)){
                        data_srids <<- c(data_srids, epsgcode)
                     }
                     sf.crs = sf::st_crs(sf.data)
                     if(is.na(sf.crs)){
                       #in case data features are not geo-referenced we check availability of self$srid and apply it to data features
                       if(!is.null(self$srid)) sf::st_crs(data_object$features) <- self$srid 
                     }
                   }
                   
                 }else{
                   warnMsg <- sprintf("Cannot read CSV data source '%s'. Dynamic metadata computation aborted!", trgCsv)
                   config$logger.warn(warnMsg)
                 }
               }else{
                 warnMsg <- sprintf("No readable CSV source '%s'. Dynamic metadata computation aborted!", datasource_file)
                 config$logger.warn(warnMsg)
               }
               
             },
             #gpkg - GeoPackage file - operated through sf package
             #---------------------------------------------------------------------------------
             "gpkg" = {
               trgGpkg <- file.path(getwd(), paste0(basefilename,".gpkg"))
               if(file.exists(trgGpkg)){
                 #read GeoPackage
                 config$logger.info("Read GPKG file from geoflow temporary data directory")
                 if(!is.null(data_object$sourceSql)){
                   sf.data <- sf::st_read(trgGpkg, query = data_object$sourceSql)
                 }else{
                   sf.data <- sf::st_read(trgGpkg)
                 }
                 
                 if(!is.null(sf.data)){
                   
                   #we try to apply the cql filter specified as data property
                   #TODO cqlfilter to dismiss in favour of a sourceFilter property
                   if(!is.null(data_object$cqlfilter)){
                     sf.data <- filter_sf_by_cqlfilter(sf.data, data_object$cqlfilter)
                   }
                   data_object$setFeatures(sf.data)
                   
                   #dynamic srid
                   if(is(sf.data, "sf")){
                     epsgcode = get_epsg_code(sf.data)
                     if(!is.na(epsgcode)){
                       data_srids <<- c(data_srids, epsgcode)
                     }
                     sf.crs = sf::st_crs(sf.data)
                     if(is.na(sf.crs)){
                       #in case data features are not geo-referenced we check availability of self$srid and apply it to data features
                       if(!is.null(self$srid)) sf::st_crs(data_object$features) <- self$srid 
                     }
                   }
                   
                 }else{
                   warnMsg <- sprintf("Cannot read GeoPackage data source '%s'. Dynamic metadata computation aborted!", trgGpkg)
                   config$logger.warn(warnMsg)
                 }
               }else{
                 warnMsg <- sprintf("No readable GeoPackage source '%s'. Dynamic metadata computation aborted!", datasource_file)
                 config$logger.warn(warnMsg)
               }
               
             },
             #dbtable - A DB table
             #---------------------------------------------------------------------------------
             "dbtable" = {
               DBI <- config$software$input$dbi
               if(!is.null(DBI)){
                 sf.data <- sf::st_read(DBI, datasource_name)
                 if(!is.null(sf.data)){
                   #we try to apply the cql filter specified as data property
                   #TODO cqlfilter to dismiss in favour of a sourceFilter property
                   if(!is.null(data_object$cqlfilter)){
                     sf.data <- filter_sf_by_cqlfilter(sf.data, data_object$cqlfilter)
                   }
                   data_object$setFeatures(sf.data)
                   
                   if(is(sf.data, "sf")){
                     #dynamic srid
                     epsgcode = get_epsg_code(sf.data)
                     if(!is.na(epsgcode)){
                       data_srids <<- c(data_srids, epsgcode)
                     }
                     sf.crs = sf::st_crs(sf.data)
                     if(is.na(sf.crs)){
                       #in case data features are not geo-referenced we check availability of self$srid and apply it to data features
                       if(!is.null(self$srid)) sf::st_crs(data_object$features) <- self$srid 
                     }
                     
                     #dynamic spatial extent
                     config$logger.info("Overwriting entity bounding box with DB spatial table bounding box")
                     if(!skipDynamicBbox) self$setSpatialBbox(data = sf.data)
                   }else{
                     warnMsg <- sprintf("DB table '%s' is not spatialized. Dynamic metadata computation aborted!", datasource_name)
                     config$logger.warn(warnMsg)
                   }
                   
                 }else{
                   warnMsg <- sprintf("Cannot get results from DB table '%s'. Dynamic metadata computation aborted!", datasource_name)
                   config$logger.warn(warnMsg)
                 }
                 
               }else{
                 warnMsg <- sprintf("No database configured to read DB table '%s'. Dynamic metadata computation aborted!", datasource_name)
                 config$logger.warn(warnMsg)
               }
             },
             #dbview
             #---------------------------------------------------------------------------------
             "dbview" = {
               DBI <- config$software$input$dbi
               if(!is.null(DBI)){
                 sf.data <- sf::st_read(DBI, datasource_name)
                 if(!is.null(sf.data)){
                   #we try to apply the cql filter specified as data property
                   #TODO cqlfilter to dismiss in favour of a sourceFilter property
                   if(!is.null(data_object$cqlfilter)){
                     sf.data <- filter_sf_by_cqlfilter(sf.data, data_object$cqlfilter)
                   }
                   data_object$setFeatures(sf.data)
                   if(is(sf.data, "sf")){
                     #dynamic srid
                     epsgcode = get_epsg_code(sf.data)
                     if(!is.na(epsgcode)){
                       data_srids <<- c(data_srids, epsgcode)
                     }
                     sf.crs = sf::st_crs(sf.data)
                     if(is.na(sf.crs)){
                       #in case data features are not geo-referenced we check availability of self$srid and apply it to data features
                       if(!is.null(self$srid)) sf::st_crs(data_object$features) <- self$srid 
                     }
                     
                     #dynamic spatial extent
                     config$logger.info("Overwriting entity bounding box with DB spatial view bounding box")
                     if(!skipDynamicBbox) self$setSpatialBbox(data = sf.data)
                   }else{
                     warnMsg <- sprintf("DB view '%s' is not spatialized. Dynamic metadata computation aborted!", datasource_name)
                     config$logger.warn(warnMsg)
                   }
                   
                 }else{
                   warnMsg <- sprintf("Cannot get results from DB view '%s'. Dynamic metadata computation aborted!", datasource_name)
                   config$logger.warn(warnMsg)
                 }
                 
               }else{
                 warnMsg <- sprintf("No database configured to read DB view '%s'. Dynamic metadata computation aborted!", datasource_name)
                 config$logger.warn(warnMsg)
               }
             },
             #dbquery
             #---------------------------------------------------------------------------------
             "dbquery" = {
               
                sqlfile <- file.path(getwd(), paste0(basefilename,".sql"))
                if(file.exists(sqlfile)){
                  config$logger.info(sprintf("Reading SQL query from file '%s'", sqlfile))
                  sql <- paste(readLines(sqlfile), collapse="")
                  config$logger.info(sql)
                  data_object$setSourceSql(sql)
                }else{
                  if(is.null(data_object$sourceSql)){
                    warnMsg <- sprintf("No SQL file provided as 'source' nor 'sourceSql' data property specified for datasource '%s'. Dynamic metadata computation aborted!", datasource_name)
                    config$logger.warn(warnMsg)
                    setwd(wd)
                    return(FALSE)
                  }
                }
                
                DBI <- config$software$input$dbi
                if(!is.null(DBI)){
                  sf.data <- try(sf::st_read(DBI, query = data_object$sourceSql))
                  if(!is.null(sf.data)){
                    if(class(sf.data)[1]=="try-error"){
                      errMsg <- sprintf("Error while executing SQL query [%s]. Please check the SQL query! Dynamic data handling aborted!", data_object$sourceSql)
                      config$logger.error(errMsg)
                      setwd(wd)
                      return(FALSE)
                    }
                    #we try to apply the cql filter specified as data property
                    #TODO cqlfilter to dismiss in favour of a sourceFilter property
                    if(!is.null(data_object$cqlfilter)){
                      sf.data <- filter_sf_by_cqlfilter(sf.data, data_object$cqlfilter)
                    }
                    data_object$setFeatures(sf.data)
                    if(is(sf.data, "sf")){
                      #dynamic srid
                      epsgcode = get_epsg_code(sf.data)
                      if(!is.na(epsgcode)){
                        data_srids <<- c(data_srids, epsgcode)
                      }
                      
                      #dynamic spatial extent
                      config$logger.info("Overwriting entity bounding box with SQL query output bounding box")
                      if(!skipDynamicBbox) self$setSpatialBbox(data = sf.data)
                      #dynamic view properties required
                      
                      geomtype <- as.character(unique(sf::st_geometry_type(sf.data))[1])
                      gsGeomType <- switch(geomtype,
                        "GEOMETRY" = "Geometry", "GEOMETRYCOLLECTION" = "GeometryCollection",
                        "POINT" = "Point", "MULTIPOINT" = "MultiPoint", 
                        "LINESTRING" = "LineString", "MULTILINESTRING" = "MultiLineString",
                        "POLYGON" = "Polygon", "MULTIPOLYGON" = "MultiPolygon"
                      )
                      config$logger.info(sprintf("Setting entity geometry type '%s'", gsGeomType))
                      data_object$setGeometryType(gsGeomType)
                      geomField <- colnames(sf.data)[sapply(colnames(sf.data), function(x){(is(sf.data[[x]],"sfc"))})][1]
                      config$logger.info(sprintf("Setting entity geometry field '%s'",geomField))
                      data_object$setGeometryField(geomField)
                    }else{
                      warnMsg <- sprintf("Result of SQL query file '%s' is not spatialized. Dynamic metadata computation aborted!", datasource_file)
                      config$logger.warn(warnMsg)
                    }
                    
                  }else{
                    warnMsg <- sprintf("Cannot get results from SQL query file '%s'. Dynamic metadata computation aborted!", datasource_file)
                    config$logger.warn(warnMsg)
                  }
                  
                }else{
                  warnMsg <- sprintf("No database configured to execute SQL query file '%s'. Dynamic metadata computation aborted!", datasource_file)
                  config$logger.warn(warnMsg)
                }
                
              },
              #geotiff - GeoTIFF
              #---------------------------------------------------------------------------------
              "geotiff" = {
               trgGeotiff <- file.path(getwd(), paste0(basefilename,".tif"))
               if(file.exists(trgGeotiff)){
                 #read GeoTIFF
                 config$logger.info(sprintf("Read GeoTIFF '%s'", trgGeotiff))
                 cov.data <- terra::rast(trgGeotiff)
                 if(!is.null(cov.data)){
                   data_object$setCoverages(cov.data)
                   
                   #dynamic srid
                   cov.crs <- terra::crs(cov.data@ptr$get_crs("wkt"), describe = TRUE)
                   if(!is.null(cov.crs$code)) if(!is.na(cov.crs$code)){
                     data_srids <<- c(data_srids, as.integer(cov.crs$code))
                   }
                   #dynamic spatial extent
                   config$logger.info("Overwriting entity bounding box with Geotiff bounding box")
                   if(!skipDynamicBbox) self$setSpatialBbox(data = cov.data)
                   
                 }else{
                   warnMsg <- sprintf("Cannot read GeoTIFF data source '%s'. Dynamic metadata computation aborted!", trgGeotiff)
                   config$logger.warn(warnMsg)
                 }
               }else{
                 warnMsg <- sprintf("No readable GeoTIFF source '%s'. Dynamic metadata computation aborted!", datasource_file)
                 config$logger.warn(warnMsg)
               }
              },
              #other format handlers to come
              {
                config$logger.warn(sprintf("Metadata dynamic handling based on 'data' not implemented for type '%s'", data_object$sourceType))
              }
          )
          
          return(data_object)
          
        })
        
        if(is.null(self$data$dir)){
          self$data <- data_objects[[1]]
        }else{
          self$data$data <- data_objects
        }
      }
      
      if(length(data_srids)>0){
        unique_data_srids <- unique(data_srids)
        if(length(unique_data_srids)==1){
          if(srid != unique_data_srids[1]){
            config$logger.info(sprintf("Overwriting entity srid [%s] with data srid [%s]", srid, unique_data_srids[1]))
            self$srid <- unique_data_srids[1]
          }
          if(!skipDynamicBbox){
            #dynamic spatial extent
            config$logger.info(sprintf("Overwriting entity bounding box with data bounding boxes, using '%s' strategy", enrichDataStrategy))
            switch(enrichDataStrategy,
              "first" = {
                data_obj <- NULL
                if(!is.null(data_objects[[1]]$features)){
                  data_obj <- data_objects[[1]]$features
                }else if(!is.null(data_objects[[1]]$coverages)){
                  data_obj <- data_objects[[1]]$coverages
                }
                if(!is.null(data_obj)) self$setSpatialBbox(data = data_obj)
              },
              "union" = {
                self$setSpatialBbox(bbox = get_union_bbox(data_objects))
              }
            )
          }
        }else{
          config$logger.warn(sprintf("Data objects with mixed SRIDs [%s], aborting overwrite of entity srid [%s] and spatial bounding box", 
                                     paste0(unique_data_srids, collapse=","), srid))
        }
      }
      
      setwd(self$getEntityJobDirPath(config, jobdir))
      
    },
    
    #'@description This function will enrich the entity data objects with data features (vector data). This method will overwrite 
    #' spatial metadata such as the bounding box (unless global option \code{skipDynamicBbox} is enabled). Note that the user spatial extent is not overwriten 
    #' since it may contain finer geometries than a bounding box.
    #'@param config geoflow config object
    #'@param jobdir relative path of the job directory
    enrichWithFeatures = function(config, jobdir = NULL){
      config$logger.warn("Method 'enrichWithFeatures' is deprecated, please use 'enrichWithData' (that encompasses both 'features' and 'coverages') instead")
      self$enrichWithData(config, jobdir = jobdir)
    },
    
    #'@description This function will enrich the entity data objects with data coverages (grid data). This method will overwrite 
    #' spatial metadata such as the bounding box (unless global option \code{skipDynamicBbox} is enabled). Note that the user spatial extent is not overwriten 
    #' since it may contain finer geometries than a bounding box.
    #'@param config geoflow config object
    #'@param jobdir relative path of the job directory
    enrichWithCoverages = function(config, jobdir = NULL){
      config$logger.warn("Method 'enrichWithCoverages' is deprecated, please use 'enrichWithData' (that encompasses both 'features' and 'coverages') instead")
      self$enrichWithData(config, jobdir = jobdir)
    },
    
    #'@description This function will 1) check (in case of upload is requested) if the type of source and upload are both different 
    #'on files formats(eg. csv,shp,gpkg) and 2) process automatically to conversion from source to upload type.
    #'@param config geoflow config object
    prepareFeaturesToUpload = function(config) {
      types_with_file<-c("csv","shp","gpkg")
      
      data_objects <- list()
      if(is.null(self$data$dir)){
        data_objects <- list(self$data)
      }else{
        data_objects <- self$data$getData()
      }
      
      if(length(data_objects)>0){
        data_objects <- lapply(1:length(data_objects), function(k){
          
          data_object = data_objects[[k]]
        
          if(data_object$upload) if(data_object$sourceType %in% types_with_file & data_object$uploadType %in% types_with_file){
            
            if(data_object$sourceType != data_object$uploadType){
              
              config$logger.info(sprintf("Conversion of source file from sourceType (%s) to uploadType (%s)",data_object$sourceType,data_object$uploadType))
              
              datasource <- data_object$source[[1]]
              datasource_parts <- unlist(strsplit(datasource, "\\.(?=[^\\.]+$)", perl=TRUE))
              if(length(datasource_parts)<2) stop("Source data file should include a file extension")
              datasource_name <- datasource_parts[1]
              datasource_ext <- datasource_parts[2]
              
              uploadSourceExt<-switch(data_object$uploadType,
                                      "shp" = "zip",
                                      "gpkg" = "zip",
                                      data_object$uploadType
                                      
              )
              writeWorkflowJobDataResource(entity=self,config=config,type=data_object$uploadType,useFeatures=TRUE,resourcename=datasource_name)
              data_object$uploadSource<-list(paste0(datasource_name,".",uploadSourceExt))
              
            }else{
              config$logger.info("sourceType and uploadType are identical, no conversion required")		
            }
          }
          return(data_object)
        })
        
        if(is.null(self$data$dir)){
          self$data <- data_objects[[1]]
        }else{
          self$data$data <- data_objects
        }
      }
      
    },
    
    #'@description Function that will enrich entity with identifiers needed across multiple actions
    #'@param config geoflow config object
    enrichWithIdentifiers = function(config){
      geometa_action <- NULL
      geonode4R_action <- NULL
      actions <- list()
      
      #on geometa, set uuid in case geometa is run with option use_uuid is enabled / and no uuid already set
      if(length(config$actions)>0) actions <- config$actions[sapply(config$actions, function(x){regexpr("geometa-create-iso-19115",x$id)>0})]
      if(length(actions)>0) geometa_action <- actions[[1]]
      if(!is.null(geometa_action)) if(geometa_action$getOption("use_uuid")) if(is.null(self$identifiers[["uuid"]])){
        self$identifiers[["uuid"]] <- uuid::UUIDgenerate()
      }
      
      #on geonode4R, set uuid in case action is run / and no uuid already set
      if(length(config$actions)>0) actions <- config$actions[sapply(config$actions, function(x){regexpr("geonode4R-publish-ogc-services",x$id)>0})]
      if(length(actions)>0) geonode4R_action <- actions[[1]]
      if(!is.null(geonode4R_action)) if(is.null(self$identifiers[["uuid"]])){
        self$identifiers[["uuid"]] <- uuid::UUIDgenerate()
      }
    },
    
    #'@description This function that will enrich the entity with relations. At now this is essentially related to adding 
    #'    relations if a Geoserver (geosapi) publishing action is enabled. Relations added will depend on the 
    #'    \code{enrich_with_relation_*} options set in the geosapi action, ie. 
    #'    1) add WMS auto-generated thumbnail (if option \code{enrich_with_relation_wms_thumbnail} is \code{TRUE})
    #'    2) add WMS base URL relation (if option \code{enrich_with_relation_wms} is \code{TRUE})
    #'    3) for vector spatial representation type:
    #'      - add WFS base URL relation (if option \code{enrich_with_relation_wfs} is \code{TRUE})
    #'      - add WFS auto-generated links as convenience for data download links (if option \code{enrich_with_relation_wfs_download_links} is \code{TRUE})
    #'    4) for grid spatial representation type:
    #'      - add WCS base URL relation (if option \code{enrich_with_relation_wcs} is \code{TRUE})
    #'@param config geoflow config object
    enrichWithRelations = function(config){
      
      geosapi_action <- NULL
      actions <- list()
      if(length(config$actions)>0) actions <- config$actions[sapply(config$actions, function(x){regexpr("geosapi",x$id)>0})]
      if(length(actions)>0) geosapi_action <- actions[[1]]
      #dynamic relations related to OGC services (only executed if geosapi action is handled and enabled in workflow)
      if(!is.null(geosapi_action)) if(geosapi_action$getOption("enrich_with_relations")) if(!is.null(self$data)){
        
        data_objects <- list()
        if(is.null(self$data$dir)){
          data_objects <- list(self$data)
        }else{
          data_objects <- self$data$getData()
        }
        
        if(length(data_objects)>0) for(k in 1:length(data_objects)){
          
          data_object = data_objects[[k]]
        
          layername <- if(!is.null(data_object$layername)) data_object$layername else self$identifiers$id
          config$logger.info(sprintf("Enrich entity with OGC relations for layer = '%s'", layername))
            
          geoserver_base_url = config$software$output$geoserver_config$parameters$url
          if(!is.null(config$software$output$geoserver_config$properties$publicUrl)){
            geoserver_base_url = config$software$output$geoserver_config$properties$publicUrl
          }
          
          #Thumbnail
          if(geosapi_action$getOption("enrich_with_relation_wms_thumbnail")){
            config$logger.info(sprintf("Enrich entity with OGC WMS thumbnail for layer = '%s'", layername))
            new_thumbnail <- geoflow_relation$new()
            new_thumbnail$setKey("thumbnail")
            new_thumbnail$setName(layername)
            new_thumbnail$setDescription(sprintf("%s - Map overview", layername))
            thumbnail_link_template = geosapi_action$getOption("map_thumbnail_template")
            thumbnail_link_template = gsub("\\{","\\{{", thumbnail_link_template)
            thumbnail_link_template = gsub("\\}","\\}}", thumbnail_link_template)
            thumbnail_link <- whisker::whisker.render(
              thumbnail_link_template,
              list(
                geoserver_url = geoserver_base_url,
                workspace = config$software$output$geoserver_config$properties$workspace,
                layer = layername,
                bbox = paste(self$spatial_bbox,collapse=","),
                srid = self$srid
              )
            )
            new_thumbnail$setLink(thumbnail_link)
            self$relations <- c(self$relations, new_thumbnail)
          }else{
            config$logger.warn(sprintf("Skip enriching entity with OGC WMS thumbnail for layer = '%s'", layername))
          }
          #WMS base URL
          if(geosapi_action$getOption("enrich_with_relation_wms")){
            config$logger.info(sprintf("Enrich entity with OGC WMS base URL for layer = '%s'", layername))
            new_wms <- geoflow_relation$new()
            new_wms$setKey("wms")
            new_wms$setName(layername)
            new_wms$setDescription(sprintf("%s - Map access - OGC Web Map Service (WMS)",layername))
            new_wms$setLink(sprintf("%s/%s/ows?service=WMS", 
                                    geoserver_base_url, 
                                    config$software$output$geoserver_config$properties$workspace))
            self$addRelation(new_wms)
          }else{
            config$logger.warn(sprintf("Skip enriching entity with OGC WMS base URL for layer = '%s'", layername))
          }
          
          #OGC WFS relations in case of spatialRepresentationType = 'vector'
          if(data_object$spatialRepresentationType == "vector"){
            #WFS base URL
            if(geosapi_action$getOption("enrich_with_relation_wfs")){
              config$logger.info(sprintf("Enrich entity with OGC WFS base URL for layer = '%s'", layername))
              new_wfs <- geoflow_relation$new()
              new_wfs$setKey("wfs")
              new_wfs$setName(layername)
              new_wfs$setDescription(sprintf("%s - Data (features) access - OGC Web Feature Service (WFS)", layername))
              new_wfs$setLink(sprintf("%s/%s/ows?service=WFS", 
                                      geoserver_base_url, 
                                      config$software$output$geoserver_config$properties$workspace))
              self$addRelation(new_wfs)
            }else{
              config$logger.warn(sprintf("Skip enriching entity with OGC WFS base URL for layer = '%s'", layername))
            }
            #WFS download links
            if(geosapi_action$getOption("enrich_with_relation_wfs_download_links")){
              config$logger.info(sprintf("Enrich entity with OGC WFS download links for layer = '%s'", layername))
              #wfs (GML)
              new_wfs_gml <- geoflow_relation$new()
              new_wfs_gml$setKey("download")
              new_wfs_gml$setName(layername)
              new_wfs_gml$setDescription(sprintf("%s - Data download - OGC Web Feature Service (WFS) - GML format", layername))
              new_wfs_gml$setLink(sprintf("%s/%s/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=%s", 
                                          geoserver_base_url, 
                                          config$software$output$geoserver_config$properties$workspace,
                                          layername))
              new_wfs_gml$setMimeType("text/xml; subtype=gml/2.1.2")
              self$addRelation(new_wfs_gml)
              #wfs (GeoJSON)
              new_wfs_geojson <- geoflow_relation$new()
              new_wfs_geojson$setKey("download")
              new_wfs_geojson$setName(layername)
              new_wfs_geojson$setDescription(sprintf("%s - Data download - OGC Web Feature Service (WFS) - GeoJSON format", layername))
              new_wfs_geojson$setLink(sprintf("%s/%s/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=%s&outputFormat=json", 
                                              geoserver_base_url, 
                                              config$software$output$geoserver_config$properties$workspace,
                                              layername))
              new_wfs_geojson$setMimeType("application/json;charset=UTF-8")
              self$addRelation(new_wfs_geojson)
              #wfs (ESRI Shapefile)
              new_wfs_shp <- geoflow_relation$new()
              new_wfs_shp$setKey("download")
              new_wfs_shp$setName(layername)
              new_wfs_shp$setDescription(sprintf("%s - Data download - OGC Web Feature Service (WFS) - ESRI Shapefile format", layername))
              new_wfs_shp$setLink(sprintf("%s/%s/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=%s&outputFormat=SHAPE-ZIP", 
                                          geoserver_base_url, 
                                          config$software$output$geoserver_config$properties$workspace,
                                          layername))
              new_wfs_shp$setMimeType("application/zip")
              self$addRelation(new_wfs_shp)
              #CSV
              new_wfs_csv <- geoflow_relation$new()
              new_wfs_csv$setKey("download")
              new_wfs_csv$setName(layername)
              new_wfs_csv$setDescription(sprintf("%s - Data download - OGC Web Feature Service (WFS) - CSV format", layername))
              new_wfs_csv$setLink(sprintf("%s/%s/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=%s&outputFormat=CSV", 
                                          geoserver_base_url, 
                                          config$software$output$geoserver_config$properties$workspace,
                                          layername))
              new_wfs_csv$setMimeType("text/csv;charset=UTF-8")
              self$addRelation(new_wfs_csv)
            }else{
              config$logger.warn("Skip enriching entity with OGC WFS download links")
            }
          }
          #OGC WCS relations in case of spatialRepresentationType = 'grid'
          if(data_object$spatialRepresentationType == 'grid'){
            #WCS base URL
            if(geosapi_action$getOption("enrich_with_relation_wcs")){
              config$logger.info(sprintf("Enrich entity with OGC WCS base URL for layer = '%s'", layername))
              new_wcs <- geoflow_relation$new()
              new_wcs$setKey("wcs")
              new_wcs$setName(layername)
              new_wcs$setDescription(sprintf("%s - Data (Coverage) access - OGC Web Coverage Service (WCS)", layername))
              new_wcs$setLink(sprintf("%s/%s/ows?service=WCS", 
                                      geoserver_base_url, 
                                      config$software$output$geoserver_config$properties$workspace))
              self$addRelation(new_wcs)
            }else{
              config$logger.warn(sprintf("Skip enriching entity with OGC WCS base URL for layer = '%s'", layername))
            }
            
            #WCS download links
            if(geosapi_action$getOption("enrich_with_relation_wcs_download_links")){
              config$logger.info(sprintf("Enrich entity with OGC WCS download links for layer = '%s'", layername))
              #wcs (image/geotiff)
              new_wcs_geotiff <- geoflow_relation$new()
              new_wcs_geotiff$setKey("download")
              new_wcs_geotiff$setName(layername)
              new_wcs_geotiff$setDescription(sprintf("%s - Data download - OGC Web Coverage Service (WCS) - GeoTIFF format", layername))
              new_wcs_geotiff$setLink(sprintf("%s/%s/ows?service=WCS&request=GetCoverage&version=2.0.1&CoverageId=%s&format=image/geotiff", 
                                          geoserver_base_url, 
                                          config$software$output$geoserver_config$properties$workspace,
                                          layername))
              new_wcs_geotiff$setMimeType("image/tiff")
              self$addRelation(new_wcs_geotiff)
            }else{
              config$logger.warn(sprintf("Skip enriching entity with OGC WCS download links for layer = '%s'", layername))
            }
          }
        }
      }
    },
    
    #'@description Enrichs the entity with subjects. If no subject specify in Subjects, 
    #'automatically add keyword from dictionary to 'theme' category
    #'@param config geoflow config object
    enrichWithSubjects = function(config){
      
      if(length(self$subjects)==0){
        
        data_objects <- self$data
        if(is(data_objects, "geoflow_data")) data_objects <- list(self$data)
        
        if(length(data_objects)>0) for(k in 1:length(data_objects)){
          
          data_object = data_objects[[k]]
        
          #List all columns of data features
          columns <- colnames(data_object$features)
          for(featureAttrName in columns){
            #Check if correspond column exist in dictionary
            fat_attr <- NULL
            fto <- data_object$featureTypeObj
            if(!is.null(fto)) fat_attr <- fto$getMemberById(featureAttrName)
            if(!is.null(fat_attr)){
              #Check if register is link
              registerId <- fat_attr$registerId
              
              if(!is.null(registerId)) if(!is.na(registerId)){
                registers <- config$registers
                if(length(registers)>0) {
                  registers <- registers[sapply(registers, function(x){x$id == registerId})]
                  fat_attr_register <- registers[[1]]
                  
                  #Check if values of column are in register
                  dataAttrValues <- unique(data_object$features[featureAttrName])
                  featureAttrValues <- switch(class(data_object$features)[1],
                                              "sf" = data_object$features[,featureAttrName][[1]],
                                              "data.frame" = data_object$features[,featureAttrName]
                  )
                  featureAttrValues <- unique(featureAttrValues)
                  matchAttrValues <- subset(fat_attr_register$data, code %in% featureAttrValues)
                  
                  if (nrow(matchAttrValues)>0){
                    #Extract label[code] of this values
                    for(i in 1:nrow(matchAttrValues)){
                      matchAttrValues$keyword[i]<-paste0("\'",matchAttrValues$label[i],"[",matchAttrValues$code[i],"]\'",if(!is.na(matchAttrValues$uri[i])){paste0("@",matchAttrValues$uri[i])}else{""})
                    }
                    keywords<-unique(matchAttrValues$keyword)
                    
                    defSource <- fat_attr$defSource
                    if(is.na(defSource)){desc_name<-paste0("[",fat_attr$name,"]")}else{
                      desc_name<-paste0("[",defSource[1],"]")
                      if(!is.null(attr(defSource,"description"))) desc_name<-paste0("[",attr(defSource,"description"),"]")
                      if(!is.null(attr(defSource,"uri"))) desc_name<-paste0(desc_name,"@",attr(defSource,"uri"))
                    }
                    subject <- paste0("theme",desc_name,":",paste0(keywords,collapse=","))
                    subject_obj <- geoflow_subject$new(str = subject)
                    self$addSubject(subject_obj)  
                    
                  }
                }
              }
            }
          }
        }
      }
    },
    
    #'@description Enrichs the entity with formats
    #'@param config geoflow config object
    enrichWithFormats = function(config){
      
      formats<-sapply(self$formats, function(x) x$key)
      
      data_objects <- self$data
      if(is(data_objects, "geoflow_data")) data_objects <- list(self$data)
      
      if(length(data_objects)>0) for(k in 1:length(data_objects)){
        
        data_object = data_objects[[k]]
      
        if(!"resource" %in% formats)if(!is.null(data_object$sourceType)){
          if(!data_object$sourceType%in%c("dbtable","dbquery","dbview")){
            format <- paste0("resource:",mime::guess_type(paste0(".",data_object$sourceType)))
            format_obj <- geoflow_format$new(str = format)
            self$addFormat(format_obj)  
          }
        }
        
        if(!"distribution" %in% formats)if(!is.null(data_object$uploadType)){
          if(!data_object$uploadType%in%c("dbtable","dbquery","dbview")){
            format <- paste0("distribution:",mime::guess_type(paste0(".",data_object$uploadType)))
            format_obj <- geoflow_format$new(str = format)
            self$addFormat(format_obj)  
          }
        }
      }
        
    },
    
    #'@description Enrichs the entity properties with entity metadata from other properties.
    #'@param config geoflow config object
    enrichWithMetadata = function(config){
      
      #enrich titles
      self$titles <- lapply(self$titles, enrich_text_from_entity, self)
  
      #enrich descriptions
      self$descriptions <- lapply(self$descriptions, enrich_text_from_entity, self)
      
      #enrich relations
      self$relations <- lapply(self$relations, function(relation){
        relation$name <- enrich_text_from_entity(relation$name, self)
        relation$description <- enrich_text_from_entity(relation$description, self)
        return(relation)
      })
      
      #enrich provenance
      self$provenance$statement <- enrich_text_from_entity(self$provenance$statement, self)
      self$provenance$processes <- lapply(self$provenance$processes, function(process){
        process$rationale <- enrich_text_from_entity(process$rationale, self)
        process$description <- enrich_text_from_entity(process$description, self)
        return(process)
      })
    },
    
    #'@description Get the entity contacts
    #'@param pretty to prettify the output as \code{data.frame}
    #'@return a list of \code{geoflow_contact} or a \code{data.frame}
    getContacts = function(pretty = FALSE){
      if(pretty){
        out <- do.call("rbind.fill", lapply(self$contacts, function(contact){
          contact.df <- data.frame(
            id = contact$id,
            stringsAsFactors = FALSE
          )
          contact.df[,"individualName"] <- contact$individualName
          contact.df[,"organizationName"] <- contact$organizationName
          contact.df[,"positionName"] <- contact$positionName
          contact.df[,"role"] <- contact$role
          contact.df[,"voice"] <- contact$voice
          contact.df[,"facsimile"] <- contact$facsimile
          contact.df[,"email"] <- contact$email
          contact.df[,"websiteUrl"] <- contact$websiteUrl
          contact.df[,"websiteName"] <- contact$websiteName
          contact.df[,"postalAddress"] <- contact$postalAddress
          contact.df[,"postalCode"] <- contact$postalCode
          contact.df[,"city"] <- contact$city
          contact.df[,"country"] <- contact$country
          for(identifierType in names(contact$identifiers)){
            contact.df[,identifierType] <- contact$identifiers[[identifierType]]
          }
          
          return(contact.df)
        }))
        return(out)
      }else{
        return(self$contacts)
      }
    },
    
    #'@description Get the entity subjects
    #'@param pretty to prettify the output as \code{data.frame}
    #'@param keywords to add keywords to the output or not. Default is \code{FALSE}
    #'@return a list of \code{geoflow_subject} or a \code{data.frame}
    getSubjects = function(pretty = FALSE, keywords = FALSE){
      if(pretty){
        out <- do.call("rbind.fill", lapply(self$subjects, function(subject){
          subject.df <- data.frame(
            subject_name = subject$name,
            subject_uri = ifelse(is.null(subject$uri),NA,subject$uri),
            stringsAsFactors = FALSE
          )
          if(length(subject$dates)>0){
            subject.df <- cbind(subject.df, as.data.frame(subject$dates, stringsAsFactors=F))
          }
          if(keywords){
            kwd.df <- subject$getKeywords(pretty = TRUE)
            if(nrow(kwd.df)>0){
              subject.dfs <- do.call("rbind", lapply(1:nrow(kwd.df), function(i){ subject.df }))
              subject.df <- cbind(subject.dfs, kwd.df, stringsAsFactors = FALSE)
            }
          }
          return(subject.df)
        }))
        return(out)
      }else{
        return(self$subjects)
      }
    },
    
    #'@description Get the entity relations
    #'@param pretty to prettify the output as \code{data.frame}
    #'@return a list of \code{geoflow_relation} or a \code{data.frame}
    getRelations = function(pretty = FALSE){
      if(pretty){
        out <- do.call("rbind.fill", lapply(self$relations, function(relation){
          relation.df <- data.frame(
            key = relation$key,
            stringsAsFactors = FALSE
          )
          relation.df$name = relation$name
          relation.df$description = relation$description
          relation.df$link = relation$link
          return(relation.df)
        }))
        return(out)
      }else{
        return(self$relations)
      }
    },
    
    #'@description Set a simple status either "draft" or "published". This method is required to deal with 
    #'    systems that manage DOIs, such as Zenodo (with \pkg{zen4R}) or Dataverse (with \pkg{atom4R})
    #'    publishing actions (Used internally by \pkg{geoflow}).
    #'@param system a system name eg. "zenodo", "dataverse"
    #'@param status a status for entity resource "draft" or "published"
    setStatus = function(system, status){
      if(!(status %in% c("draft", "published"))){
        stop("The status should be either 'draft' or 'published'")
      }
      self$status[[system]] <- status
    },
    
    #'@description Get an entity job resource path
    #'@param config a geoflow config object
    #'@param resourceType type of resource, matching a sub-directory within the entity job directory
    #'@param filename filename
    #'@return the file path of the job resource
    getJobResource = function(config, resourceType, filename){
      return(file.path(config$job, "entities", self$getEntityJobDirname(), resourceType, filename))
    },
    
    #'@description Get an entity job data resource path
    #'@param config a geoflow config object
    #'@param filename filename
    #'@return the file path of the job data resource
    getJobDataResource = function(config, filename){
      self$getJobResource(config, "data", filename)
    },
    
    #'@description Get an entity job metadata resource path
    #'@param config a geoflow config object
    #'@param filename filename
    #'@return the file path of the job metadata resource
    getJobMetadataResource = function(config, filename){
      self$getJobResource(config, "metadata", filename)
    },
    
    #'@description Adds a resource to the entity
    #'@param id id of the resource
    #'@param resource resource
    addResource = function(id, resource){
      self$resources[[id]] <- resource
    },
    
    #'@description Methods to export the \link{geoflow_entity} as \code{data.frame} using key-based syntax.
    #'@param line_separator a line separator. By default, the default line separator will be used.
    #'@return an object of class \code{data.frame} giving the entities using key-based syntax
    asDataFrame = function(line_separator = NULL){
      if(is.null(line_separator)) line_separator <- get_line_separator()
      out <- data.frame(
        #Identifier
        Identifier = paste0(sapply(names(self$identifiers),function(name){
          outid <- paste(name, self$identifiers[[name]],sep=":")
          return(outid)
        }),collapse=line_separator),
        #Title
        Title = paste0(sapply(names(self$titles), function(name){
          outtitle <- paste(name, self$titles[[name]],sep=":")
          return(outtitle)
        }),collapse=line_separator),
        #Description
        Description = paste0(sapply(names(self$descriptions), function(name){
          outdesc <- paste(name, self$descriptions[[name]],sep=":")
          return(outdesc)
        }),collapse=line_separator),
        #Subject
        Subject = paste0(sapply(self$subjects,function(subject){
          key <- subject$key
          name <- subject$name
          str <- key
          #if name/uri not null, we add these information (case of an explicit thesaurus)
          if(!is.null(subject$name)) key <- sprintf("%s[%s]", key, name)
          if(!is.null(subject$uri)) key <- paste(key, subject$uri, sep = "@")
          kwds <- paste0(sapply(subject$keywords, function(kwd){
            outkwd <- kwd$name
            if(!is.null(kwd$uri)) outkwd <- paste(outkwd, kwd$uri, sep="@")
            return(outkwd)
          }),collapse=",")
          outsubj <- paste(name,kwds,sep=":")
          return(outsubj)
        }),collapse=line_separator),
        #Contact
        Creator = paste0(sapply(unique(sapply(self$contacts, function(contact){contact$role})),function(role){
          role_contacts <- self$contacts[sapply(self$contacts, function(x){x$role == role})]
          outrole <- paste(role, paste0(sapply(role_contacts, function(role_c){return(role_c$identifiers[["id"]])}),collapse=","), sep=":")
          return(outrole)
        }),collapse=line_separator),
        #Date
        Date = paste0(sapply(self$dates,function(x){
          outdate <- paste(x$key, x$value,sep=":")
          return(outdate)
        }),collapse=line_separator),
        #Type
        Type = paste0(sapply(names(self$types),function(type){
          outtype <- paste(type, self$types[[type]],sep=":")
          return(outtype)
        }),collapse=line_separator),
        #Language
        Language = self$language,
        #SpatialCoverage
        SpatialCoverage = {
          outsp <- ""
          if(!is.null(self$spatial_extent)){
            outsp <- paste(sprintf("SRID=%s",self$srid),st_as_text(self$spatial_extent),sep=";")
          }else{
            if(!is.null(self$srid)) outsp <- paste0("srid:", self$srid)
          }
          outsp
        },
        #TemporalCoverage
        TemporalCoverage = {
          outime <- ""
          if(length(self$temporal_extent)>0){
            if(names(self$temporal_extent)[1] == "instant"){
              outime <- posix_to_str(self$temporal_extent$instant)
            }else if(length(names(self$temporal_extent))==2 && names(self$temporal_extent) %in% c("start","end")){
              start <- posix_to_str(self$temporal_extent$start)
              end <- posix_to_str(self$temporal_extent$end)
              outime <- paste0(start,"/",end)
            }
          }
          outime
        },
        #Relation
        Relation = paste0(sapply(self$relations,function(relation){
          outrel <- paste0(relation$key,":\"",relation$name,"\"")
          if(!is.null(relation$description)) outrel <- paste0(outrel,"[\"",relation$description,"\"]")
          if(!is.null(relation$link)) outrel <- paste(outrel, relation$link, sep = "@")
          return(outrel)
        }),collapse=line_separator),
        #Rights
        Rights = paste0(sapply(self$rights, function(right){
          value <- right$value
          if(!endsWith(right$key, "Constraint")) value <- paste0("\"", value,"\"")
          outright <- paste0(right$key, ":", value)
          return(outright)
        }),collapse = line_separator),
        #Provenance
        Provenance = {
          outprov <- NA
          if(!is.null(self$provenance)){
            outprov <- paste0("statement:", self$provenance$statement, line_separator)
            if(length(self$provenance$processes)>0){
              processes_str <- paste0(sapply(self$provenance$processes, function(process){
                rationale <- paste0("\"", process$rationale, "\"")
                outproc <- paste0("process:", rationale)
                if(!is.null(process$description)){
                  description <- paste0("\"", process$description, "\"")
                  outproc <- paste0(outproc, "[", description, "]")
                }
                return(outproc)
              }),collapse=line_separator)
              outprov <- paste0(outprov, processes_str)
              #processors_str <- paste0("processor:",paste0(sapply(self$provenance$processes, function(process){
              #  return(process$processor$id)
              #}),collapse=","))
              #outprov <- paste0(outprov, processors_str)
            }
          }
          outprov
        },
        #Data
        Data = {
          out_sources <- list()
          
          outdata <- ""
          
          if(!is.null(self$data$dir)){
            outdata <- paste0("dir:", self$data$dir, line_separator)
          }
          
          if(!is.null(self$data$access)){
            outdata <- paste0("access:", self$data$access, line_separator)
          }
          
          if(!is.null(self$data$source)){
            for(src in self$data$source){
              src_uri <- attr(src,"uri")
              attributes(src) <- NULL
              if(is.null(out_sources)) out_sources <- ""
              if(!is.null(src_uri)){
                out_sources <- c(out_sources, paste0(src, "@", src_uri))
              }else{
                out_sources <- c(out_sources, src)
              }
            }
            outdata <- paste0(outdata, "source:", paste0(out_sources, collapse=","), line_separator)
          }
          if(!is.null(self$data$sourceType)) outdata <- paste0(outdata, "sourceType:", self$data$sourceType, line_separator)
          if(!is.null(self$data$sourceZip)) outdata <- paste0(outdata, "sourceZip:", tolower(as.character(self$data$sourceZip)), line_separator)
          if(!is.null(self$data$sourceZipOnly)) outdata <- paste0(outdata, "sourceZipOnly:", tolower(as.character(self$data$sourceZipOnly)), line_separator)
          out_upload_sources <- NULL
          if(!is.null(self$data$uploadSource)){
            for(src in self$data$uploadSource){
              src_uri <- attr(src,"uri")
              attributes(src) <- NULL
              if(is.null(out_upload_sources)) out_upload_sources <- ""
              out_upload_sources <- paste0(out_upload_sources, src, "@", src_uri)
            }
            outdata <- paste0(outdata, "uploadSource:", out_upload_sources, line_separator)
          }
          if(!is.null(self$data$uploadType)){
            outdata <- paste0(outdata, "uploadType:", tolower(as.character(self$data$uploadType)), line_separator)
          }
          if(!is.null(self$data$upload)) outdata <- paste0(outdata, "upload:", tolower(as.character(self$data$upload)), line_separator)
          
          if(!is.null(self$data$featureType)){
            outdata <- paste0(outdata, "featureType:", tolower(as.character(self$data$featureType)), line_separator)
          }
          
          if(!is.null(self$data$sql)) outdata <- paste0(outdata, "sql:", self$data$sql, line_separator)
          if(!is.null(self$data$workspace)) outdata <- paste0(outdata, "workspace:", self$data$workspace, line_separator)
          if(!is.null(self$data$store)) outdata <- paste0(outdata, "store:", self$data$store, line_separator)
          if(!is.null(self$data$layername)) outdata <- paste0(outdata, "layername:", self$data$layername, line_separator)
          if(!is.null(self$data$cqlfilter)) outdata <- paste0(outdata, "cqlfilter:", self$data$cqlfilter, line_separator)
          if(length(self$data$styles)>0){
            out_styles <- paste0(self$data$styles, collapse=",")
            outdata <- paste0(outdata, "style:", out_styles, line_separator)
          }
          if(length(self$data$parameters)>0){
            out_params <- paste0(sapply(names(self$data$parameters), function(paramName){
              param <- self$data$parameters[[paramName]]
              out_param <- paste0("parameter:", param$fieldname, "[", paramName, "],", param$regexp, ",", param$defaultvalue)
              return(out_param)
            }),collapse=line_separator)
            outdata <- paste0(outdata, out_params,line_separator)
          }
          if(!is.null(self$data$geometryField) && !is.null(self$data$geometryType)){
            outdata <- paste0(outdata, "geometry:", self$data$geometryField, ",", self$data$geometryType, line_separator)
          }
          if(length(self$data$attributes)>0) {
            out_attrs <- paste0(sapply(self$data$attributes, function(attribute){
              uri <- attr(attribute, "uri")
              desc <- attr(attribute, "description")
              attributes(attribute) <- NULL
              out_attr <- attribute
              if(!is.null(desc)) out_attr <- paste0(out_attr, "[\"", out_attr, "\"]")
              if(!is.null(uri)) out_attr <- paste0(out_attr, "@", uri)
              return(out_attr)
            }), collapse = ",")
            outdata <- paste0(outdata, "attribute:", out_attrs, line_separator)
            
          }
          if(length(self$data$variables)>0) for(variable in self$data$variables){
            out_vars <- paste0(sapply(self$data$variables, function(variable){
              uri <- attr(variable, "uri")
              desc <- attr(variable, "description")
              attributes(variable) <- NULL
              out_var <- variable
              if(!is.null(desc)) out_var <- paste0(out_var, "[\"", out_var, "\"]")
              return(out_var)
            }),collapse=",")
            outdata <- paste0(outdata, "variable:", out_vars, line_separator)
          }
          if(length(self$data$actions)>0){
            for(action in self$data$actions){
              out_act <- paste0("action:", action$id)
              if(!is.null(action$def)) out_act <- paste0(out_act, "[\"", action$def, "\"]")
              if(!is.null(action$script)) out_act <- paste0(out_act, "@", action$script)
              outdata <- paste0(outdata, out_act, line_separator)
            }
            outdata <- paste0(outdata, "run:", tolower(as.character(self$data$run)), line_separator)
            action_options <- self$data$actions[[1]]$options
            for(optname in names(action_options)){
              optvalue <- action_options[[optname]]
              if(length(optvalue)>0) optvalue <- paste0(optvalue, collapse=",")
              if(is.logical(optvalue)) optvalue <- tolower(as.character(optvalue))
              outdata <- paste0(outdata, sprintf("action_option_%s:%s", optname, optvalue),
                                line_separator)
            }
          }
          if(endsWith(outdata, line_separator)) outdata <- substr(outdata, 0, nchar(outdata)-2)
          outdata
          
        },
        stringsAsFactors = FALSE
      )
      return(out)
    }
    
  )
)

