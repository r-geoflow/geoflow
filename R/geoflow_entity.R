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
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate a geoflow_entity object
#'  }
#'  \item{\code{setIdentifier(key, id)}}{
#'    Set an identifier given a key. Default key is "id", but others can be specified, eg "doi".
#'  }
#'  \item{\code{addDate(dateType, date)}}{
#'    Add a date object of class \code{Date} or \code{POSIXt}, with a date type, object of class \code{character}.
#'  }
#'  \item{\code{setLanguage(language)}}{
#'    Set the language used for the entity description (metadata). Default is "eng".
#'  }
#'  \item{\code{setType(key, type)}}{
#'    Set the type of description. By default a generic type (key = "generic") is defined to "dataset", and
#'    will be used as default type for actions that perform metadata production / publication.
#'  }
#'  \item{\code{setTitle(title)}}{
#'    Set entity title
#'  }
#'  \item{\code{setDescription(description)}}{
#'    Set entity description
#'  }
#'  \item{\code{addSubject(subject)}}{
#'    Add a subject, object of class \code{geoflow_subject}.
#'  }
#'  \item{\code{addContact(contact)}}{
#'    Add a contact, object of class \code{geoflow_contact}
#'  }
#'  \item{\code{addRelation(relation)}}{
#'    Add a relation, object of class \code{geoflow_relation}
#'  }
#'  \item{\code{addRight(right)}}{
#'    Add a right, object of class \code{geoflow_right}
#'  }
#'  \item{\code{setSpatialExtent(wkt,bbox,data,crs)}}{
#'    Set spatial extent. Various ways can be used to set the spatial extent 1) with a WKT string,
#'    2) with a bbox, object of class \code{matrix}, or 3) specifying a data object (from \pkg{sf}).
#'    The \code{crs} (coordinate reference system) should be specified with the crs SRID (number).
#'    The spatial extent is not necessarily a bounding box but can be one or more geometries.
#'  }
#'  \item{\code{setSpatialBbox(wkt,bbox,data,crs)}}{
#'    Set spatial bbox. Various ways can be used to set the spatial extent 1) with a WKT string,
#'    2) with a bbox, object of class \code{matrix}, or 3) specifying a data object (from \pkg{sf}).
#'    The \code{crs} (coordinate reference system) should be specified with the crs SRID (number).
#'  }
#'  \item{\code{setSrid(srid)}}{
#'    Set the SRID
#'  }
#'  \item{\code{setTemporalExtent(str)}}{
#'    Set the temporal extent from a string representation (object of class \code{character}) of
#'    an ISO date / datetime (timestamp) or interval.
#'  }
#'  \item{\code{setProvenance(provenance)}}{
#'    Set the provenance as object of class \code{geoflow_provenance}.
#'  }
#'  \item{\code{setData(data)}}{
#'    Set the related data object of class \code{geoflow_data}
#'  }
#'  \item{\code{copyDataToJobDir(config, jobdir)}}{
#'    This function will look at data object associated to the entity (previously set with \code{setData}),
#'    and will try to (download)/copy the data source to the geoflow job directory.
#'  }
#'  \item{\code{enrichWithFeatures(config)}}{
#'    This function will enrich the entity with data features, but trying to read the spatial data (eg shapefile,
#'    sql query - if a database input software is declared in the geoflow config). This method will overwrite 
#'    spatial metadata such as the bounding box and temporal extent. Note that the user spatial extent is not overwriten
#'    since it may content finer geometries than a bounding box
#'  }
#'  \item{\code{enrichWithRelations(config)}}{
#'    This function that will enrich the entity with relations. At now this is essentially related to adding 
#'    relations if a Geoserver (geosapi) publishing action is enabled in which case this function will add 1) 
#'    a thumbnail link built from OGC WMS service, 2) a WMS protocol relation, 3) WFS data protocols in common 
#'    formats (GML, GeoJSON, ESRI Shapefile).
#'  }
#'  \item{\code{enrichWithSubjects(config)}}{
#'    This function is expected to enrich entities with subjects. Related to data vocabularies / thesauri / ontologies.
#'    NOT YET IMPLEMENTED
#'  }
#'  \item{\code{getContacts(pretty)}}{
#'    Get the list of entity contacts. By default, a list of \code{geoflow_contact} will be returned. To
#'    return the list of entity contacts as \code{data.frame}, set \code{pretty = TRUE}.
#'  }
#'  \item{\code{getRelations(pretty)}}{
#'    Get the list of entity relations. By default, a list of \code{geoflow_relation} will be returned. To
#'    return the list of entity relations as \code{data.frame}, set \code{pretty = TRUE}.
#'  }
#'  \item{\code{getSubjects(pretty)}}{
#'    Get the list of entity subjects. By default, a list of \code{geoflow_subjects} will be returned. To
#'    return the list of entity subjects as \code{data.frame}, set \code{pretty = TRUE}.
#'  }
#'  \item{\code{setStatus(status)}}{
#'    Set a simple status either "draft" or "published". This method is required to deal with Zenodo (zen4R)
#'    publishing action.
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_entity <- R6Class("geoflow_entity",
  public = list(
    identifiers = list(),
    dates = list(),
    language = "eng",
    types = list(generic = "dataset"),
    title = NULL,
    descriptions = list(),
    subjects = list(),
    contacts = list(),
    relations = list(),
    rights = list(),
    spatial_extent = NULL,
    spatial_bbox = NULL,
    srid = NULL,
    temporal_extent = NULL,
    provenance = NULL,
    data = NULL,
    status = NULL,
    initialize = function(){
      self$addDate("creation", Sys.time())
    },
    
    #setIdentifier
    setIdentifier = function(key = "id", id){
      self$identifiers[[key]] <- id
    },
    
    #addDate
    addDate = function(dateType, date){
      date_obj <- geoflow_date$new()
      date_obj$setKey(dateType)
      date_obj$setValue(date)
      self$dates[[length(self$dates)+1]] <- date_obj
    },
    
    #setLanguage
    setLanguage = function(language){
      self$language <- language
    },
    
    #setType
    setType = function(key = "generic", type){
      self$types[[key]] <- type
    },
    
    #setTitle
    setTitle = function(title){
      self$title <- title
    },
    
    #setDescription
    setDescription = function(key, description){
      self$descriptions[[key]] <- description
    },
    
    #addSubject
    addSubject = function(subject){
      if(!is(subject, "geoflow_subject")){
        stop("The argument should be an object of class 'geoflow_subject'")
      }
      self$subjects <- c(self$subjects, subject)
    },
    
    #addContact
    addContact = function(contact){
      if(!is(contact, "geoflow_contact")){
        stop("The argument should be an object of class 'geoflow_contact'")
      }
      self$contacts <- c(self$contacts, contact)
    },
    
    #addRelation
    addRelation = function(relation){
      if(!is(relation, "geoflow_relation")){
        stop("The argument should be an object of class 'geoflow_relation'")
      }
      self$relations <- c(self$relations, relation)
    },
    
    #addRight
    addRight = function(right){
      if(!is(right, "geoflow_right")){
        stop("The argument should be an object of class 'geoflow_right'")
      }
      self$rights <- c(self$rights, right)
    },
    
    #setSpatialExtent (method call from handler, but not from enrichWithFeatures)
    setSpatialExtent = function(wkt = NULL, bbox = NULL, data = NULL, crs = NA){
      if(is.null(wkt) & is.null(bbox) & is.null(data)){
        stop("At least one of the arguments 'wkt' (WKT string) or 'bbox' should be provided!")
      }
      spatial_extent <- NULL
      if(!is.null(wkt)) spatial_extent <- sf::st_as_sfc(wkt, crs = crs)
      if(!is.null(bbox)) spatial_extent <- bbox
      if(!is.null(data)) spatial_extent <- data

      if(class(spatial_extent)[1]=="try-error"){
        stop("The spatial extent is invalid!")
      }
      self$spatial_extent <- spatial_extent
      self$setSpatialBbox(wkt, bbox, data, crs)
    },
    
    #setSpatialBbox  (method call from handler in setSpatialExtent, and from enrichWithFeatures)
    setSpatialBbox = function(wkt = NULL, bbox = NULL, data = NULL, crs = NA){
      if(is.null(wkt) & is.null(bbox) & is.null(data)){
        stop("At least one of the arguments 'wkt' (WKT string) or 'bbox' should be provided!")
      }
      spatial_bbox  <- NULL
      if(!is.null(wkt)) spatial_bbox <- sf::st_bbox(sf::st_as_sfc(wkt, crs = crs))
      if(!is.null(bbox)) spatial_bbox <- bbox
      if(!is.null(data)) spatial_bbox <- sf::st_bbox(data)
      
      if(class(spatial_bbox)[1]=="try-error"){
        stop("The spatial bbox is invalid!")
      }
      self$spatial_bbox <- spatial_bbox
    },
    
    #setSrid
    setSrid = function(srid){
      self$srid <- srid
    },
    
    #setTemporalExtent
    setTemporalExtent = function(str){
      isInstant <- FALSE
      strs <- unlist(strsplit(str,"/"))
      if(length(strs)==1) isInstant <- TRUE
      if(isInstant){
        self$temporal_extent <- list(instant = str_to_posix(strs))
      }else{
        self$temporal_extent <- list(
          start = str_to_posix(strs[1]),
          end = str_to_posix(strs[2])
        )
      }
    },
    
    #setProvenance
    setProvenance = function(provenance){
      if(!is(provenance, "geoflow_provenance")){
        stop("The provenance should be an object of class 'geoflow_provenance'")
      }
      self$provenance <- provenance
    },
    
    #setData
    setData = function(data){
      if(!is(data,"geoflow_data")){
        stop("Data should be an object of class 'geoflow_data'")
      }
      self$data <- data
    },
    
    #copyDataToJobDir
    copyDataToJobDir = function(config, jobdir){
      
      wd <- getwd()
      setwd(file.path(jobdir,"data"))
      
      config$logger.info(sprintf("Copying data to job directory '%s'", jobdir))
      
      for(i in 1:length(self$data$source)){
      
        datasource <- self$data$source[[i]]
        datasource_parts <- unlist(strsplit(datasource, "\\."))
        datasource_name <- datasource_parts[1]
        datasource_ext <- ifelse(length(datasource_parts)>1, datasource_parts[2], "zip")
        datasource_file <- attr(datasource, "uri")
        attributes(datasource) <- NULL
        
        
        if(is.null(datasource_file)){
          warnMsg <- sprintf("No source file/URL for datasource '%s'. Data source copying aborted!", datasource_name)
          config$logger.warn(warnMsg)
          setwd(wd)
          return(FALSE)
        }
        
        config$logger.info(sprintf("Copying data source %s '%s' (%s) to job directory '%s'",
                                   i, datasource, datasource_file, jobdir))
        
        resourceId <- if(!is.null(self$data$layername) & i==1) self$data$layername else datasource_name
        basefilename <- paste0(self$identifiers$id, "_", self$data$uploadType,"_",resourceId)
      
        #here either we only pickup zipped files and re-distribute them in job data directory
        #or we write it from entity$data$features if the latter is not NULL and if writer available (for now only shp)
        #The latter allows to push features filtered by cqlfilter (matching an eventual geoserver layer) to Zenodo
        #instead of the complete dataset
      
        isSourceUrl <- regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", datasource_file) > 0
        if(isSourceUrl && is.null(self$data$features)){
          #case where data is remote and there was no data enrichment in initWorkflow
          warnMsg <- "Copying data from URL to Job data directory!"
          config$logger.warn(warnMsg)
          download.file(datasource_file, destfile = paste(basefilename, datasource_ext, sep="."), mode = "wb")
        }else{
          if(is.null(self$data$features)){
            config$logger.info("Copying data local file(s) to Job data directory!")
            srcFilename <- datasource_file
            data.files <- list.files(path = dirname(srcFilename), pattern = datasource_name)
            if(length(data.files)>0){
              isZipped <- any(sapply(data.files, endsWith, ".zip"))
              if(!isZipped){
                config$logger.info("Copying data local file(s): copying also unzipped files to job data directory")
                for(data.file in data.files){
                  file.copy(from = data.file, to = getwd())
                  fileparts <- unlist(strsplit(data.file,"\\."))
                  fileext <- fileparts[length(fileparts)]
                  file.rename(from = data.file, to = paste0(basefilename, ".", fileext))
                }
                config$logger.info("Copying data local file(s): zipping files as archive into job data directory")
                data.files <- list.files(pattern = basefilename)
                if(length(data.files)>0) zip(zipfile = paste0(basefilename,".zip"), files = data.files)
              }else{
                config$logger.info("Copying data local file(s): copying unzipped files to job data directory")
                unzip(zipfile = srcFilename, unzip = getOption("unzip"))
                data.files <- list.files(pattern = datasource_name)
                if(length(data.files)>0) for(data.file in data.files){
                  file.copy(from = data.file, to = getwd())
                  fileparts <- unlist(strsplit(data.file,"\\."))
                  fileext <- fileparts[length(fileparts)]
                  file.rename(from = data.file, to = paste0(basefilename, ".", fileext))
                }
                data.files <- list.files(pattern = basefilename)
                if(length(data.files)>0) zip(zipfile = paste0(basefilename,".zip"), files = data.files)
              }
            }else{
              errMsg <- sprintf("Copying data local file(s): no files found for source '%s' (%s)", srcFilename, datasource_name)
              config$logger.error(errMsg)
              stop(errMsg)
            }
            
          }else{
            config$logger.info("Writing entity data features to job data directory!")
            switch(self$data$uploadType,
              "shp" = {
                sf::st_write(self$data$features, paste0(basefilename, ".shp"), delete_dsn = FALSE)
                data.files <- list.files(pattern = basefilename)
                zip(zipfile = paste0(basefilename, ".zip"), files = data.files)
              },{
                config$logger.warn(sprintf("Entity data features writer not implemented for type '%s'", self$data$uploadType))
              }
            )
            #we also copy the source with its native name
            #this is required eg for geosapi if we want to set-up filtered shapefile layers
            #based on the same source shapefile
            config$logger.info("For entities enriched with spatial data, copy the datasource")
            file.copy(
              from = file.path(config$wd, get_temp_directory(), paste0(datasource_name, ".", datasource_ext)),
              to = getwd()
            )
            
          }
        }
      }
      
      if(self$data$uploadType == "other" & self$data$uploadZip){
        config$logger.info("uploadZip = TRUE: Zip sources into single data file")
        data.files <- list.files()
        print(data.files)
        zip(zipfile = paste0(self$identifiers$id, "_files", ".zip"), files = data.files)
        if(self$data$uploadZipOnly){
          config$logger.info("uploadZipOnly = TRUE: deleting zipped, they will not be uploaded")
          for(data.file in data.files){
            unlink(data.file, force = TRUE)
          }
        }else{
          config$logger.info("uploadZipOnly = FALSE: both zip and zipped files will be uploaded")
        }
      }else{
        config$logger.info("uploadZip = FALSE: source files will be uploaded")
      }
      
      setwd(wd)
      
    },
    
    #enrichWithFeatures
    enrichWithFeatures = function(config){
    
      if(length(self$data$source)>1) 
        config$logger.warn("More than one data sources, entity metadata enrichment with data based on the first source only!")
      
      datasource <- self$data$source[[1]]
      datasource_name <- unlist(strsplit(datasource, "\\."))[1]
      datasource_file <- attr(datasource, "uri")
      attributes(datasource) <- NULL
      
      types_without_file <- c("dbtable","dbview","dbquery")
      datasource_file_needed <- !(self$data$uploadType %in% types_without_file)
      if(datasource_file_needed && is.null(datasource_file)){
        warnMsg <- sprintf("No source file/URL for datasource '%s'. Dynamic metadata computation aborted!", datasource_name)
        config$logger.warn(warnMsg)
        return(FALSE)
      }
      
      TEMP_DATA_DIR <- file.path(getwd(), get_temp_directory())
      if(!dir.exists(TEMP_DATA_DIR)){
        config$logger.info("Create geoflow temporary data directory")
        dir.create(TEMP_DATA_DIR)
      }
      
      switch(self$data$uploadType,
           #shp - ESRI Shapefile (if remote, shapefiles should be zipped)
           #---------------------------------------------------------------------------------
           "shp" = {
             
             trgFilename <- file.path(TEMP_DATA_DIR, paste0(datasource_name,".zip"))
             
             shpExists <- FALSE
             isSourceUrl <- regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", datasource_file) > 0
             if(isSourceUrl){
               warnMsg <- "Downloading remote data from URL to temporary geoflow temporary data directory!"
               config$logger.warn(warnMsg)
               download.file(datasource_file, destfile = trgFilename, mode = "wb")
               unzip(zipfile = trgFilename, exdir = TEMP_DATA_DIR, unzip = getOption("unzip"))
               shpExists <- TRUE
             }else{
               data.files <- list.files(path = dirname(datasource_file), pattern = datasource_name)
               if(length(data.files)>0){
                 shpExists <- TRUE
                 config$logger.info("Copying local data to temporary geoflow temporary data directory")
                 isZipped <- any(sapply(data.files, endsWith, ".zip"))
                 if(!isZipped){
                   zip(zipfile = trgFilename, files = data.files)
                   for(data.file in data.files) file.copy(from = data.file, to = TEMP_DATA_DIR)
                 }else{
                   file.copy(from = datasource_file, to = TEMP_DATA_DIR)
                   unzip(zipfile = trgFilename, exdir = TEMP_DATA_DIR, unzip = getOption("unzip"))
                 }
               }
             }
            
             if(shpExists){
               #read shapefile
               config$logger.info("Read Shapefiles from geoflow temporary data directory")
               trgShp <- file.path(TEMP_DATA_DIR, paste0(datasource_name,".shp"))
               sf.data <- sf::st_read(trgShp)
               if(!is.null(sf.data)){
                 #we try to apply the cql filter specified as data property
                 if(!is.null(self$data$cqlfilter)){
                   sf.data <- filter_sf_by_cqlfilter(sf.data, self$data$cqlfilter)
                 }
                 self$data$setFeatures(sf.data)
                 
                 #dynamic srid
                 sf.crs <- sf::st_crs(sf.data)
                 if(!is.na(sf.crs)){
                   srid <- if(!is.null(self$srid)) self$srid else ""
                   if(srid != sf.crs$epsg){
                    config$logger.info(sprintf("Overwriting entity srid [%s] with shapefile srid [%s]", srid, sf.crs$epsg)) 
                    self$setSrid(sf.crs$epsg)
                   }
                 }
                 #dynamic spatial extent
                 config$logger.info("Overwriting entity bounding box with shapefile bounding box")
                 self$setSpatialBbox(data = sf.data)
                   
               }else{
                 warnMsg <- sprintf("Cannot read data source '%s'. Dynamic metadata computation aborted!", trgShp)
                 config$logger.warn(warnMsg)
               }
             }else{
               warnMsg <- sprintf("No readable source '%s'. Dynamic metadata computation aborted!", datasource_file)
               config$logger.warn(warnMsg)
             }
            },
           #dbtable
           #---------------------------------------------------------------------------------
           "dbtable" = {
             DBI <- config$software$input$dbi
             if(!is.null(DBI)){
               sf.data <- sf::st_read(DBI, datasource_name)
               if(!is.null(sf.data)){
                 #we try to apply the cql filter specified as data property
                 if(!is.null(self$data$cqlfilter)){
                   sf.data <- filter_sf_by_cqlfilter(sf.data, self$data$cqlfilter)
                 }
                 self$data$setFeatures(sf.data)
                 if(is(sf.data, "sf")){
                   #dynamic srid
                   sf.crs <- sf::st_crs(sf.data)
                   if(!is.na(sf.crs)){
                     srid <- if(!is.null(self$srid)) self$srid else ""
                     if(srid != sf.crs$epsg){
                       config$logger.info(sprintf("Overwriting entity srid [%s] with DB spatial table srid [%s]", srid, sf.crs$epsg)) 
                       self$setSrid(sf.crs$epsg)
                     }
                   }
                   #dynamic spatial extent
                   config$logger.info("Overwriting entity bounding box with DB spatial table bounding box")
                   self$setSpatialBbox(data = sf.data)
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
                 if(!is.null(self$data$cqlfilter)){
                   sf.data <- filter_sf_by_cqlfilter(sf.data, self$data$cqlfilter)
                 }
                 self$data$setFeatures(sf.data)
                 if(is(sf.data, "sf")){
                   #dynamic srid
                   sf.crs <- sf::st_crs(sf.data)
                   if(!is.na(sf.crs)){
                     srid <- if(!is.null(self$srid)) self$srid else ""
                     if(srid != sf.crs$epsg){
                       config$logger.info(sprintf("Overwriting entity srid [%s] with DB spatial view srid [%s]", srid, sf.crs$epsg)) 
                       self$setSrid(sf.crs$epsg)
                     }
                   }
                   #dynamic spatial extent
                   config$logger.info("Overwriting entity bounding box with DB spatial view bounding box")
                   self$setSpatialBbox(data = sf.data)
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
             
              trgFilename <- file.path(TEMP_DATA_DIR, paste0(datasource_name,".sql"))
             
              sqlFileExists <- FALSE
              if(!is.null(datasource_file)){
                isSourceUrl <- regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", datasource_file) > 0
                if(isSourceUrl){
                  warnMsg <- "Downloading remote SQL file from URL to temporary geoflow temporary data directory!"
                  config$logger.warn(warnMsg)
                  download.file(datasource_file, destfile = trgFilename, mode = "wb")
                  sqlFileExists <- TRUE
                }else{
                  data.files <- list.files(path = dirname(datasource_file), pattern = datasource_name)
                  if(length(data.files)>0){
                    sqlFileExists <- TRUE
                    config$logger.info("Copying local SQL scrit to temporary geoflow temporary data directory")
                    file.copy(from = data.files[1], to = TEMP_DATA_DIR)
                  }
                }
              }
             
              if(sqlFileExists){
                sqlfile <- file.path(TEMP_DATA_DIR, paste0(datasource_name,".sql"))
                config$logger.info(sprintf("Reading SQL query from file '%s'", sqlfile))
                sql <- paste(readLines(sqlfile), collapse="")
                config$logger.info(sql)
                self$data$setSql(sql)
                unlink(sqlfile)
              }else{
                if(is.null(self$data$sql)){
                  warnMsg <- sprintf("No SQL file nor 'sql' data property specified for datasource '%s'. Dynamic metadata computation aborted!", datasource_name)
                  config$logger.warn(warnMsg)
                  return(FALSE)
                }
              }
              
              DBI <- config$software$input$dbi
              if(!is.null(DBI)){
                sf.data <- try(sf::st_read(DBI, query = self$data$sql))
                if(!is.null(sf.data)){
                  if(class(sf.data)[1]=="try-error"){
                    errMsg <- sprintf("Error while executing SQL query [%s]. Please check the SQL query! Dynamic data handling aborted!", self$data$sql)
                    config$logger.error(errMsg)
                    return(FALSE)
                    
                  }
                  #we try to apply the cql filter specified as data property
                  if(!is.null(self$data$cqlfilter)){
                    sf.data <- filter_sf_by_cqlfilter(sf.data, self$data$cqlfilter)
                  }
                  self$data$setFeatures(sf.data)
                  if(is(sf.data, "sf")){
                    #dynamic srid
                    sf.crs <- sf::st_crs(sf.data)
                    if(!is.na(sf.crs)){
                      srid <- if(!is.null(self$srid)) self$srid else ""
                      if(srid != sf.crs$epsg){
                        config$logger.info(sprintf("Overwriting entity srid [%s] with SQL query output srid [%s]", srid, sf.crs$epsg)) 
                        self$setSrid(sf.crs$epsg)
                      }
                    }
                    #dynamic spatial extent
                    config$logger.info("Overwriting entity bounding box with SQL query output bounding box")
                    self$setSpatialBbox(data = sf.data)
                    #dynamic view properties required
                    
                    geomtype <- as.character(unique(sf::st_geometry_type(sf.data))[1])
                    gsGeomType <- switch(geomtype,
                      "GEOMETRY" = "Geometry", "GEOMETRYCOLLECTION" = "GeometryCollection",
                      "POINT" = "Point", "MULTIPOINT" = "MultiPoint", 
                      "LINESTRING" = "LineString", "MULTILINESTRING" = "MultiLineString",
                      "POLYGON" = "Polygon", "MULTIPOLYGON" = "MultiPolygon"
                    )
                    config$logger.info(sprintf("Setting entity geometry type '%s'", gsGeomType))
                    self$data$setGeometryType(gsGeomType)
                    geomField <- colnames(sf.data)[sapply(colnames(sf.data), function(x){(is(sf.data[[x]],"sfc"))})][1]
                    config$logger.info(sprintf("Setting entity geometry field '%s'",geomField))
                    self$data$setGeometryField(geomField)
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
            #other format handlers to come
            {
              config$logger.warn(sprintf("Metadata dynamic handling based on 'data' not implemented for type '%s'", self$data$uploadType))
            }
      ) 
    },
    
    #enrichWithRelations
    enrichWithRelations = function(config){
      geosapi_action <- NULL
      actions <- list()
      if(length(config$actions)>0) actions <- config$actions[sapply(config$actions, function(x){regexpr("geosapi",x$id)>0})]
      if(length(actions)>0) geosapi_action <- actions[[1]]
      #dynamic relations related to OGC services (only executed if geosapi action is handled and enabled in workflow)
      if(!is.null(geosapi_action)) if(!is.null(self$data)) if(self$data$uploadType != "other"){
        
        layername <- if(!is.null(self$data$layername)) self$data$layername else self$identifiers$id
        
        #Thumbnail
        new_thumbnail <- geoflow_relation$new()
        new_thumbnail$setKey("thumbnail")
        new_thumbnail$setName(layername)
        new_thumbnail$setDescription(paste0(self$title, " - Layer Overview"))
        new_thumbnail$setLink(sprintf("%s/%s/ows?service=WMS&version=1.1.0&request=GetMap&layers=%s&bbox=%s&width=600&height=300&srs=EPSG:%s&format=image/png", 
                                      config$software$output$geoserver_config$parameters$url, 
                                      config$software$output$geoserver_config$properties$workspace,
                                      layername, paste(self$spatial_bbox,collapse=","),self$srid))
        self$relations <- c(self$relations, new_thumbnail)
        #WMS
        new_wms <- geoflow_relation$new()
        new_wms$setKey("wms")
        new_wms$setName(layername)
        new_wms$setDescription(self$title)
        new_wms$setLink(sprintf("%s/%s/ows?service=WMS", 
                                config$software$output$geoserver_config$parameters$url, 
                                config$software$output$geoserver_config$properties$workspace))
        self$addRelation(new_wms)
        #wfs (GML)
        new_wfs_gml <- geoflow_relation$new()
        new_wfs_gml$setKey("wfs")
        new_wfs_gml$setName(layername)
        new_wfs_gml$setDescription(paste0(self$title, " - GIS Data Download (GML)"))
        new_wfs_gml$setLink(sprintf("%s/%s/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=%s", 
                                    config$software$output$geoserver_config$parameters$url, 
                                    config$software$output$geoserver_config$properties$workspace,
                                    layername))
        self$addRelation(new_wfs_gml)
        #wfs (GeoJSON)
        new_wfs_geojson <- geoflow_relation$new()
        new_wfs_geojson$setKey("wfs")
        new_wfs_geojson$setName(layername)
        new_wfs_geojson$setDescription(paste0(self$title, " - GIS Data Download (GeoJSON)"))
        new_wfs_geojson$setLink(sprintf("%s/%s/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=%s&outputFormat=json", 
                                        config$software$output$geoserver_config$parameters$url, 
                                        config$software$output$geoserver_config$properties$workspace,
                                        layername))
        self$addRelation(new_wfs_geojson)
        #wfs (ESRI Shapefile)
        new_wfs_shp <- geoflow_relation$new()
        new_wfs_shp$setKey("wfs")
        new_wfs_shp$setName(layername)
        new_wfs_shp$setDescription(paste0(self$title, " - GIS Data Download (ESRI Shapefile)"))
        new_wfs_shp$setLink(sprintf("%s/%s/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=%s&outputFormat=SHAPE-ZIP", 
                                    config$software$output$geoserver_config$parameters$url, 
                                    config$software$output$geoserver_config$properties$workspace,
                                    layername))
        self$addRelation(new_wfs_shp)
      }
    },
    
    #enrichWithSubjects
    enrichWithSubjects = function(config){
      stop("Not yet implemented")
    },
    
    #enrichWithMetadata
    enrichWithMetadata = function(config){
      
      #enrich title
      self$title <- enrich_text_from_entity(self$title, self)
  
      #enrich descriptions
      desNames <- names(self$descriptions)
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
    
    #getContacts
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
    
    #getSubjects
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
    
    #getRelations
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
    
    #setStatus
    setStatus = function(status){
      if(!(status %in% c("draft", "published"))){
        stop("The status should be either 'draft' or 'published'")
      }
      self$status <- status
    },
    
    #getJobResource
    getJobResource = function(config, resourceType, filename){
      return(file.path(config$job, resourceType, paste(self$identifiers[["id"]], self$data$uploadType, filename, sep="_")))
    },
    
    #getJobDataResource
    getJobDataResource = function(config, filename){
      self$getJobResource(config, "data", filename)
    },
    
    #getJobMetadataResource
    getJobMetadataResource = function(config, filename){
      self$getJobResource(config, "metadata", filename)
    }
    
  )
)

