#'geoflow_entity
#'@export
geoflow_entity <- R6Class("geoflow_entity",
  public = list(
    identifiers = list(),
    date = NULL,
    language = "eng",
    types = list(generic = "dataset"),
    title = NULL,
    descriptions = list(),
    subjects = list(),
    contacts = list(),
    relations = list(),
    rights = list(),
    spatial_extent = NULL,
    srid = NULL,
    temporal_extent = NULL,
    provenance = NULL,
    data = NULL,
    status = NULL,
    initialize = function(){},
    
    #setIdentifier
    setIdentifier = function(key, id){
      self$identifiers[[key]] <- id
    },
    
    #setDate
    setDate = function(date){
      self$date <- date
    },
    
    #setLanguage
    setLanguage = function(language){
      self$language <- language
    },
    
    #setType
    setType = function(key, type){
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
    
    #setSpatialExtent
    setSpatialExtent = function(wkt = NULL, bbox = NULL, data = NULL, crs = NA){
      if(is.null(wkt) & is.null(bbox) & is.null(data)){
        stop("At least one of the arguments 'wkt' (WKT string) or 'bbox' should be provided!")
      }
      if(!is.null(wkt)) spatial_extent <- attr(sf::st_as_sfc(wkt, crs = crs), "bbox")
      if(!is.null(bbox)) spatial_extent <- bbox
      if(!is.null(data)) spatial_extent <- sf::st_bbox(data)
      if(class(spatial_extent)[1]=="try-error"){
        stop("The spatial extent is invalid!")
      }
      self$spatial_extent <- spatial_extent
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
      
      resourceId <- if(!is.null(self$data$identifier)) self$data$identifier else self$identifiers$id
      basefilename <- paste0(self$identifiers$id,"_",self$data$type,"_",resourceId)
      
      #here either we only pickup zipped files and re-distribute them in job data directory
      #or we write it from entity$data$features if the latter is not NULL and if writer available (for now only shp)
      #The latter allows to push features filtered by cqlfilter (matching an eventual geoserver layer) to Zenodo
      #instead of the complete dataset
      
      isSourceUrl <- regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", self$data$source) > 0
      if(isSourceUrl){
        warnMsg <- "Copying data from URL to Job data directory!"
        config$logger.warn(warnMsg)
        download.file(self$data$source, destfile = basefilename)
      }else{
        if(is.null(self$data$features)){
          
          config$logger.info("Copying data local file(s) to Job data directory!")
          srcFilename <- self$data$source
          data.files <- list.files(path = dirname(srcFilename), pattern = self$data$sourceName)
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
              data.files <- list.files(pattern = self$data$sourceName)
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
            errMsg <- sprintf("Copying data local file(s): no files found for source '%s' (%s)", srcFilename, self$data$sourceName)
            config$logger.error(errMsg)
            stop(errMsg)
          }
          
        }else{
          config$logger.info("Writing entity data features to job data directory!")
          switch(self$data$type,
            "shp" = {
              sf::st_write(self$data$features, paste0(basefilename, ".shp"), delete_dsn = TRUE)
              data.files <- list.files(pattern = basefilename)
              zip(zipfile = paste0(basefilename, ".zip"), files = data.files)
            },{
              config$logger.warn(sprintf("Entity data features writer not implemented for type '%s'", self$data$type))
            }
          )
        }
      }
      setwd(wd)
    },
    
    #enrichWithData
    enrichWithData = function(config){
    
      layername <- if(!is.null(self$data$identifier)) self$data$identifier else self$identifiers$id
      
      TEMP_DATA_DIR <- file.path(getwd(), "geoflow_temp_data")
      if(!dir.exists(TEMP_DATA_DIR)){
        config$logger.info("Create geoflow temporary data directory")
        dir.create(TEMP_DATA_DIR)
      }
      trgFilename <- file.path(TEMP_DATA_DIR, paste0(self$data$sourceName,".zip"))
      
      switch(self$data$type,
           #Method for ESRI Shapefile (if remote, shapefiles should be zipped)
           "shp" = {
             shpExists <- FALSE
             isSourceUrl <- regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", self$data$source) > 0
             if(isSourceUrl){
               warnMsg <- "Downloading remote data from URL to temporary geoflow temporary data directory!"
               config$logger.warn(warnMsg)
               download.file(self$data$source, destfile = trgFilename)
               unzip(zipfile = trgFilename, exdir = TEMP_DATA_DIR, unzip = getOption("unzip"))
               shpExists <- TRUE
             }else{
               data.files <- list.files(path = dirname(self$data$source), pattern = self$data$sourceName)
               if(length(data.files)>0){
                 shpExists <- TRUE
                 config$logger.info("Copying local data to temporary geoflow temporary data directory")
                 isZipped <- any(sapply(data.files, endsWith, ".zip"))
                 if(!isZipped){
                   zip(zipfile = trgFilename, files = data.files)
                   for(data.file in data.files) file.copy(from = data.file, to = TEMP_DATA_DIR)
                 }else{
                   file.copy(from = self$data$source, to = TEMP_DATA_DIR)
                   unzip(zipfile = trgFilename, exdir = TEMP_DATA_DIR, unzip = getOption("unzip"))
                 }
               }
             }
            
             if(shpExists){
               #read shapefile
               config$logger.info("Read Shapefiles from geoflow temporary data directory")
               trgShp <- file.path(TEMP_DATA_DIR, paste0(self$data$sourceName,".shp"))
               sf.data <- sf::st_read(trgShp)
               if(!is.null(sf.data)){
                 #we try to apply the cql filter specified as data property
                 if(!is.null(self$data$cqlfilter)){
                   sf.data <- filter_sf_by_cqlfilter(sf.data, self$data$cqlfilter)
                 }
                 self$data$setFeatures(sf.data)
                 
                 #dynamic srid
                 sf.crs <- sf::st_crs(sf.data)
                 if(!is.na(sf.crs)) self$setSrid(sf.crs$epsg)
                 #dynamic spatial extent
                 self$setSpatialExtent(data = sf.data)
                 #dynamic relations related to OGC services (only executed if geosapi action is handled in workflow)
                 if(any(sapply(config$actions, function(x){regexpr("geosapi",x$id)>0}))){
                   #Thumbnail
                   new_thumbnail <- geoflow_relation$new()
                   new_thumbnail$setKey("thumbnail")
                   new_thumbnail$setName(layername)
                   new_thumbnail$setDescription(paste0(self$title, " - Map Overview"))
                   new_thumbnail$setLink(sprintf("%s/%s/ows?service=WMS&version=1.1.0&request=GetMap&layers=%s&bbox=%s&width=600&height=300&srs=EPSG:%s&format=image/png", 
                                                 config$software$output$geoserver_config$parameters$url, 
                                                 config$software$output$geoserver_config$properties$workspace,
                                                 layername, paste(self$spatial_extent,collapse=","),self$srid))
                   self$addRelation(new_thumbnail)
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
                 #TODO dynamic subjects
                   
                   
               }else{
                 warnMsg <- sprintf("Cannot read data source '%s'. Dynamic metadata computation aborted!", trgShp)
                 config$logger.warn(warnMsg)
               }
             }else{
               warnMsg <- sprintf("No readable source '%s'. Dynamic metadata computation aborted!", self$data$source)
               config$logger.warn(warnMsg)
             }
          },
          #other format handlers to come
          {
            config$logger.warn(sprintf("Metadata dynamic handling based on 'data' not implemented for type '%s'", self$data$type))
          }
      ) 
      
      #remove temp dir
      unlink(TEMP_DATA_DIR, force = TRUE)
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
    }
  )
)

