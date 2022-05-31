function(action, entity, config){
  
  if(!requireNamespace("geosapi", quietly = TRUE)){
    stop("The 'geosapi-publish-ogc-services' action requires the 'geosapi' package")
  }
  
  #options
  options <- action$options
  createWorkspace <- if(!is.null(options$createWorkspace)) options$createWorkspace else FALSE
  createStore <- if(!is.null(options$createStore)) options$createStore else FALSE
  store_description <- if(!is.null(options$store_description)) options$store_description else ""
  
  #check presence of data
  if(is.null(entity$data)){
    warnMsg <- sprintf("No data object associated to entity '%s'. Skipping data publication!", 
                       entity$identifiers[["id"]])
    config$logger.warn(warnMsg)
    return(NULL)
  }
  
  if(length(entity$data$source)>1) 
    config$logger.warn("More than one data sources, geosapi action will consider the first one only!")
  
  #datasource
  datasource <- entity$data$uploadSource[[1]]
  datasource_name <- NULL
  datasource_file <- NULL
  if(!is.null(datasource)){
    datasource_name <- unlist(strsplit(datasource, "\\."))[1]
    datasource_file <- attr(datasource, "uri")
    attributes(datasource) <- NULL
  }else{
    if(entity$data$upload){
      errMsg <- sprintf("Upload source is missing!")
      stop(errMsg)
    }
  }
  
  #layername/sourcename
  layername <- if(!is.null(entity$data$layername)) entity$data$layername else entity$identifiers$id
  
  #shortcut for gs config
  GS_CONFIG <- config$software$output$geoserver_config
  GS <- config$software$output$geoserver
  if(is.null(GS)){
    errMsg <- "This action requires a GeoServer software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  workspace <- GS_CONFIG$properties$workspace
  if(is.null(workspace)) if(!is.null(entity$data$workspaces$geoserver)) workspace <- entity$data$workspaces$geoserver
  if(is.null(workspace)){
    errMsg <- "The geoserver configuration requires a workspace for publishing action"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  store <- GS_CONFIG$properties$store
  if(is.null(store)) if(!is.null(entity$data$store)) store <- entity$data$store
  if(is.null(store)){
    errMsg <- "The geoserver configuration requires a data/coverage store for publishing action"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  if(entity$data$uploadType == "other"){
    warnMsg <- "No 'geosapi' action possible for type 'other'. Action skipped"
    config$logger.warn(warnMsg)
    return(NULL)
  }
  
  # Check existence of data/coverage store
  the_store <- switch(entity$data$spatialRepresentationType,
                      "vector" = GS$getDataStore(workspace, store),
                      "grid" = GS$getCoverageStore(workspace, store)
  )
  # If store does not exist
  # Check if createStore is TRUE
  if(length(the_store)==0){
    if(createStore){
      switch(entity$data$uploadType,
             #vector/features upload types
             #===========================================================================================
             #vector/GeoPackage
             #-------------------------------------------------------------------------------------------
             "gpkg"= {
               the_store<-GSGeoPackageDataStore$new(
                 name = store, 
                 description = store_description , 
                 enabled = TRUE, 
                 database = paste0("file://data/",workspace,"/",entity$data$uploadSource,".gpkg")
               )
             },
             #vector/dbtable
             #-------------------------------------------------------------------------------------------
             "dbtable"= {
               dbi<-config$software$output$dbi_config
               if(is.null(dbi)) dbi<-config$software$output$dbi_config
               if(is.null(dbi)) {
                 errMsg <- sprintf("Error during Geoserver '%s' datastore creation, this datastore type requires a DBI type software declaration in the configuration", store)
                 config$logger.error(errMsg)
                 stop(errMsg)   
               }
               Postgres<-dbi$parameters$drv %in% c("Postgres","PostreSQL")
               if(!Postgres){
                 errMsg <- sprintf("Error during Geoserver '%s' datastore creation, the DBI software declared in the configuration is not a PostGis database", store)
                 config$logger.error(errMsg)
                 stop(errMsg)   
               }
               the_store<-GSPostGISDataStore$new(name=store, description = store_description, enabled = TRUE)
               the_store$setHost(dbi$parameters$host)
               the_store$setPort(dbi$parameters$port)
               the_store$setDatabase(dbi$parameters$dbname)
               #the_store$setSchema()#Not yet implemented in dbi software arguments
               the_store$setUser(dbi$parameters$user)
               the_store$setPassword(dbi$parameters$password)
             },
             #vector/dbquery
             #-------------------------------------------------------------------------------------------
             "dbquery"= {
               dbi<-config$software$output$dbi_config
               if(is.null(dbi)) dbi<-config$software$output$dbi_config
               if(is.null(dbi)) {
                 errMsg <- sprintf("Error during Geoserver '%s' datastore creation, this datastore type requires a DBI type software declaration in the configuration", store)
                 config$logger.error(errMsg)
                 stop(errMsg)   
               }
               Postgres<-dbi$parameters$drv %in% c("Postgres","PostreSQL")
               if(!Postgres){
                 errMsg <- sprintf("Error during Geoserver '%s' datastore creation, the DBI software declared in the configuration is not a PostGis database", store)
                 config$logger.error(errMsg)
                 stop(errMsg)   
               }
               the_store<-GSPostGISDataStore$new(name=store, description = store_description, enabled = TRUE)
               the_store$setHost(dbi$parameters$host)
               the_store$setPort(dbi$parameters$port)
               the_store$setDatabase(dbi$parameters$dbname)
               #the_store$setSchema()#Not yet implemented in dbi software arguments
               the_store$setUser(dbi$parameters$user)
               the_store$setPassword(dbi$parameters$password)
             },
             #vector/shapefile (ESRI)
             #-------------------------------------------------------------------------------------------
             "shp"= {
               the_store <- GSShapefileDirectoryDataStore$new(
                 name=store, 
                 description = store_description,
                 enabled = TRUE,
                 url = paste0("file://data","/",workspace)
               )
             },
             #grid/coverages upload types
             #-----------------------------------------------
             "geotiff" = {
               the_store <- GSGeoTIFFCoverageStore$new(name = store, description = store_description, enabled = TRUE)
             }
      )
      if(is.null(the_store)){
        errMsg <- sprintf("Error during Geoserver data/coverage store creation, format '%s' not supported. Aborting 'geosapi' action!",entity$data$uploadType)
        config$logger.error(errMsg)
        stop(errMsg)      
      }else{
        created <- switch(entity$data$spatialRepresentationType,
                          "vector" = GS$createDataStore(workspace, the_store),
                          "grid" = GS$createCoverageStore(workspace, the_store)
        )
        if(created){
          infoMsg <- sprintf("Successful Geoserver '%s' data/coverage store creaction", store)
          config$logger.info(infoMsg)
        }else{
          errMsg <- "Error during Geoserver data/coverage store creation. Aborting 'geosapi' action!"
          config$logger.error(errMsg)
          stop(errMsg)
        }
      }
    }else{
      # If createStore is FALSE edit ERROR Message
      errMsg <- sprintf("Data/Coverage store '%s' does not exist and 'createStore' option = FALSE, please verify config if data/coverage store already exists or change createStore = TRUE to create it",store)
      config$logger.error(errMsg)
      stop(errMsg)
    }    
  }
  
  #upload
  #-------------------------------------------------------------------------------------------------
  if(entity$data$upload){
    
    config$logger.info("Upload mode is set to true")
    if(startsWith(entity$data$uploadType,"db") || entity$data$uploadType == "other"){
      warnMsg <- "Skipping upload: Upload mode is only valid for types 'shp', 'spatialite' or 'h2'"
      config$logger.warn(warnMsg)
    }else{
      uploaded <- FALSE
      config$logger.info("Upload from local file(s)")
      filepath <- file.path(getwd(), "data", datasource)
      config$logger.info(sprintf("File to upload to Geoserver: %s", filepath))
      if(file.exists(filepath)){
        config$logger.info(sprintf("Upload file '%s' [%s] to GeoServer...", filepath, entity$data$uploadType))
        uploaded <- switch(entity$data$spatialRepresentationType,
                           #vector/features upload
                           "vector" = GS$uploadData(
                             workspace, store, endpoint = "file", configure = "none", update = "overwrite",
                             filename = filepath, extension = entity$data$uploadType, charset = "UTF-8",
                             contentType = if(entity$data$uploadType=="spatialite") "application/x-sqlite3" else ""
                           ),
                           #grid/coverages upload
                           "grid" = GS$uploadCoverage(
                             workspace, store, endpoint = "file", configure = "none", update = "overwrite",
                             filename = filepath, extension = entity$data$uploadType,
                             contentType = switch(entity$data$uploadType,
                                                  "geotiff" = "text/plain",
                                                  "arcgrid" = "text/plain",
                                                  "worldimage" = "application/zip",
                                                  "imagemosaic" = "application/zip"
                             )
                           )
        )
      }else{
        errMsg <- sprintf("Upload from local file(s): no zipped file found for source '%s' (%s)", filepath, datasource)
        config$logger.error(errMsg)
        stop(errMsg)
      }
      
      if(uploaded){
        infoMsg <- sprintf("Successful Geoserver upload for file '%s' (%s)", datasource_file, entity$data$uploadType)
        config$logger.info(infoMsg)
      }else{
        errMsg <- "Error during Geoserver file upload. Aborting 'geosapi' action!"
        config$logger.error(errMsg)
        stop(errMsg)
      }
    }
  }
  
  #featuretype/coverage  +layer publication
  #--------------------------------------------------------------------------------------------------
  
  #variables
  epsgCode <- sprintf("EPSG:%s", entity$srid)
  
  #build resource (either featuretype or coverage)
  resource <- switch(entity$data$spatialRepresentationType,
                     "vector" = GSFeatureType$new(),
                     "grid" = GSCoverage$new()
  )
  resource$setName(layername)
  nativename <- datasource_name
  if(entity$data$uploadType == "dbquery") nativename <- layername
  if(entity$data$spatialRepresentationType == "grid") nativename <- store
  resource$setNativeName(nativename)
  resource$setAbstract(entity$descriptions$abstract)
  resource$setTitle(entity$titles[["title"]])
  resource$setSrs(epsgCode)
  resource$setNativeCRS(epsgCode)
  resource$setEnabled(TRUE)
  resource$setProjectionPolicy("FORCE_DECLARED")
  bbox <- entity$spatial_bbox
  resource$setNativeBoundingBox(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, crs = epsgCode)
  sfc_min <- sf::st_sfc(sf::st_point(c(bbox$xmin, bbox$ymin)), crs = epsgCode)
  sfc_max <- sf::st_sfc(sf::st_point(c(bbox$xmax, bbox$ymax)), crs = epsgCode)
  sfc_min_ll <- sf::st_bbox(sf::st_transform(sfc_min, crs = 4326))
  sfc_max_ll <- sf::st_bbox(sf::st_transform(sfc_max, crs = 4326))
  resource$setLatLonBoundingBox(sfc_min_ll$xmin, sfc_min_ll$ymin, sfc_max_ll$xmax, sfc_max_ll$ymax, crs = 4326)
  for(subject in entity$subjects){
    kwds <- subject$keywords
    for(kwd in kwds) resource$addKeyword(kwd$name)
  }
  
  #add metadata links
  #in case (only if) geoflow defines either CSW or Geonetwork software, we can add metadata links
  md_link_xml <- NULL
  md_link_html <- NULL
  if(!is.null(config$software$output$csw)|!is.null(config$software$output$geonetwork)){
    if(!is.null(config$software$output$csw)){
      md_link_xml <- paste0(config$software$output$csw_config$parameters$url, "?service=CSW&request=GetRecordById&Version=", config$software$output$csw_config$parameters$version,
                            "&elementSetName=full&outputSchema=http%3A//www.isotc211.org/2005/gmd&id=", entity$identifiers[["id"]])
    }
    if(!is.null(config$software$output$geonetwork)){
      md_link_xml <- paste0(config$software$output$geonetwork_config$parameters$url, "/srv/eng/csw?service=CSW&request=GetRecordById&Version=2.0.2",
                            "&elementSetName=full&outputSchema=http%3A//www.isotc211.org/2005/gmd&id=", entity$identifiers[["id"]])
      if(startsWith(config$software$output$geonetwork_config$parameters$version, "2")){
        md_link_html <- paste0(config$software$output$geonetwork_config$parameters$url, "/srv/en/main.home?uuid=", entity$identifiers[["id"]])
      }else if(startsWith(config$software$output$geonetwork_config$parameters$version, "3")){
        md_link_html <- paste0(config$software$output$geonetwork_config$parameters$url, "/srv/eng/catalog.search#/metadata/", entity$identifiers[["id"]])
      }
    }
  }
  if(!is.null(md_link_xml)){
    md_xml <- GSMetadataLink$new(type = "text/xml", metadataType = "ISO19115:2003", content = md_link_xml)
    resource$addMetadataLink(md_xml)
  }
  if(!is.null(md_link_html)){
    md_html <- GSMetadataLink$new(type = "text/html", metadataType = "ISO19115:2003", content = md_link_html)
    resource$addMetadataLink(md_html)
  }
  
  #resource type specific properties
  switch(entity$data$spatialRepresentationType,
         "vector" = {
           #cql filter?
           if(!is.null(entity$data$cqlfilter)){
             resource$setCqlFilter(entity$data$cqlfilter)
           }
           
           #virtual table?
           if(entity$data$uploadType == "dbquery"){
             vt <- GSVirtualTable$new()
             vt$setName(layername)
             vt$setSql(entity$data$sql)
             #if the virtual table is spatialized
             if(!is.null(entity$data$geometryField) & !is.null(entity$data$geometryType)){
               vtg <- GSVirtualTableGeometry$new(
                 name = entity$data$geometryField, 
                 type = entity$data$geometryType, 
                 srid = entity$srid
               )
               vt$setGeometry(vtg)
             }
             #if the virtual table has service parameters
             if(length(entity$data$parameters)>0){
               for(param in entity$data$parameters){
                 vtp <- GSVirtualTableParameter$new(
                   name = param$name, 
                   defaultValue = param$defaultvalue, 
                   regexpValidator = param$regexp
                 )
                 vt$addParameter(vtp)
               }
             }
             resource$setVirtualTable(vt)
           }
         },
         "grid" = {
           
           #coverage view?
           if(length(entity$data$bands)>0){
             coview <- GSCoverageView$new()
             coview$setName(layername)
             coview$setEnvelopeCompositionType(entity$data$envelopeCompositionType)
             coview$setSelectedResolution(entity$data$selectedResolution)
             coview$setSelectedResolutionIndex(entity$data$selectedResolutionIndex)
             for(band in entity$data$bands){
               cvb <- GSCoverageBand$new()
               covname <- if(!is.null(band$name)) band$name else layername
               cvb$setDefinition(paste0(covname,"@", band$index))
               cvb$setIndex(band$index)
               cvb$addInputBand(GSInputCoverageBand$new( coverageName = covname, band = band$index))
               coview$addBand(cvb)
             }
             resource$setView(coview)
           }else{
             #check nb of bands, if > 3 we configure a coverage view
             bands <- names(entity$data$coverages)
             if(length(bands)>3){
               coview <- GSCoverageView$new()
               coview$setName(layername)
               ect <- entity$data$envelopeCompositionType
               if(is.null(ect)) ect <- "INTERSECTION"
               coview$setEnvelopeCompositionType(ect)
               sr <- entity$data$selectedResolution
               if(is.null(sr)) sr <- "BEST"
               coview$setSelectedResolution(sr)
               sri <- entity$data$selectedResolutionIndex
               if(is.null(sri)) sri <- -1
               coview$setSelectedResolutionIndex(sri)
               for(i in 1:length(bands)){
                 band <- bands[i]
                 cvb <- GSCoverageBand$new()
                 covname <- layername
                 cvb$setDefinition(paste0(covname,"@", i-1))
                 cvb$setIndex(i-1)
                 cvb$addInputBand(GSInputCoverageBand$new( coverageName = covname, band = i-1))
                 coview$addBand(cvb)
               }
               resource$setView(coview)
             }
           }
           
         }
  )
  
  #styles publication if needed
  gs_styles <- GS$getStyleNames()
  if(entity$data$styleUpload) if(length(entity$data$styles)>0){
    for(i in 1:length(entity$data$styles)){
      style <- entity$data$styles[i]
      #check if any style SLD file is available in source
      style_sldfile <- paste0(style,".sld")
      if(!style %in% gs_styles){
        config$logger.warn(sprintf("No style '%s' in Geoserver", style))
        if(style_sldfile %in% entity$data$source){
          config$logger.info(sprintf("Creating GeoServer style '%s' from SLD style file '%s' available as source", style, style_sldfile))
          created <- GS$createStyle(file = file.path(getwd(), "data", style_sldfile), name = style)
        }
      }
    }
    GS$reload()
  }
  
  #layer build and publication
  switch(entity$data$spatialRepresentationType,
         "vector" = {
           layer <- GSLayer$new()
           layer$setName(layername)
           if(length(entity$data$styles)>0){
             for(i in 1:length(entity$data$styles)){
               style <- entity$data$styles[[i]]
               if(i==1) layer$setDefaultStyle(style) else layer$addStyle(style)
             }
           }else{
             layer$setDefaultStyle("generic")
           }
           
           #publish
           try(GS$unpublishLayer(workspace, store, layername))
           out <- GS$publishLayer(workspace, store, resource, layer)
           if(!out){
             errMsg <- sprintf("Error during layer '%s' publication for entity '%s'!",layername, entity$identifiers[["id"]])
             config$logger.error(errMsg)
           }else{
             infoMsg <- sprintf("Successful layer'%s' publication in Geoserver for entity '%s'!", layername, entity$identifiers[["id"]])
           }
         },
         "grid" = {
           out <- FALSE
           cov <- GS$getCoverage(ws = workspace, cs = store, cv = layername)
           if(is.null(cov)){
             out <- GS$createCoverage(ws = workspace, cs = store, coverage = resource)
           }else{
             out <- GS$updateCoverage(ws = workspace, cs = store, coverage = resource)
           }
           #manage coverage styles by updating associated layer object
           layer <- GS$getLayer(layername)
           if(is(layer, "GSLayer")){
             layer$setName(layername)
             if(length(entity$data$styles)>0){
               layer$styles <- list()
               for(i in 1:length(entity$data$styles)){
                 style <- entity$data$styles[[i]]
                 if(i==1) layer$setDefaultStyle(style) else layer$addStyle(style)
               }
             }else{
               layer$setDefaultStyle("generic")
             }
             GS$updateLayer(layer)  
           }
           
           if(!out){
             errMsg <- sprintf("Error during layer '%s' publication for entity '%s'!",layername, entity$identifiers[["id"]])
             config$logger.error(errMsg)
           }else{
             infoMsg <- sprintf("Successful layer'%s' publication in Geoserver for entity '%s'!", layername, entity$identifiers[["id"]])
           }
         }
  )
  
  
}