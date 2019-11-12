geosapi_publish_ogc_services <- function(entity, config, options){
  
  #for the timebeing, this action targets Vector data (featureType)
  #Later this action may also target coverage, but it's not yet supported by geosapi
  
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
  datasource <- entity$data$source[[1]]
  datasource_name <- unlist(strsplit(datasource, "\\."))[1]
  datasource_file <- attr(datasource, "uri")
  attributes(datasource) <- NULL
  
  #layername/sourcename
  layername <- if(!is.null(entity$data$layername)) entity$data$layername else entity$identifiers$id
  sourcename <- if(!is.null(datasource_name)) datasource_name else layername
  basefilename <- paste0(entity$identifiers$id, "_", entity$data$uploadType,"_", layername)
  filename <- paste0(basefilename, ".zip")
  
  #shortcut for gs config
  GS <- config$software$output$geoserver
  if(is.null(GS)){
    errMsg <- "This action requires a GeoServer software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  GS_CONFIG <- config$software$output$geoserver_config
  workspace <- GS_CONFIG$properties$workspace
  if(is.null(workspace)){
    errMsg <- "The geoserver configuration requires a workspace for publishing action"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  datastore <- GS_CONFIG$properties$datastore
  if(is.null(datastore)){
    errMsg <- "The geoserver configuration requires a datastore for publishing action"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #check presence of workspace/datastore local config
  if(!is.null(entity$data$workspace)) workspace <- entity$data$workspace
  if(!is.null(entity$data$datastore)) datastore <- entity$data$datastore
  
  if(entity$data$uploadType == "other"){
    warnMsg <- "No 'geosapi' action possible for type 'other'. Action skipped"
    config$logger.warn(warnMsg)
    return(NULL)
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
      filepath <- file.path(getwd(), "data", filename)
      if(file.exists(filepath)){
        config$logger.info(sprintf("Upload file '%s' [%s] to GeoServer...", filepath, entity$data$uploadType))
        uploaded <- GS$uploadData(workspace, datastore, endpoint = "file", configure = "none", update = "overwrite",
                                  filename = filepath, extension = entity$data$uploadType, charset = "UTF-8",
                                  contentType = if(entity$data$uploadType=="spatialite") "application/x-sqlite3" else "")
        }else{
          errMsg <- sprintf("Upload from local file(s): no zipped file found for source '%s' (%s)", filepath, sourcename)
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
  
  #featuretype/layer publication
  #--------------------------------------------------------------------------------------------------
  
  #variables
  epsgCode <- sprintf("EPSG:%s", entity$srid)
  
  #build feature type
  featureType <- GSFeatureType$new()
  featureType$setName(layername)
  nativename <- sourcename
  if(entity$data$upload) nativename <- basefilename
  if(entity$data$uploadType == "dbquery") nativename <- layername
  featureType$setNativeName(nativename)
  featureType$setAbstract(entity$descriptions$abstract)
  featureType$setTitle(entity$title)
  featureType$setSrs(epsgCode)
  featureType$setNativeCRS(epsgCode)
  featureType$setEnabled(TRUE)
  featureType$setProjectionPolicy("FORCE_DECLARED")
  bbox <- entity$spatial_bbox
  featureType$setLatLonBoundingBox(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, crs = epsgCode)
  featureType$setNativeBoundingBox(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, crs = epsgCode) 
  for(subject in entity$subjects){
    kwds <- subject$keywords
    for(kwd in kwds) featureType$addKeyword(kwd$name)
  }
  
  #cql filter?
  if(!is.null(entity$data$cqlfilter)){
    featureType$setCqlFilter(entity$data$cqlfilter)
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
    if(length(entity$data$parameters)){
      for(param in entity$data$parameters){
        vtp <- GSVirtualTableParameter$new(
          name = param$name, 
          defaultValue = param$defaultvalue, 
          regexpValidator = param$regexp
        )
        vt$addParameter(vtp)
      }
    }
    featureType$setVirtualTable(vt)
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
    featureType$addMetadataLink(md_xml)
  }
  if(!is.null(md_link_html)){
    md_html <- GSMetadataLink$new(type = "text/html", metadataType = "ISO19115:2003", content = md_link_html)
    featureType$addMetadataLink(md_html)
  }
  
  #build layer
  layer <- GSLayer$new()
  layer$setName(layername)
  for(i in 1:length(entity$data$styles)){
    style <- entity$data$styles[i]
    if(i==1) layer$setDefaultStyle(style) else layer$addStyle(style)
  }
  
  #publish
  try(GS$unpublishLayer(workspace, datastore, layername))
  out <- GS$publishLayer(workspace, datastore, featureType, layer)
  if(!out){
    errMsg <- sprintf("Error during layer '%s' publication for entity '%s'!",layername, entity$identifiers[["id"]])
    config$logger.error(errMsg)
  }else{
    infoMsg <- sprintf("Successful layer'%s' publication in Geoserver for entity '%s'!", layername, entity$identifiers[["id"]])
  }
  
}