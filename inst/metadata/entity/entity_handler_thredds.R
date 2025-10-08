#handle_entities_thredds
handle_entities_thredds <- function(handler, source, config){
  
  if(!requireNamespace("thredds", quietly = TRUE)){
    stop("The Thredds handler requires the 'thredds' package")
  }
  if(!requireNamespace("ncdf4", quietly = TRUE)){
    stop("The Thredds handler requires the 'ncdf4' package")
  }
  if(!requireNamespace("XML", quietly = TRUE)){
    stop("The Thredds handler requires the 'XML' package")
  }
  
  thredds <- config$software$input$thredds
  if(is.null(thredds)){
    stop("There is no database input software configured to handle entities from Thredds")
  }
  
  if(length(thredds$get_dataset_names())==0) if(length(thredds$get_dataset_names(xpath=".//d1:dataset"))==0) {
    errMsg <- sprintf("No datasets for the thredds")
    config$logger$ERROR(errMsg)
    stop(errMsg)
  }
  
  if(!is.null(source)) if(source != ""){
    datasets<-unlist(strsplit(source,","))
  }else{
    datasets<- thredds$get_dataset_names()
    if(is.null(datasets)) datasets <- thredds$get_dataset_names(xpath=".//d1:dataset")
  }
  
  
  uri<-XML::parseURI(thredds$url)
  base_uri<-paste0(uri$scheme,"://",uri$server)
  config$logger$INFO("Thredds Base URL: %s", base_uri)
  
  entities<-list()
  entities<- lapply(datasets, function(dataset){
    data<-thredds$get_datasets(dataset)[[dataset]]
    if(is.null(data)) data<-thredds$get_datasets(dataset,xpath=".//d1:dataset")[[dataset]]
    config$logger$INFO("Build entity for '%s'", data$url)
    
    #entity
    
    odap<-unlist(sapply(names(thredds$list_services()), function(x) if(thredds$list_services()[[x]]["serviceType"]=="OPENDAP") thredds$list_services()[[x]]["base"]))[1]
    if(!is.null(odap)) odap_uri<-paste0(base_uri,odap,data$url) else odap_uri<-NULL
    
    ncml<-unlist(sapply(names(thredds$list_services()), function(x) if(thredds$list_services()[[x]]["serviceType"]=="NCML") thredds$list_services()[[x]]["base"]))[1]
    if(!is.null(ncml)) ncml_uri<-paste0(base_uri,ncml,data$url) else ncml_uri<-NULL
    
    if(!is.null(ncml_uri)){
      config$logger$INFO("NCML URL for '%s': %s", data$url, ncml_uri)
      entity <- handle_entities_ncml(handler,ncml_uri,config)[[1]]
    } else if (!is.null(odap_uri)){
      config$logger$INFO("OpenDAP URL for '%s': %s", data$url, odap_uri)
      entity <- handle_entities_ncdf(handler,odap_uri,config)[[1]]
    }else{
      errMsg <- sprintf("No OpenDAP or NCML service for Thredds '%s'", thredds$url)
      config$logger$ERROR(errMsg)
      stop(errMsg)
    }
    #relations
    
    #Thredds
    new_thredds_link <- geoflow::geoflow_relation$new()
    new_thredds_link$setKey("http")
    new_thredds_link$setName(dataset)
    new_thredds_link$setDescription(paste0(entity$titles[["title"]]," - Thredds Catalog"))
    new_thredds_link$setLink(thredds$url)
    entity$addRelation(new_thredds_link)
    
    #data
    http<-unlist(sapply(names(thredds$list_services()), function(x) if(thredds$list_services()[[x]]["serviceType"]=="HTTPServer") thredds$list_services()[[x]]["base"]))[1]
    if(!is.null(http)){
      http_uri<-paste0(base_uri,http,data$url)
      new_data_link <- geoflow::geoflow_relation$new()
      new_data_link$setKey("http")
      new_data_link$setName(dataset)
      new_data_link$setDescription(paste0(entity$titles[["title"]]," - Data"))
      new_data_link$setLink(http_uri)
      entity$addRelation(new_data_link)
    }
    #WMS
    ogc_dimensions<-NULL
    wms<-unlist(sapply(names(thredds$list_services()), function(x) if(thredds$list_services()[[x]]["serviceType"]=="WMS") thredds$list_services()[[x]]["base"]))[1]
    if(!is.null(wms)){
      if(!requireNamespace("ows4R", quietly = TRUE)){
        stop("The Thredds handler requires the 'ows4R' package")
      }
      wms_uri<-paste0(base_uri,wms,data$url,"?service=WMS")
      wms_request<-paste0(base_uri,wms,data$url)
      wms <- ows4R::WMSClient$new(url = wms_request, serviceVersion = "1.3.0", logger = "DEBUG")
      thumbnails<-data.frame(NULL)
      for(layer in wms$getLayers()){
        layername<-layer$getName()  
        if(!is.null(layername)){
          title<-sprintf("Map access - OGC Web Map Service (WMS) - %s", layer$getTitle())
          bbox<-paste(entity$spatial_bbox,collapse=",")
          srs<-if(!is.null(entity$srid))entity$srid else "EPSG:4326"
          style<-layer$getStylenames()[1]
          thumbnail<-sprintf("%s&version=1.1.1&request=GetMap&LAYERS=%s&SRS=%s&BBOX=%s&WIDTH=600&HEIGHT=300&STYLES=%s&FORMAT=image/png&TRANSPARENT=true",
                             wms_uri,layername,srs,bbox,style)
          
          #thumbnail quality
          img_link<-curl::curl_fetch_memory(thumbnail)
          if(img_link$status_code==200){
            img<-png::readPNG(img_link$content)
            
            R = length(unique(as.vector(img[,,1])))
            G = length(unique(as.vector(img[,,2])))
            B = length(unique(as.vector(img[,,3])))
            imgScoring <- data.frame(
              layername = layername,
              title = title,
              link = thumbnail,
              score = sum(c(R,G,B))
            )
            
            thumbnails<-rbind(thumbnails,imgScoring)
          }
          #add wms relation
          new_wms <- geoflow::geoflow_relation$new()
          new_wms$setKey("wms130")
          new_wms$setName(layername)
          new_wms$setDescription(title)
          new_wms$setLink(wms_uri)
          entity$addRelation(new_wms)
        }
      }
      #add prettiest thumbnail relation
      if(nrow(thumbnails)>0){
        thumbnails<-thumbnails[order(thumbnails$score, decreasing = T),]
        for(i in 1:nrow(thumbnails)){
          thumbnail<-thumbnails[i,]
          new_thumbnail <- geoflow::geoflow_relation$new()
          new_thumbnail$setKey("thumbnail")
          new_thumbnail$setName(thumbnail$layername)
          new_thumbnail$setDescription(paste0(thumbnail$title," [",thumbnail$layername,"]", " - Layer Overview",if(i==1)" - Pretty" else ""))
          new_thumbnail$setLink(thumbnail$link)
          entity$addRelation(new_thumbnail)
        }
      }
      ogc_dimensions<-wms$getLayers()[[2]]$getDimensions()#in some case the service layer 1 has no dimension
    }
    
    # #WCS
    # NOT YET IMPLEMENTED
    # wcs<-unlist(sapply(names(thredds$list_services()), function(x) if(thredds$list_services()[[x]]["serviceType"]=="WCS") thredds$list_services()[[x]]["base"]))
    # if(!is.null(wcs)){
    #   wcs_uri<-paste0(base_uri,wcs,data$url,"?service=WCS")
    #   new_wcs <- geoflow::geoflow_relation$new()
    #   new_wcs$setKey("wcs")
    #   new_wcs$setName(layername)
    #   new_wcs$setDescription(entity$titles[["title"]])
    #   new_wcs$setLink(wcs_uri)
    #   entity$addRelation(new_wcs)
    # }
    
    #data
    entity$data$setAccess("thredds")
    entity$data$setSource(dataset)
    entity$data$setSourceType("nc")
    if(!is.null(ogc_dimensions))entity$data$ogc_dimensions<-ogc_dimensions
    
    time<-entity$data$ogc_dimensions$time$values
    if(!is.null(time)){
      values<-as.POSIXct(entity$data$ogc_dimensions$time$values,format ="%Y-%m-%dT%H:%M:%S")
      temporal_cov<- paste(utils::head(sort(values),1), utils::tail(sort(values),1),sep="/")
      entity$setTemporalExtent(temporal_cov)
    }
    
    return(entity)
    
  })
  return(entities)
}
