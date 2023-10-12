#handle_entities_ncdf
handle_entities_ncdf <- function(handler, source, config, handle = TRUE){
  
  config$logger.info("NCDF Handler")
  if(!requireNamespace("ncdf4", quietly = TRUE)){
    stop("The NCDF handler requires the 'ncdf4' package")
  }
  if(!requireNamespace("XML", quietly = TRUE)){
    stop("The NCDF handler requires the 'XML' package")
  }
  
  #if(!mime::guess_type(source)=="application/x-netcdf"){
  #  errMsg <- "Error in 'handle_entities_df': source parameter should be an 'netcdf' file"
  #  config$logger.error(errMsg)
  #  stop(errMsg)
  #}
  entities<-list()
  entity <- geoflow::geoflow_entity$new()
  source_name<-source
  source <- ncdf4::nc_open(source)
  if(!handle) return(source)
  
  #list attributes of source
  attr<-ncdf4::ncatt_get(source,varid=0)
  
  #simplify attributes names 
  for(i in 1:length(attr)){
    names(attr)[i]<-gsub("NC_GLOBAL.","",names(attr)[i])
  }
  
  #identifiers
  # identifier <-attr$id
  # if(!is.null(identifier)){
  #     entity$setIdentifier("id", identifier)
  #   }else{
  entity$setIdentifier("id", basename(source_name))
  # }
  
  doi <- attr$identifier_product_doi
  if(!is.null(doi)){
    entity$setIdentifier("doi", doi)
  }
  
  #title
  title <- attr$title
  if(!is.null(title)){
    entity$setTitle("title", title)
  }else{
    entity$setTitle("title",basename(source_name))  
  }
  
  #description
  summary <- attr$summary
  if(!is.null(summary)){
    entity$setDescription("abstract", summary)
  }
  
  edition <- attr$product_version
  if(!is.null(edition)){
    entity$setDescription("edition", edition)
  }
  
  credit <- attr$credit
  if(!is.null(credit)){
    entity$setDescription("credit", credit)
  }
  
  #subjects
  for(subject in c("keywords","instrument","platform","project","product_name","institution")){
    if(subject %in% names(attr)){
      keywords<-paste0(unique(unlist(strsplit(attr[[subject]],";"))),collapse=",")
      key<-switch(subject,
                  "keywords"="theme",
                  "instrument"="instrument",
                  "platform"="platform",
                  "project"="project",
                  "product_name"="product",
                  "institution"="dataCenter"
      )
      thesaurus<-if(paste0(subject,"_vocabulary") %in% names(attr)){attr[paste0(subject,"_vocabulary")] }else{""}
      subject_obj <- geoflow::geoflow_subject$new(str = paste0(key,"[",thesaurus,"]:",keywords))
      entity$addSubject(subject_obj)
    }
  }
  
  #contacts
  if(!is.null(attr$creator_name)){
    contact_obj <- geoflow::geoflow_contact$new()
    contact_obj$setRole("owner")
    contact_obj$setLastName(attr$creator_name)
    contact_obj$setOrganizationName(attr$creator_institution)
    contact_obj$setEmail(attr$creator_email)
    contact_obj$setWebsiteUrl(attr$creator_url)
    entity$addContact(contact_obj) 
    
    contact_obj <- geoflow::geoflow_contact$new()
    contact_obj$setRole("metadata")
    contact_obj$setLastName(attr$creator_name)
    contact_obj$setOrganizationName(attr$creator_institution)
    contact_obj$setEmail(attr$creator_email)
    contact_obj$setWebsiteUrl(attr$creator_url)
    entity$addContact(contact_obj) 
  }
  
  if(!is.null(attr$publisher_name)){
    contact_obj <- geoflow::geoflow_contact$new()
    contact_obj$setRole("publisher")
    contact_obj$setLastName(attr$publisher_name)
    contact_obj$setOrganizationName(attr$publisher_institution)
    contact_obj$setEmail(attr$publisher_email)
    contact_obj$setWebsiteUrl(attr$publisher_url)
    entity$addContact(contact_obj) 
  }
  
  #dates
  if(is.null(attr$date_created)){entity$addDate("creation", Sys.time())} else if(!"try-error" %in% class(try(as.POSIXlt.character(attr$date_created)))){entity$addDate("creation", attr$date_created)} else {entity$addDate("creation", Sys.time())}
  if(!is.null(attr$date_modified)) if(!"try-error" %in% class(try(as.POSIXlt.character(attr$date_modified))))entity$addDate("revision", attr$date_modified)
  if(!is.null(attr$date_metadata_modified)) if(!"try-error" %in% class(try(as.POSIXlt.character(attr$date_metadata_modified))))entity$addDate("metadata", attr$date_metadata_modified)
  if(!is.null(attr$date_issued)) if(!"try-error" %in% class(try(as.POSIXlt.character(attr$date_issued))))entity$addDate("publication", attr$date_issued)
  
  #Type
  #Not yet implemented
  
  #Language
  #Not yet implemented
  
  #spatial extent
  # spatial_cov<-if(!is.null(attr$geospatial_bounds)){attr$geospatial_bounds}else{NULL}
  spatial_cov<-NULL
  spatial_srid<-if(!is.null(attr$geospatial_bounds_crs)){attr$geospatial_bounds_crs}else{NA}
  if(is.null(spatial_cov)){
    if(!is.null(attr$geospatial_lat_min)&!is.null(attr$geospatial_lat_max)&!is.null(attr$geospatial_lon_min)&!is.null(attr$geospatial_lon_max)){
      spatial_cov<-paste0("POLYGON((",attr$geospatial_lon_min," ",attr$geospatial_lat_min,",",attr$geospatial_lon_min," ",attr$geospatial_lat_max,",",attr$geospatial_lon_max," ",attr$geospatial_lat_max,",",attr$geospatial_lon_max," ",attr$geospatial_lat_min,",",attr$geospatial_lon_min," ",attr$geospatial_lat_min,"))")
    }
  }
  if(!is.null(spatial_cov)) entity$setSpatialExtent(spatial_cov, crs = spatial_srid)
  
  #temporal extent
  if(!is.null(attr$time_coverage_start)&!is.null(attr$time_coverage_end)){
    if(!"try-error" %in% class(try(as.POSIXlt.character(attr$time_coverage_start))))if(!"try-error" %in% class(try(as.POSIXlt.character(attr$time_coverage_end)))){
      temporal_cov<- paste(attr$time_coverage_start,attr$time_coverage_end,sep="/")
      entity$setTemporalExtent(temporal_cov)
    }
  }
  
  #relation
  #Not yet implemented
  
  #rights
  if(!is.null(attr$license)){
    right_obj <- geoflow::geoflow_right$new(str = paste0("license:",attr$license))
    entity$addRight(right_obj)
  }
  
  #formats
  format_obj <- geoflow::geoflow_format$new(str = "resource:application/x-netcdf")
  entity$addFormat(format_obj)
  
  #provenance
  #Not yet implemented
  
  #data
  data_obj <- geoflow::geoflow_data$new()
  
  #variables
  
  variables = lapply(names(source$var), function(x){
    #TODO in future create a geoflow::geoflow_variable class to handle more info (eg. min/max, type, domain eg. physicalMeasurement, etc)
    var = source$var[[x]]
    outvar <- x
    var_attrs <- ncdf4::ncatt_get(source, x)
    name <- var$longname
    if(!is.null(var_attrs$standard_name)) name <- paste0(name, " (", var_attrs$standard_name, ")")
    attr(outvar, "description") <- name
    attr(outvar, "units") <- gsub(" ","_",var_attrs$units)
    return(outvar)
  })
  
  data_obj$setVariables(variables)
  
  #dimensions
  for(dim in names(source$dim)){
    if(length(ncdf4::ncatt_get(source,dim))>0){
      #row
      if(startsWith(source$dim[[dim]]$name,"lat")){
        name="row"
        resolution = list(
          uom=if(!is.null(attr$geospatial_lat_units)) attr$geospatial_lat_units else ncdf4::ncatt_get(source,dim)$units,
          value = if(!is.null(attr$geospatial_lat_resolution)){if(is.numeric(attr$geospatial_lat_resolution)){attr$geospatial_lat_resolution}else{unlist(strsplit(attr$geospatial_lat_resolution," "))[1]}}else{NULL}
        )
      }
      #column
      if(startsWith(source$dim[[dim]]$name,"lon")){
        name="column" 
        resolution = list(
          uom=if(!is.null(attr$geospatial_lon_units)) attr$geospatial_lon_units else ncdf4::ncatt_get(source,dim)$units,
          value = if(!is.null(attr$geospatial_lon_resolution)){if(is.numeric(attr$geospatial_lon_resolution)){attr$geospatial_lon_resolution}else{unlist(strsplit(attr$geospatial_lon_resolution," "))[1]}}else{NULL}
        )
      }
      #time
      if(startsWith(source$dim[[dim]]$name,"time")){
        name="time"
        if(!is.null(attr$time_coverage_resolution)){
          duration<-attr$time_coverage_resolution
          resolution=list(
            uom=
              if(startsWith(duration,"PT")){
                switch(substr(duration, nchar(duration),nchar(duration)),
                       "H"="hour",
                       "M"="minute",
                       "S"="second")
              }else if(startsWith(duration,"P")){
                switch(substr(duration, nchar(duration),nchar(duration)),
                       "Y"="year",
                       "M"="month",
                       "W"="week",
                       "D"="day")    
              }else{""},
            value=gsub("\\D", "", duration)
          )
        }else if(!is.null(attr$temporal_range)){
          duration<-attr$temporal_range
          resolution=list(
            uom=gsub("[^[:alpha:]]", "", duration),
            value=if(gsub("\\D", "", duration)!=""){gsub("\\D", "", duration)}else{"1"}
          )
        }else if(!is.null(ncdf4::ncatt_get(source, dim)$units)){
          resolution=list(uom=unlist(strsplit(ncdf4::ncatt_get(source, dim)$units," "))[1],value=NULL)
        }else{
          resolution=list(uom=NULL,value=NULL)
        }
      }
      #vertical
      if(startsWith(source$dim[[dim]]$name,"z")){
        name="vertical" 
        resolution = list(
          uom=attr$geospatial_vertical_units,
          value = if(!is.null(attr$geospatial_vertical_resolution)) as.numeric(gsub("\\D","",attr$geospatial_vertical_resolution)) else NULL
        )
      }
      dimension_obj<-geoflow::geoflow_dimension$new()
      dimension_obj$setLongName(ncdf4::ncatt_get(source,dim)$long_name)
      dimension_obj$setMinValue(ncdf4::ncatt_get(source,dim)$valid_min)
      dimension_obj$setMaxValue(ncdf4::ncatt_get(source,dim)$valid_max)
      dimension_obj$setResolution(uom = resolution$uom,value=resolution$value)
      dimension_obj$setSize(source$dim[[dim]]$len)
      dimension_obj$setValues(source$dim[[dim]]$vals)
      
      data_obj$addDimension(name,dimension_obj)
    }
  }
  
  #spatialRepresentationType
  if(!is.null(attr$featureType)){
    spatialRepresentationType <-tolower(attr$featureType) 
  }else if(!is.null(attr$cdm_data_type)){
    spatialRepresentationType <-tolower(attr$cdm_data_type)
  }else{
    spatialRepresentationType <-""
  }
  
  if(spatialRepresentationType=="trajectory"){spatialRepresentationType<-"vector"}else{spatialRepresentationType<-"grid"}
  
  
  data_obj$setSpatialRepresentationType(spatialRepresentationType)
  
  if(!startsWith(source_name,"http")){
    #how to deduce a download link from an opendap link (without being on Thredds)
    data_obj$setSource(source_name)
    data_obj$setSourceType("nc")
  }
  entity$setData(data_obj)
  
  entities <- list(entity)
  return(entities)
}