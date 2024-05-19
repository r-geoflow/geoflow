#handle_entities_ncml
handle_entities_ncml <- function(handler, source, config, handle = TRUE){
  
  config$logger.info("NCML Handle")
  
  if(!requireNamespace("XML", quietly = TRUE)){
    stop("The NCML handler requires the 'XML' package")
  }
  
  getNCML <- function(file){
    
    xml <- XML::xmlParse(httr::content(httr::GET(file),"text"))
    
    ncml_str <- XML::xmlChildren(XML::xmlChildren(xml)[[1]])
    
    #global attributes
    global_attributes_xml <- ncml_str[names(ncml_str) == "attribute"]
    global_attributes_names <- sapply(global_attributes_xml, XML::xmlGetAttr, "name")
    global_attributes <- lapply(global_attributes_xml, function(x){
      out_attr <- list(value = XML::xmlGetAttr(x,"value"), type = XML::xmlGetAttr(x, "type"))
      return(out_attr)
    })
    names(global_attributes) <- global_attributes_names
    
    
    #global attributes (in groups)
    global_attributes_ingroup_xml <- ncml_str[names(ncml_str) == "group"]
    global_attributes_ingroup <- sapply(global_attributes_ingroup_xml, function(x){
      group_attrs_xml <- XML::xmlChildren(x)
      group_attrs_names <- sapply(group_attrs_xml, XML::xmlGetAttr, "name")
      group_attrs <- lapply(group_attrs_xml, function(node){
        out_attr <- list(value = XML::xmlGetAttr(node, "value"), type = XML::xmlGetAttr(node, "type"))
        return(out_attr)
      })
      names(group_attrs) <- group_attrs_names
      return(group_attrs)
    })
    names(global_attributes_ingroup) <- NULL
    global_attributes_ingroup <- do.call("c", global_attributes_ingroup)
    
    global_attributes <- c(global_attributes, global_attributes_ingroup)
    
    #dimensions
    dimensions_xml <- ncml_str[names(ncml_str)=="dimension"]
    dimensions_names <- sapply(dimensions_xml, XML::xmlGetAttr, "name")
    dimensions <- lapply(dimensions_xml, function(x){
      out_dim <- list(name = XML::xmlGetAttr(x, "value"), length = XML::xmlGetAttr(x, "length"))
      return(out_dim)
    })
    names(dimensions) <- dimensions_names
    
    #variables
    variables_xml <- ncml_str[names(ncml_str)=="variable"]
    variables_names <- sapply(variables_xml, XML::xmlGetAttr, "name")
    variables <- lapply(variables_xml, function(x){
      
      out_var <- list(
        name = XML::xmlGetAttr(x, "name"),
        shape = XML::xmlGetAttr(x, "shape"),
        type = XML::xmlGetAttr(x, "type")
      )
      var_attributes_xml <- XML::xmlChildren(x)
      var_attributes_names <- sapply(var_attributes_xml, XML::xmlGetAttr, "name")
      var_attributes <- lapply(var_attributes_xml, function(x){
        out_var_attr <- list(value = XML::xmlGetAttr(x,"value"), type = XML::xmlGetAttr(x, "type"))
        return(out_var_attr)
      })
      names(var_attributes) <- var_attributes_names
      out_var$attributes <- var_attributes
      return(out_var)
    })
    names(variables) <- variables_names
    
    out_ncml <- list(
      attributes = global_attributes,
      dimensions = dimensions,
      variables = variables
    )
    return(out_ncml)
  }
  
  entities<-list()
  entity <- geoflow::geoflow_entity$new()
  source_name<-source
  source <- getNCML(source)
  if(!handle) return(source)
  
  #list attributes of source
  attr<-source$attributes
  
  
  #identifiers
  entity$setIdentifier("id", basename(attr$id$value))
  
  doi <- attr$identifier_product_doi$value
  if(!is.null(doi)){
    entity$setIdentifier("doi", doi)
  }
  
  #title
  title <- attr$title$value
  if(!is.null(title)){
    entity$setTitle("title", title)
  }else{
    entity$setTitle("title",basename(attr$id$value))  
  }
  
  #description
  summary <- attr$summary$value
  if(!is.null(summary)){
    entity$setDescription("abstract", summary)
    #entity$setDescription("abstract","Test")
    
  }
  
  edition <- attr$product_version$value
  if(!is.null(edition)){
    entity$setDescription("edition", edition)
  }
  
  credit <- attr$credit$value
  if(!is.null(credit)){
    entity$setDescription("credit", attr$credit$value)
  }
  
  #subjects
  for(subject in c("keywords","instrument","platform","project","product_name","institution")){
    if(subject %in% names(attr)){
      keywords<-paste0(unique(unlist(strsplit(attr[[subject]]$value,";"))),collapse=",")
      key<-switch(subject,
                  "keywords"="theme",
                  "instrument"="instrument",
                  "platform"="platform",
                  "project"="project",
                  "product_name"="product",
                  "institution"="dataCenter"
      )
      thesaurus<-if(paste0(subject,"_vocabulary") %in% names(attr)){attr[paste0(subject,"_vocabulary")]$value }else{""}
      subject_obj <- geoflow::geoflow_subject$new(str = paste0(key,"[",thesaurus,"]:",keywords))
      entity$addSubject(subject_obj)
    }
  }
  
  #contacts
  if(!is.null(attr$creator_name$value)){
    contact_obj <- geoflow::geoflow_contact$new()
    contact_obj$setRole("owner")
    contact_obj$setLastName(attr$creator_name$value)
    contact_obj$setOrganizationName(attr$creator_institution$value)
    contact_obj$setEmail(attr$creator_email$value)
    contact_obj$setWebsiteUrl(attr$creator_url$value)
    entity$addContact(contact_obj) 
    
    contact_obj <- geoflow::geoflow_contact$new()
    contact_obj$setRole("metadata")
    contact_obj$setLastName(attr$creator_name$value)
    contact_obj$setOrganizationName(attr$creator_institution$value)
    contact_obj$setEmail(attr$creator_email$value)
    contact_obj$setWebsiteUrl(attr$creator_url$value)
    entity$addContact(contact_obj) 
  }
  
  if(!is.null(attr$publisher_name)){
    contact_obj <- geoflow::geoflow_contact$new()
    contact_obj$setRole("publisher")
    contact_obj$setLastName(attr$publisher_name$value)
    contact_obj$setOrganizationName(attr$publisher_institution$value)
    contact_obj$setEmail(attr$publisher_email$value)
    contact_obj$setWebsiteUrl(attr$publisher_url$value)
    entity$addContact(contact_obj) 
  }
  
  #dates
  if(is.null(attr$date_created$value)){entity$addDate("creation", Sys.time())} else if(!"try-error" %in% class(try(as.POSIXlt.character(attr$date_created$value)))){entity$addDate("creation", attr$date_created$value)} else {entity$addDate("creation", Sys.time())}
  if(!is.null(attr$date_modified$value)) if(!"try-error" %in% class(try(as.POSIXlt.character(attr$date_modified$value))))entity$addDate("revision", attr$date_modified$value)
  if(!is.null(attr$date_metadata_modified$value)) if(!"try-error" %in% class(try(as.POSIXlt.character(attr$date_metadata_modified$value))))entity$addDate("metadata", attr$date_metadata_modified$value)
  if(!is.null(attr$date_issued$value)) if(!"try-error" %in% class(try(as.POSIXlt.character(attr$date_issued$value))))entity$addDate("publication", attr$date_issued$value)
  
  
  #Type
  #No information available
  
  #Language
  #No information available
  
  #spatial extent
  # spatial_cov<-if(!is.null(attr$geospatial_bounds)){attr$geospatial_bounds}else{NULL}
  spatial_cov<-NULL
  spatial_srid<-if(!is.null(attr$geospatial_bounds_crs$value)){attr$geospatial_bounds_crs$value}else{NA}
  if(is.null(spatial_cov)){
    if(!is.null(attr$geospatial_lat_min$value)&!is.null(attr$geospatial_lat_max$value)&!is.null(attr$geospatial_lon_min$value)&!is.null(attr$geospatial_lon_max$value)){
      spatial_cov<-paste0("POLYGON((",attr$geospatial_lon_min$value," ",attr$geospatial_lat_min$value,",",attr$geospatial_lon_min$value," ",attr$geospatial_lat_max$value,",",attr$geospatial_lon_max$value," ",attr$geospatial_lat_max$value,",",attr$geospatial_lon_max$value," ",attr$geospatial_lat_min$value,",",attr$geospatial_lon_min$value," ",attr$geospatial_lat_min$value,"))")
    }
  }
  if(!is.null(spatial_cov)) entity$setSpatialExtent(spatial_cov, crs = spatial_srid)
  
  #temporal extent
  if(!is.null(attr$time_coverage_start$value)&!is.null(attr$time_coverage_end$value)){
    if(!"try-error" %in% class(try(as.POSIXlt.character(attr$time_coverage_start$value))))if(!"try-error" %in% class(try(as.POSIXlt.character(attr$time_coverage_end$value)))){
      temporal_cov<- paste(attr$time_coverage_start$value,attr$time_coverage_end$value,sep="/")
      entity$setTemporalExtent(temporal_cov)
    }
  }
  
  #relation
  #No information available
  
  #rights
  if(!is.null(attr$license$value)){
    right_obj <- geoflow::geoflow_right$new(str = paste0("license:",attr$license$value))
    entity$addRight(right_obj)
  }
  
  #formats
  format_obj <- geoflow::geoflow_format$new(str = "resource:application/x-netcdf")
  entity$addFormat(format_obj)
  
  #provenance
  #No information available 
  
  #data
  data_obj <- geoflow::geoflow_data$new()
  
  #variables
  variables = lapply(setdiff(names(source$variables),names(source$dimensions)), function(x){
    #TODO in future create a geoflow::geoflow_variable class to handle more info (eg. min/max, type, domain eg. physicalMeasurement, etc)
    var = source$var[[x]]
    outvar <- x
    var_attrs <- var$attributes
    name <- var_attrs$long_name$value
    if(!is.null(var_attrs$standard_name$value)) name <- paste0(name, " (", var_attrs$standard_name$value, ")")
    attr(outvar, "description") <- name
    attr(outvar, "units") <- gsub(" ","_",var_attrs$units$value)
    return(outvar)
  })
  
  data_obj$setVariables(variables)
  
  #dimensions
  for(dim in names(source$dimensions)){
    
    if(startsWith(dim,"lat")){#row
      name="row"
      resolution = list(
        uom=if(!is.null(attr$geospatial_lat_units$value)) attr$geospatial_lat_units$value else source$variables[[dim]]$attributes$units$value,
        value = if(!is.null(attr$geospatial_lat_resolution$value)){if(is.numeric(attr$geospatial_lat_resolution$value)){attr$geospatial_lat_resolution$value}else{unlist(strsplit(attr$geospatial_lat_resolution$value," "))[1]}}else{NULL}
      )
    }else if(startsWith(dim,"lon")){#column
      name="column" 
      resolution = list(
        uom=if(!is.null(attr$geospatial_lon_units$value)) attr$geospatial_lon_units$value else source$variables[[dim]]$attributes$units$value,
        value = if(!is.null(attr$geospatial_lon_resolution$value)){if(is.numeric(attr$geospatial_lon_resolution$value)){attr$geospatial_lon_resolution$value}else{unlist(strsplit(attr$geospatial_lon_resolution$value," "))[1]}}else{NULL}
      )
    }else if(startsWith(dim,"time")){#time
      name="time"
      if(!is.null(attr$time_coverage_resolution$value)){
        duration<-attr$time_coverage_resolution$value
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
      }else if(!is.null(attr$temporal_range$value)){
        duration<-attr$temporal_range$value
        resolution=list(
          uom=gsub("[^[:alpha:]]", "", duration),
          value=if(gsub("\\D", "", duration)!=""){gsub("\\D", "", duration)}else{"1"}
        )
      }else if(!is.null(source$variables[[dim]]$attributes$units$value)){
        resolution=list(uom=unlist(strsplit(source$variables[[dim]]$attributes$units$value," "))[1],value=NULL)
      }else{
        resolution=list(uom=NULL,value=NULL)
      }
    }else if(startsWith(dim,"z")|startsWith(dim,"alt")){#vertical
      name="vertical" 
      resolution = list(
        uom=attr$geospatial_vertical_units$value,
        value = if(!is.null(attr$geospatial_vertical_resolution$value)) as.numeric(gsub("\\D","",attr$geospatial_vertical_resolution$value)) else NULL
      )
    }else{
      name=dim
      resolution = list(
        uom=source$variables[[dim]]$attributes$units$value,
        value=NULL
      )
    }
    dimension_obj<-geoflow::geoflow_dimension$new()
    dimension_obj$setLongName(source$variables[[dim]]$attributes$long_name$value)
    dimension_obj$setMinValue(source$variables[[dim]]$attributes$valid_min$value)
    dimension_obj$setMaxValue(source$variables[[dim]]$attributes$valid_max$value)
    dimension_obj$setResolution(uom = resolution$uom,value=resolution$value)
    dimension_obj$setSize(source$dimensions[[dim]]$length)
    dimension_obj$setValues(source$dim[[dim]]$vals)
    
    data_obj$addDimension(name,dimension_obj)
    
  }
  
  #spatialRepresentationType
  if(!is.null(attr$featureType$value)){
    spatialRepresentationType <-tolower(attr$featureType$value) 
  }else if(!is.null(attr$cdm_data_type$value)){
    spatialRepresentationType <-tolower(attr$cdm_data_type$value)
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