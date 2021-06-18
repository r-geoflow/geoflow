#handle_entities_df
handle_entities_df <- function(config, source){
  
  if(!is(source, "data.frame")){
    errMsg <- "Error in 'handle_entities_df': source parameter should be an object of class 'data.frame'"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  entities <- list()
  rowNum <- nrow(source)
  config$logger.info(sprintf("Parsing %s entities from tabular source", rowNum))
  for(i in 1:rowNum){
    config$logger.info(sprintf("Parsing entity %s", i))
    source_entity <- source[i,]
    entity <- geoflow_entity$new()
    
    #language
    entity$setLanguage(source_entity[,"Language"])
    
    #dates
    src_date <- sanitize_str(as(source_entity[,"Date"], "character"))
    dates <- if(!is.na(src_date)) extract_cell_components(src_date) else list()
    if(length(dates)>0){
      if(length(dates)==1){
        if(regexpr(":",dates) == -1 && nchar(dates)>0){
          entity$dates <- list()
          entity$addDate("creation", dates)
        }else{
          date_kvp <- extract_kvp(dates)
          for(date in date_kvp$values){
            entity$addDate(date_kvp$key, date)
          }
        }
      }else{
        for(date in dates){
          if(regexpr("creation:",date)>0){
            entity$dates <- list()
            date_kvp <- extract_kvp(date)
            for(adate in date_kvp$values){
              entity$addDate(date_kvp$key, adate)
            }
          }else{
            date_kvp <- extract_kvp(date)
            for(adate in date_kvp$values){
              entity$addDate(date_kvp$key, adate)
            }
          }
        }
      }
    }else{
      entity$addDate("creation", Sys.time())
    }
    
    #types
    src_type <- sanitize_str(source_entity[,"Type"])
    types <- if(!is.na(src_type)) extract_cell_components(src_type) else list()
    if(length(types)>0){
      if(length(types)==1){
        entity$setType("generic", types)
      }else{
        for(type in types){
          if(regexpr(":",type) == -1){
            entity$setType("generic", type)
          }else{
            type_kvp <- extract_kvp(type)
            entity$setType(type_kvp$key, type_kvp$values[[1]])
          }
        }
      }
    }
    
    #identifier
    identifiers <-extract_cell_components(sanitize_str(source_entity[,"Identifier"]))
    for(identifier in identifiers){
      if(regexpr(":",identifier) == -1){
        entity$setIdentifier("id", identifier)
      }else{
        id_kvp <- extract_kvp(identifier)
        entity$setIdentifier(id_kvp$key, id_kvp$values[[1]])
      }
    }
    
    #title
    src_title <- sanitize_str(source_entity[,"Title"])
    allowedTitleKeys <- entity$getAllowedKeyValuesFor("titles")
    hasTitleKey <- any(sapply(allowedTitleKeys, function(x){startsWith(src_title, x)}))
    if(!hasTitleKey) src_title <- paste0("title:", src_title)
    titles <- if(!is.na(src_title)) extract_cell_components(src_title) else list()
    if(length(titles)>0){
      if(length(titles)==1){
        kvp <- extract_kvp(titles)
        entity$setTitle("title", paste(kvp$values, collapse=",")) 
      }else{
        for(title in titles){
          kvp <- extract_kvp(title)
          entity$setTitle(kvp$key, paste(kvp$values, collapse=","))
        }
      }
    }

    #description
    src_description <- sanitize_str(source_entity[,"Description"])
    allowedDescKeys <- entity$getAllowedKeyValuesFor("descriptions")
    hasDescKey <- any(sapply(allowedDescKeys, function(x){startsWith(src_description, x)}))
    if(!hasDescKey) src_description <- paste0("abstract:", src_description)
    descriptions <- if(!is.na(src_description)) extract_cell_components(src_description) else list()
    if(length(descriptions)>0){
      if(length(descriptions)==1){
        des_kvp <- extract_kvp(descriptions)
        entity$setDescription("abstract", paste(des_kvp$values, collapse=",")) 
      }else{
        for(description in descriptions){
          des_kvp <- extract_kvp(description)
          entity$setDescription(des_kvp$key, paste(des_kvp$values, collapse=","))
        }
      }
    }
    
    #subjects
    src_subject <- sanitize_str(source_entity[,"Subject"])
    subjects <- if(!is.na(src_subject)) extract_cell_components(src_subject) else list()
    if(length(subjects)>0){
      invisible(lapply(subjects, function(subject){
        subject_obj <- geoflow_subject$new(str = subject)
        entity$addSubject(subject_obj)
      }))
    }
    
    #formats
    src_format <- sanitize_str(source_entity[,"Format"])
    if(!is.na(src_format)){
      allowedFormatsKeys <- entity$getAllowedKeyValuesFor("formats")
      hasFormatKey <- any(sapply(allowedFormatsKeys, function(x){startsWith(src_format, x)}))
      if(!hasFormatKey) src_format <- paste0("resource:", src_format)
    }
    formats <- if(!is.na(src_format)) extract_cell_components(src_format) else list()
    if(length(formats)>0){
      invisible(lapply(formats, function(format){
        format_obj <- geoflow_format$new(str = format)
        entity$addFormat(format_obj)
      }))
    }
      
    #contacts
    src_contact <- sanitize_str(source_entity[,"Creator"])
    contacts <- if(!is.na(src_contact)) extract_cell_components(src_contact) else list()
    if(length(contacts)>0){
      invisible(lapply(contacts, function(contact){
        contact_splits <- unlist(strsplit(contact, ":"))
        contact_ids <- tolower(unlist(strsplit(contact_splits[2],",")))
        for(contact_id in contact_ids){
          if(is.na(contact_id)){
            config$logger.warn(sprintf("Warning: In entity %s, empty contact id will be ignored!", i))
          }else if(contact_id==""){
            config$logger.warn(sprintf("Warning: In entity %s, empty contact id will be ignored!", i))
          }else{
            contact_obj <- geoflow_contact$new()
            contact_obj$setIdentifier(key = "id", contact_id)
            contact_obj$setRole(contact_splits[1])
            entity$addContact(contact_obj)
          }
        }
      }))
    }
    
    #relations
    src_relation <- sanitize_str(source_entity[,"Relation"])
    relations <- if(!is.na(src_relation)) extract_cell_components(src_relation) else list()
    if(length(relations)>0){
      invisible(lapply(relations, function(relation){
        relation_obj <- geoflow_relation$new(str = relation)
        entity$addRelation(relation_obj)
      }))
    }
    
    #spatial extent
    spatial_cov <- sanitize_str(source_entity[,"SpatialCoverage"])
    if(!is.na(spatial_cov)){
      allowedSpatialCoverageKeys <- entity$getAllowedKeyValuesFor("spatialCoverage")
      hasSpatialCoverageKey <- any(sapply(allowedSpatialCoverageKeys, function(x){startsWith(spatial_cov, x)}))
      if(!hasSpatialCoverageKey) spatial_cov <- paste0("ewkt:", spatial_cov)
      spatial_props <- if(!is.na(spatial_cov)) extract_cell_components(spatial_cov) else list()
      if(length(spatial_props)>0){
        kvps <- lapply(spatial_props, extract_kvp)
        kvps <- lapply(kvps, function(x){out <- x; out$values <- list(paste0(out$values, collapse=",")); return(out)})
        names(kvps) <- sapply(kvps, function(x){x$key})
        for(kvpname in names(kvps)){
          switch(kvpname,
            "ewkt" = {
              spatial_cov <- kvps$ewkt$values[[1]]
              if(!is.na(spatial_cov)){
                if(!startsWith(spatial_cov,"SRID=")){
                  stop("SRID is missing! The spatial coverage should be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry")
                }
                spatial_cov <- unlist(strsplit(spatial_cov, ";"))
                if(length(spatial_cov)!=2){ 
                  stop("The spatial coverage should be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry")
                }
                spatial_srid <- as.integer(unlist(strsplit(spatial_cov[1],"SRID="))[2])
                spatial_cov <- spatial_cov[2]
                entity$setSrid(spatial_srid)
                entity$setSpatialExtent(spatial_cov, crs = spatial_srid)
              }
            },
            "wkt" = {
              spatial_cov <- kvps$wkt$values[[1]]
              if("srid" %in% names(kvps)){
                spatial_srid <- as.integer(kvps$srid$values[[1]])
                if(is.na(spatial_srid)){
                  stop("The spatial SRID should be an integer.")
                }
                entity$setSpatialExtent(spatial_cov, crs = spatial_srid)
              }else{
                warning("A WKT geometry is specified but without SRID!")
                entity$setSpatialExtent(spatial_cov, crs = NA)
              }
            },
            "srid" = {
              spatial_srid <- as.integer(kvps$srid$values[[1]])
              if(is.na(spatial_srid))
                stop("The spatial SRID should be an integer.")
              entity$setSrid(spatial_srid)
            }
          )
        }
      }
    }
    
    #temporal extent
    temporal_cov <- sanitize_str(source_entity[,"TemporalCoverage"])
    if(is(temporal_cov, "character")) if(temporal_cov == "") temporal_cov <- NA
    if(!is.null(temporal_cov)){
      if(!is.na(temporal_cov)) entity$setTemporalExtent(temporal_cov)
    }
    #Rights
    src_rights <- sanitize_str(source_entity[,"Rights"])
    rights <- if(!is.na(src_rights)) extract_cell_components(src_rights) else list()
    if(length(rights)>0){
      invisible(lapply(rights, function(right){
        right_obj <- geoflow_right$new(str = right)
        entity$addRight(right_obj)
      }))
    }
    
    #Provenance
    prov <- sanitize_str(source_entity[,"Provenance"])
    if(!is.na(prov)){
      prov_obj <- geoflow_provenance$new(str = prov)
      entity$setProvenance(prov_obj)
    }
    
    #data
    data <- sanitize_str(source_entity[,"Data"])
    if(!is.na(data)){
      if(data != ""){
        data_obj <- geoflow_data$new(str = data)
        data_obj$checkSoftwareProperties(config = config)
        entity$setData(data_obj)
        
        #check existence of feature type in dictionary
        dict = config$metadata$content$dictionary
        if(!is.null(dict)){
          ft <- entity$data$featureType
          if(is.null(entity$data$featureType)){
            config$logger.warn("No data featureType declared. Set feature type to dataset identifier")
            ft <- entity$identifiers[["id"]]
          }
          featureTypeObj <- dict$getFeatureTypeById(id = ft)
          if(is.null(featureTypeObj)){
            config$logger.warn(sprintf("No featuretype '%s' declared in dictionary!", ft))
          }else{
            entity$data$setFeatureTypeObj(featureTypeObj)
          }
        }
        
        #build featuretype for attributes/variables if declared
        if(length(entity$data$attributes)>0 | length(entity$data$attributes)>0){
          featureTypeObj <- geoflow_featuretype$new(id = entity$identifiers[["id"]])
          if(length(entity$data$attributes)>0){
            for(attribute in entity$data$attributes){
              attr_desc <- attr(attribute,"description")
              attr_handler <- attr(attribute, "uri")
              attributes(attribute) <- NULL
              member <- geoflow_featuremember$new(
                type = "attribute",
                code = attribute,
                name = attr_desc,
                def = NA,
                defSource = NA,
                registerId = attr_handler
              )
              featureTypeObj$addMember(member)
            }
          }
          if(length(entity$data$variables)>0){
            for(variable in entity$data$variables){
              var_desc <- attr(variable,"description")
              var_handler <- attr(variable, "uri")
              attributes(variable) <- NULL
              member <- geoflow_featuremember$new(
                type = "variable",
                code = variable,
                name = var_desc,
                def = NA,
                defSource = NA,
                registerId = var_handler
              )
              featureTypeObj$addMember(member)
            }
          }
          entity$data$setFeatureTypeObj(featureTypeObj)
        }
      }
    }
    
    entities <- c(entities, entity)
  }
  attr(entities, "source") <- source
  return(entities)
}

#handle_entities_gsheets
handle_entities_gsheet <- function(config, source){

  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  
  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}

#handle_entities_csv
handle_entities_csv <- function(config, source){
  
  #read csv TODO -> options management: sep, encoding etc
  source <- read.csv(source,stringsAsFactors = F)
  
  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}

#handle_entities_excel
handle_entities_excel <- function(config, source){
  
  #read excel TODO -> options management: sep, encoding etc
  source <- as.data.frame(readxl::read_excel(source))
  
  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}

#handle_entities_dbi
handle_entities_dbi <- function(config, source){
  dbi <- config$software$input$dbi
  if(is.null(dbi)){
    stop("There is no database input software configured to handle entities from DB")
  }
  
  #db source
  is_query <- startsWith(tolower(source), "select ")
  if(is_query){
    source <- try(DBI::dbGetQuery(dbi, source))
    if(class(source)=="try-error"){
      errMsg <- sprintf("Error while trying to execute DB query '%s'.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }else{
    source <- try(DBI::dbGetQuery(dbi, sprintf("select * from %s", source)))
    if(class(source)=="try-error"){
      errMsg <- sprintf("Error while trying to read DB table/view '%s'. Check if it exists in DB.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }

  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}

#handle_entities_ncdf
handle_entities_ncdf <- function(config, source){

  #if(!mime::guess_type(source)=="application/x-netcdf"){
  #  errMsg <- "Error in 'handle_entities_df': source parameter should be an 'netcdf' file"
  #  config$logger.error(errMsg)
  #  stop(errMsg)
  #}
  entities<-list()
  entity <- geoflow_entity$new()
  source_name<-source
  source <- ncdf4::nc_open(source)
  
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
      entity$setDescription("abstract", attr$summary)
  }
  
  edition <- attr$product_version
  if(!is.null(edition)){
    entity$setDescription("edition", attr$edition)
  }
  
  credit <- attr$credit
  if(!is.null(credit)){
    entity$setDescription("credit", attr$edition)
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
      subject_obj <- geoflow_subject$new(str = paste0(key,"[",thesaurus,"]:",keywords))
      entity$addSubject(subject_obj)
    }
  }

  #contacts
  if(!is.null(attr$creator_name)){
    contact_obj <- geoflow_contact$new()
    contact_obj$setRole("owner")
    contact_obj$setLastName(attr$creator_name)
    contact_obj$setOrganizationName(attr$creator_institution)
    contact_obj$setEmail(attr$creator_email)
    contact_obj$setWebsiteUrl(attr$creator_url)
    entity$addContact(contact_obj) 
    
    contact_obj <- geoflow_contact$new()
    contact_obj$setRole("metadata")
    contact_obj$setLastName(attr$creator_name)
    contact_obj$setOrganizationName(attr$creator_institution)
    contact_obj$setEmail(attr$creator_email)
    contact_obj$setWebsiteUrl(attr$creator_url)
    entity$addContact(contact_obj) 
  }
  
  if(!is.null(attr$publisher_name)){
    contact_obj <- geoflow_contact$new()
    contact_obj$setRole("publisher")
    contact_obj$setLastName(attr$publisher_name)
    contact_obj$setOrganizationName(attr$publisher_institution)
    contact_obj$setEmail(attr$publisher_email)
    contact_obj$setWebsiteUrl(attr$publisher_url)
    entity$addContact(contact_obj) 
  }
  
  #dates
  if(!is.null(attr$date_created)) if(class(try(as.POSIXlt.character(attr$date_created)))!="try-error"){entity$addDate("creation", attr$date_created)}else{entity$addDate("creation", Sys.time())}
  if(!is.null(attr$date_modified)) if(class(try(as.POSIXlt.character(attr$date_modified)))!="try-error")entity$addDate("revision", attr$date_modified)
  if(!is.null(attr$date_metadata_modified)) if(class(try(as.POSIXlt.character(attr$date_metadata_modified)))!="try-error")entity$addDate("metadata", attr$date_metadata_modified)
  if(!is.null(attr$date_issued)) if(class(try(as.POSIXlt.character(attr$date_issued)))!="try-error")entity$addDate("publication", attr$date_issued)

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
    if(class(try(as.POSIXlt.character(attr$time_coverage_start)))!="try-error")if(class(try(as.POSIXlt.character(attr$time_coverage_end)))!="try-error"){
      temporal_cov<- paste(attr$time_coverage_start,attr$time_coverage_end,sep="/")
      entity$setTemporalExtent(temporal_cov)
    }
  }
   
  #relation
  #Not yet implemented
  
  #rights
  if(!is.null(attr$license)){
    right_obj <- geoflow_right$new(str = paste0("license:",attr$license))
    entity$addRight(right_obj)
  }
  
  #formats
  format_obj <- geoflow_format$new(str = "resource:application/x-netcdf")
  entity$addFormat(format_obj)
  
  #provenance
  #Not yet implemented
  
  #data
  data_obj <- geoflow_data$new()
  
  #variables
  
  variables = lapply(names(source$var), function(x){
    #TODO in future create a geoflow_variable class to handle more info (eg. min/max, type, domain eg. physicalMeasurement, etc)
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
          uom=attr$geospatial_lat_units,
          value = if(is.numeric(attr$geospatial_lat_resolution)){attr$geospatial_lat_resolution}else{unlist(strsplit(attr$geospatial_lat_resolution," "))[1]}
        )
      }
      #column
      if(startsWith(source$dim[[dim]]$name,"lon")){
        name="column" 
        resolution = list(
          uom=attr$geospatial_lon_units,
          value = if(is.numeric(attr$geospatial_lat_resolution)){attr$geospatial_lat_resolution}else{unlist(strsplit(attr$geospatial_lat_resolution," "))[1]}
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
        }else{
          resolution=list(uom=NULL,value=NULL)
        }
      }
      #vertical
      if(startsWith(source$dim[[dim]]$name,"z")){
        name="vertical" 
        resolution = list(
          uom=attr$geospatial_vertical_units,
          value = unlist(strsplit(attr$geospatial_vertical_resolution,"[.]"))[1]
        )
      }
      dimension_obj<-geoflow_dimension$new()
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

#handle_entities_thredds
handle_entities_thredds <- function(config, source){

  thredds <- config$software$input$thredds
  if(is.null(thredds)){
    stop("There is no database input software configured to handle entities from Thredds")
  }
  
  if(length(thredds$get_dataset_names())==0) {
    errMsg <- sprintf("No datasets for the thredds")
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  if(!is.null(source)) if(source != ""){
    datasets<-unlist(strsplit(source,","))
  }else{
    datasets<- thredds$get_dataset_names()
  }

  
  uri<-XML::parseURI(thredds$url)
  base_uri<-paste0(uri$scheme,"://",uri$server)
  config$logger.info(sprintf("Thredds Base URL: %s", base_uri))
  
  entities<-list()
  entities<- lapply(datasets, function(dataset){
    data<-thredds$get_datasets(dataset)[[dataset]]
    config$logger.info(sprintf("Build entity for '%s'", data$url))
    
    #entity
    
    odap<-unlist(sapply(names(thredds$list_services()), function(x) if(thredds$list_services()[[x]]["serviceType"]=="OPENDAP") thredds$list_services()[[x]]["base"]))[1]
    if(is.null(odap)){
      errMsg <- sprintf("No OpenDAP service for Thredds '%s'", thredds$url)
      config$logger.error(errMsg)
      stop(errMsg)
    }

    odap_uri<-paste0(base_uri,odap,data$url)
    cat(odap_uri)
    config$logger.info(sprintf("OpenDAP URL for '%s': %s", data$url, odap_uri))
    #layername<-ncdf4::nc_open(odap_uri)$var[[2]]$name
    entity <- handle_entities_ncdf(config,odap_uri)[[1]]
    
    #relations
    
    #Thredds
    new_thredds_link <- geoflow_relation$new()
    new_thredds_link$setKey("http")
    new_thredds_link$setName(dataset)
    new_thredds_link$setDescription(paste0(entity$titles[["title"]]," - Thredds Catalog"))
    new_thredds_link$setLink(thredds$url)
    entity$addRelation(new_thredds_link)
    
    #data
    http<-unlist(sapply(names(thredds$list_services()), function(x) if(thredds$list_services()[[x]]["serviceType"]=="HTTPServer") thredds$list_services()[[x]]["base"]))[1]
    if(!is.null(http)){
      http_uri<-paste0(base_uri,http,data$url)
      new_data_link <- geoflow_relation$new()
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
      requireNamespace("ows4R")
      wms_uri<-paste0(base_uri,wms,data$url,"?service=WMS")
      wms_request<-paste0(base_uri,wms,data$url)
      wms <- ows4R::WMSClient$new(url = wms_request, serviceVersion = "1.3.0", logger = "INFO")
      
      thumbnails<-data.frame(NULL)
      for(layer in wms$getLayers()){
      layername<-layer$getName()  
        if(!is.null(layername)){
          title<-layer$getTitle()
          bbox<-paste(entity$spatial_bbox,collapse=",")
          srs<-if(!is.null(entity$srid))entity$srid else "EPSG:4326"
          style<-layer$getStyle()
          thumbnail<-sprintf("%s&version=1.1.1&request=GetMap&LAYERS=%s&SRS=%s&BBOX=%s&WIDTH=600&HEIGHT=300&STYLES=%s&FORMAT=image/png&TRANSPARENT=true",
                  wms_uri,layername,srs,bbox,style)
          
          #thumbnail quality
          img<-png::readPNG(RCurl::getURLContent(thumbnail))
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
          
          #add wms relation
          new_wms <- geoflow_relation$new()
          new_wms$setKey("wms130")
          new_wms$setName(layername)
          new_wms$setDescription(title)
          new_wms$setLink(wms_uri)
          entity$addRelation(new_wms)
          }
      }
      #add prettiest thumbnail relation
      if(!is.null(thumbnails)){
        thumbnails<-thumbnails[order(thumbnails$score, decreasing = T),]
        for(i in 1:nrow(thumbnails)){
          thumbnail<-thumbnails[i,]
          new_thumbnail <- geoflow_relation$new()
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
    #   new_wcs <- geoflow_relation$new()
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
      temporal_cov<- paste(min(values),max(values),sep="/")
      entity$setTemporalExtent(temporal_cov)
    }
    
    return(entity)
    
  })
  return(entities)
}