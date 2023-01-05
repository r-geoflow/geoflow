#handle_entities_df
handle_entities_df <- function(config, source){
  
  if(!is(source, "data.frame")){
    errMsg <- "Error in 'handle_entities_df': source parameter should be an object of class 'data.frame'"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #validation
  config$logger.info("Validating entities")
  validation_report <- geoflow::geoflow_validator_entities$new(source = source)$validate_content()
  if(is.null(validation_report)){
    errMsg <- "Error of metadata structure for entities"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  if(nrow(validation_report)==0){
    config$logger.info("No validation issue detected!")
  }else{
    config$logger.info("Validation issues -->")
    print(validation_report)
    if(any(validation_report$type == "ERROR")){
      errMsg <- "At least one error of metadata syntax has been detected, aborting..."
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }
  
  entities <- list()
  rowNum <- nrow(source)
  config$logger.info(sprintf("Parsing %s entities from tabular source", rowNum))
  for(i in 1:rowNum){
    config$logger.info(sprintf("Parsing entity %s", i))
    source_entity <- source[i,]
    entity <- geoflow::geoflow_entity$new()
    
    #language
    entity$setLanguage(source_entity[,"Language"])
    
    #dates
    src_date <- geoflow::sanitize_str(as(source_entity[,"Date"], "character"))
    dates <- if(!is.na(src_date)) geoflow::extract_cell_components(src_date) else list()
    if(length(dates)>0){
      if(length(dates)==1){
        if(regexpr(":",dates) == -1 && nchar(dates)>0){
          entity$dates <- list()
          entity$addDate("creation", dates)
        }else{
          date_kvp <- geoflow::extract_kvp(dates)
          for(date in date_kvp$values){
            entity$addDate(date_kvp$key, date)
          }
        }
      }else{
        for(date in dates){
          if(regexpr("creation:",date)>0){
            entity$dates <- list()
            date_kvp <- geoflow::extract_kvp(date)
            for(adate in date_kvp$values){
              entity$addDate(date_kvp$key, adate)
            }
          }else{
            date_kvp <- geoflow::extract_kvp(date)
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
    src_type <- geoflow::sanitize_str(source_entity[,"Type"])
    types <- if(!is.na(src_type)) geoflow::extract_cell_components(src_type) else list()
    if(length(types)>0){
      if(length(types)==1){
        entity$setType("generic", types)
      }else{
        for(type in types){
          if(regexpr(":",type) == -1){
            entity$setType("generic", type)
          }else{
            type_kvp <- geoflow::extract_kvp(type)
            entity$setType(type_kvp$key, type_kvp$values[[1]])
          }
        }
      }
    }
    
    #identifier
    identifiers <-geoflow::extract_cell_components(geoflow::sanitize_str(source_entity[,"Identifier"]))
    for(identifier in identifiers){
      if(regexpr(":",identifier) == -1){
        entity$setIdentifier("id", identifier)
      }else{
        id_kvp <- geoflow::extract_kvp(identifier)
        entity$setIdentifier(id_kvp$key, id_kvp$values[[1]])
      }
    }
    
    #title
    src_title <- geoflow::sanitize_str(source_entity[,"Title"])
    if(!is.na(src_title)){
      allowedTitleKeys <- entity$getAllowedKeyValuesFor("Title")
      hasTitleKey <- any(sapply(allowedTitleKeys, function(x){startsWith(src_title, x)}))
      if(!hasTitleKey) src_title <- paste0("title:", src_title)
    }
    titles <- if(!is.na(src_title)) geoflow::extract_cell_components(src_title) else list()
    if(length(titles)>0){
      kvps <- geoflow::extract_kvps(titles, collapse=",")
      for(kvp in kvps){
        entity$setTitle(kvp$key, kvp$values)
      }
    }
    
    #description
    src_description <- geoflow::sanitize_str(source_entity[,"Description"])
    if(!is.na(src_description)){
      allowedDescKeys <- entity$getAllowedKeyValuesFor("Description")
      hasDescKey <- any(sapply(allowedDescKeys, function(x){startsWith(src_description, x)}))
      if(!hasDescKey) src_description <- paste0("abstract:", src_description)
    }
    descriptions <- if(!is.na(src_description)) geoflow::extract_cell_components(src_description) else list()
    if(length(descriptions)>0){
      kvps <- geoflow::extract_kvps(descriptions, collapse=",")
      for(kvp in kvps){
        entity$setDescription(kvp$key, kvp$values)
      }
    }
    
    #subjects
    src_subject <- geoflow::sanitize_str(source_entity[,"Subject"])
    subjects <- if(!is.na(src_subject)) geoflow::extract_cell_components(src_subject) else list()
    if(length(subjects)>0){
      kvps <- geoflow::extract_kvps(subjects)
      for(kvp in kvps){
        subject_obj <- geoflow::geoflow_subject$new(kvp = kvp)
        entity$addSubject(subject_obj)
      }
    }
    
    #formats
    src_format <- geoflow::sanitize_str(source_entity[,"Format"])
    if(!is.na(src_format)){
      allowedFormatsKeys <- entity$getAllowedKeyValuesFor("Format")
      hasFormatKey <- any(sapply(allowedFormatsKeys, function(x){startsWith(src_format, x)}))
      if(!hasFormatKey) src_format <- paste0("resource:", src_format)
    }
    formats <- if(!is.na(src_format)) geoflow::extract_cell_components(src_format) else list()
    if(length(formats)>0){
      invisible(lapply(formats, function(format){
        format_obj <- geoflow::geoflow_format$new(str = format)
        entity$addFormat(format_obj)
      }))
    }
    
    #contacts
    src_contact <- geoflow::sanitize_str(source_entity[,"Creator"])
    contacts <- if(!is.na(src_contact)) geoflow::extract_cell_components(src_contact) else list()
    if(length(contacts)>0){
      invisible(lapply(contacts, function(contact){
        contact_splits <- unlist(strsplit(contact, ":"))
        contact_ids <- unlist(strsplit(contact_splits[2],","))
        for(contact_id in contact_ids){
          if(is.na(contact_id)){
            config$logger.warn(sprintf("Warning: In entity %s, empty contact id will be ignored!", i))
          }else if(contact_id==""){
            config$logger.warn(sprintf("Warning: In entity %s, empty contact id will be ignored!", i))
          }else{
            contact_obj <- geoflow::geoflow_contact$new()
            contact_obj$setIdentifier(key = "id", contact_id)
            contact_obj$setRole(contact_splits[1])
            entity$addContact(contact_obj)
          }
        }
      }))
    }
    
    #relations
    src_relation <- geoflow::sanitize_str(source_entity[,"Relation"])
    relations <- if(!is.na(src_relation)) geoflow::extract_cell_components(src_relation) else list()
    if(length(relations)>0){
      invisible(lapply(relations, function(relation){
        relation_obj <- geoflow::geoflow_relation$new(str = relation)
        entity$addRelation(relation_obj)
      }))
    }
    
    #spatial extent
    spatial_cov <- geoflow::sanitize_str(source_entity[,"SpatialCoverage"])
    if(!is.na(spatial_cov)){
      allowedSpatialCoverageKeys <- entity$getAllowedKeyValuesFor("SpatialCoverage")
      hasSpatialCoverageKey <- any(sapply(allowedSpatialCoverageKeys, function(x){startsWith(spatial_cov, x)}))
      if(!hasSpatialCoverageKey) spatial_cov <- paste0("ewkt:", spatial_cov)
      spatial_props <- if(!is.na(spatial_cov)) geoflow::extract_cell_components(spatial_cov) else list()
      if(length(spatial_props)>0){
        kvps <- lapply(spatial_props, geoflow::extract_kvp)
        kvps <- lapply(kvps, function(x){out <- x; out$values <- list(paste0(out$values, collapse=",")); return(out)})
        names(kvps) <- sapply(kvps, function(x){x$key})
        for(kvpname in names(kvps)){
          switch(kvpname,
                 "ewkt" = {
                   spatial_cov <- kvps$ewkt$values[[1]]
                   if(!is.na(spatial_cov)){
                     if(!startsWith(spatial_cov,"SRID=")){
                       # stop("SRID is missing! The spatial coverage should be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry")
                     }
                     spatial_cov <- unlist(strsplit(spatial_cov, ";"))
                     if(length(spatial_cov)!=2){ 
                       # stop("The spatial coverage should be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry")
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
                       # stop("The spatial SRID should be an integer.")
                     }
                     entity$setSpatialExtent(spatial_cov, crs = spatial_srid)
                   }else{
                     # warning("A WKT geometry is specified but without SRID!")
                     entity$setSpatialExtent(spatial_cov, crs = NA)
                   }
                 },
                 "srid" = {
                   spatial_srid <- as.integer(kvps$srid$values[[1]])
                   entity$setSrid(spatial_srid)
                 }
          )
        }
      }
    }
    
    #temporal extent
    temporal_cov <- geoflow::sanitize_str(source_entity[,"TemporalCoverage"])
    if(is(temporal_cov, "character")) if(temporal_cov == "") temporal_cov <- NA
    if(!is.null(temporal_cov)){
      if(!is.na(temporal_cov)) entity$setTemporalExtent(temporal_cov)
    }
    #Rights
    src_rights <- geoflow::sanitize_str(source_entity[,"Rights"])
    rights <- if(!is.na(src_rights)) geoflow::extract_cell_components(src_rights) else list()
    if(length(rights)>0){
      kvps <- geoflow::extract_kvps(rights)
      for(kvp in kvps){
        right_obj <- geoflow::geoflow_right$new(kvp = kvp)
        entity$addRight(right_obj)
      }
    }
    
    #Provenance
    prov <- geoflow::sanitize_str(source_entity[,"Provenance"])
    if(!is.na(prov)){
      prov_obj <- geoflow::geoflow_provenance$new(str = prov)
      entity$setProvenance(prov_obj)
    }
    
    #data
    data <- geoflow::sanitize_str(source_entity[,"Data"])
    if(!is.na(data)){
      if(data != ""){
        
        data_obj <- geoflow::geoflow_data$new(str = data, config = config)
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
          featureTypeObj <- geoflow::geoflow_featuretype$new(id = entity$identifiers[["id"]])
          if(length(entity$data$attributes)>0){
            for(attribute in entity$data$attributes){
              attr_desc <- attr(attribute,"description")
              attr_handler <- attr(attribute, "uri")
              attributes(attribute) <- NULL
              member <- geoflow::geoflow_featuremember$new(
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
              member <- geoflow::geoflow_featuremember$new(
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
