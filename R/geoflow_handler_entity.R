#' handle_entities_df
#' @export
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
    
    date <- sanitize_date(source_entity[,"Date"])
    if(!is.na(date)) entity$setDate(date)
    
    #types
    src_type <- sanitize_str(source_entity[,"Type"])
    types <- if(!is.na(src_type)) extract_cell_components(src_type) else list()
    if(length(types)>0){
      if(length(types)==1){
        entity$setType("generic", types)
      }else{
        for(type in types){
          if(regexpr(":",type) == -1){
            entity$setDescription("generic", type)
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
    entity$setTitle(source_entity[,"Title"])
    
    #description
    src_description <- sanitize_str(source_entity[,"Description"])
    descriptions <- if(!is.na(src_description)) extract_cell_components(src_description) else list()
    if(length(descriptions)>0){
      if(length(descriptions)==1){
        if(regexpr(":",descriptions) == -1){
          entity$setDescription("abstract", descriptions)
        }else{
          des_kvp <- extract_kvp(descriptions)
          descriptions <- paste(des_kvp$values, collapse=",")
          entity$setDescription("abstract", descriptions) 
        }
      }else{
        for(description in descriptions){
          if(regexpr(":",description) == -1)
            entity$setDescription("abstract", description)
          else
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
            contact_obj$setId(contact_id)
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
      if(!startsWith(spatial_cov,"SRID=")) 
        stop("The spatial coverage should be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry")
      spatial_cov <- unlist(strsplit(spatial_cov, ";"))
      if(length(spatial_cov)!=2) 
        stop("The spatial coverage should be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry")
      spatial_srid <- as.integer(unlist(strsplit(spatial_cov[1],"SRID="))[2])
      spatial_cov <- spatial_cov[2]
      entity$setSrid(spatial_srid)
      entity$setSpatialExtent(spatial_cov, crs = spatial_srid)
    }
    
    #temporal extent
    temporal_cov <- sanitize_str(source_entity[,"TemporalCoverage"])
    if(!is.na(temporal_cov) & temporal_cov != ""){
      entity$setTemporalExtent(temporal_cov)
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
        entity$setData(data_obj)
      }
    }
    
    #enrich metadata with dynamic properties
    if(!is.null(entity$data)){
      #data features
      entity$enrichWithFeatures(config)
      #data relations (eg. geosapi & OGC data protocol online resources)
      entity$enrichWithRelations(config)
    }
    
    entities <- c(entities, entity)
  }
  attr(entities, "source") <- source
  return(entities)
}

#' handle_entities_gsheets
#' @export
handle_entities_gsheet <- function(config, source){
  
  if(!require("gsheet")) stop("Package 'gsheet' is required!")
  
  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  
  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}

#' handle_entities_csv
#' @export
handle_entities_csv <- function(config, source){
  
  #read csv TODO -> options management: sep, encoding etc
  source <- read.csv(source)
  
  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}

#' handle_entities_excel
#' @export
handle_entities_excel <- function(config, source){
  
  #read excel TODO -> options management: sep, encoding etc
  source <- as.data.frame(readxl::read_excel(source))
  
  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}

#' handle_entities_dbi
#' @export
handle_entities_dbi <- function(config, source){
  dbi <- config$software$input$dbi
  if(is.null(dbi)){
    stop("There is no database input software configured to handle entities from DB")
  }
  
  #db source
  is_query <- startsWith(tolower(source), "select ")
  if(is_query){
    source <- try(DBI::dbGetQuery(dbi, source))
    if(class(source)="try-error"){
      errMsg <- sprintf("Error while trying to execute DB query '%s'.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }else{
    source <- try(DBI::dbReadTable(dbi, source))
    if(class(source)="try-error"){
      errMsg <- sprintf("Error while trying to read DB table/view '%s'. Check if it exists in DB.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }

  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}
