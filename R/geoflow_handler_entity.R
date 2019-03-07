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
    source_entity <- source[i,]
    entity <- geoflow_entity$new()
    entity$setId(source_entity[,"Identifier"])
    entity$setTitle(source_entity[,"Title"])
    entity$setAbstract(source_entity[,"Description"])
    
    #subjects
    subjects <- unlist(strsplit(sanitize_str(source_entity[,"Subject"]), ";"))
    invisible(lapply(subjects, function(subject){
      subject_obj <- geoflow_subject$new(str = subject)
      entity$addSubject(subject_obj)
    }))
    
    #contacts
    contacts <- unlist(strsplit(sanitize_str(source_entity[,"Creator"]), ";"))
    invisible(lapply(contacts, function(contact){
      contact_splits <- unlist(strsplit(contact, ":"))
      contact_obj <- geoflow_contact$new()
      contact_obj$setId(contact_splits[2])
      contact_obj$setRole(contact_splits[1])
      entity$addContact(contact_obj)
    }))
    
    #relations
    relations <- unlist(strsplit(sanitize_str(source_entity[,"Relation"]), ";"))
    invisible(lapply(relations, function(relation){
      relation_obj <- geoflow_relation$new(str = relation)
      entity$addRelation(relation_obj)
    }))
    
    #spatial extent
    entity$setSrid(source_entity[,"SpatialReferenceSystem"])
    entity$setSpatialExtent(source_entity[,"SpatialCoverage"], crs = source_entity[,"SpatialReferenceSystem"])
    
    #temporal extent
    entity$setTemporalExtent(source_entity[,"TemporalCoverage"])
    
    entities <- c(entities, entity)
  }
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
  contacts <- handle_entities_df(config, source)
  return(contacts)
}
