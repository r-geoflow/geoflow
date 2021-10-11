ows4R_publish_iso_19139 <- function(entity, config, options){
  
  if(!requireNamespace("ows4R", quietly = TRUE)){
    stop("The 'ows4R-publish-iso-19139' action requires the 'ows4R' package")
  }
  
  geometa_inspire <- if(!is.null(options$geometa_inspire)) options$geometa_inspire else FALSE
  if(geometa_inspire){
    config$logger.info("INSPIRE geometa option enabled: The record will be checked against the INSPIRE reference validator prior its CSW-T publication")
  }

  #shortcut for csw config
  CSW <- config$software$output$csw
  
  if(is.null(CSW)){
    errMsg <- "This action requires a CSW software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #function to publish
  doPublish <- function(md, inspire){
    meta_id <- NULL
    if(is(md, "ISOMetadata")) meta_id <- md$fileIdentifier
    if(is(md, "ISOFeatureCatalogue")) meta_id <- md$attrs[["uuid"]]
    meta_dc <- CSW$getRecordById(meta_id)
    if(is.null(meta_dc)){
      config$logger.info(sprintf("Inserting new record with id '%s'", meta_id))
      CSW$insertRecord(record = md, geometa_inspire = inspire)
    }else{
      config$logger.info(sprintf("Updating existing record with id '%s'", meta_id))
      CSW$updateRecord(record = md, geometa_inspire = inspire)
    }
  }
  
  #geometa ISO 19115
  geometa_iso19115_action <- NULL
  actions <- config$actions[sapply(config$actions, function(x){x$id=="geometa-create-iso-19115"})]
  if(length(actions)>0) geometa_iso19115_action <- actions[[1]]
  if(!is.null(geometa_iso19115_action)){
    metaFile <- file.path("metadata", paste0(entity$identifiers[["id"]],"_ISO-19115.xml"))
    md <- geometa::readISO19139(metaFile)
    if(file.exists(metaFile)) doPublish(md, geometa_inspire)
    rm(md)
  }
  #geometa ISO 19110
  geometa_iso19110_action <- NULL
  actions <- config$actions[sapply(config$actions, function(x){x$id=="geometa-create-iso-19110"})]
  if(length(actions)>0) geometa_iso19110_action <- actions[[1]]
  if(!is.null(geometa_iso19110_action)){
    geometa_inspire <- FALSE
    metaFile <- file.path("metadata", paste0(entity$identifiers[["id"]],"_ISO-19110.xml"))
    md <- geometa::readISO19139(metaFile)
    if(file.exists(metaFile)) doPublish(md, geometa_inspire)
    rm(md)
  }
  
  return(TRUE)
}