ows4R_publish_iso_19139 <- function(entity, config, options){
  
  geometa_inspire <- if(!is.null(options$geometa_inspire)) options$geometa_inspire else FALSE
  if(geometa_inspire){
    config$logger.info("INSPIRE geometa option enabled: The record will be checked against the INSPIRE reference validator prior its CSW-T publication")
  }
    
  if(!require("geometa")){
    stop("Package 'geometa' is required for this action")
  }
  if(!require("ows4R")){
    stop("Package 'ows4R' is required for this action")
  }

  metaFile <- file.path("metadata", paste0(entity$identifiers[["id"]],".rds"))
  md <- readRDS(metaFile)
  
  #shortcut for csw config
  CSW <- config$software$output$csw
  
  if(is.null(CSW)){
    errMsg <- "This action requires a CSW software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  meta_dc <- CSW$getRecordById(entity$identifiers[["id"]])
  if(is.null(meta_dc)){
    config$logger.info(sprintf("Inserting new record with id '%s'", meta_dc))
    CSW$insertRecord(record = md, geometa_inspire = geometa_inspire)
  }else{
    config$logger.info(sprintf("Updating existing record with id '%s'", meta_dc))
    CSW$updateRecord(record = md, geometa_inspire = geometa_inspire)
  }
  return(md$fileIdentifier)
}