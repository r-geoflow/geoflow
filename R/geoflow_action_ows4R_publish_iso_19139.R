ows4R_publish_iso_19139 <- function(entity, config, options){
    
  if(!require("geometa")){
    stop("Package 'geometa' is required for this action")
  }
  if(!require("ows4R")){
    stop("Package 'ows4R' is required for this action")
  }

  metaFile <- file.path("metadata", paste0(entity$id,".rds"))
  md <- readRDS(metaFile)
  
  #shortcut for csw config
  CSW <- config$software$csw
  
  if(is.null(CSW)){
    errMsg <- "This action requires a CSW software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  meta_dc <- CSW$getRecordById(entity$id)
  if(is.null(meta_dc)){
    CSW$insertRecord(record = md)
  }else{
    CSW$updateRecord(record = md)
  }
  return(md$fileIdentifier)
}