function(action, entity, config){
  
  if(!requireNamespace("ows4R", quietly = TRUE)){
    stop("The 'ows4R-publish-iso-19139' action requires the 'ows4R' package")
  }
  
  geometa_inspire <- action$getOption("geometa_inspire")
  INSPIRE_VALIDATOR <- NULL
  #as of 2025-05-02, there is no need anymore to have an API key to validate metadata
  #therefore the INSPIRE metadata validator software declaration is not needed
  # if(geometa_inspire){
  #   #check inspire metadata validator configuration
  #   INSPIRE_VALIDATOR <- config$software$output$inspire
  #   if(is.null(INSPIRE_VALIDATOR)){
  #     errMsg <- "This action requires a INSPIRE metadata validator software to be declared in the configuration"
  #     config$logger$ERROR(errMsg)
  #     stop(errMsg)
  #   }
  #   config$logger$INFO("INSPIRE geometa option enabled: The record will be checked against the INSPIRE reference validator prior its CSW-T publication")
  # }
  
  #shortcut for csw config
  CSW <- config$software$output$csw
  
  if(is.null(CSW)){
    errMsg <- "This action requires a CSW software to be declared in the configuration"
    config$logger$ERROR(errMsg)
    stop(errMsg)
  }
  
  #function to publish
  doPublish <- function(md, inspire, inspireValidator = NULL){
    meta_id <- NULL
    if(is(md, "ISOMetadata")) meta_id <- md$fileIdentifier
    if(is(md, "ISOFeatureCatalogue")) meta_id <- md$attrs[["uuid"]]
    meta_dc <- CSW$getRecordById(meta_id)
    if(is.null(meta_dc)){
      config$logger$INFO("Inserting new record with id '%s'", meta_id)
      CSW$insertRecord(record = md, geometa_inspire = inspire, geometa_inspireValidator = inspireValidator)
    }else{
      config$logger$INFO("Updating existing record with id '%s'", meta_id)
      CSW$updateRecord(record = md, geometa_inspire = inspire, geometa_inspireValidator = inspireValidator)
    }
  }
  
  #geometa ISO 19115
  geometa_iso19115_action <- NULL
  actions <- config$actions[sapply(config$actions, function(x){x$id=="geometa-create-iso-19115"})]
  if(length(actions)>0) geometa_iso19115_action <- actions[[1]]
  if(!is.null(geometa_iso19115_action)){
    metaFile <- file.path("metadata", paste0(entity$identifiers[["id"]],"_ISO-19115.xml"))
    if(file.exists(metaFile)){
      md <- geometa::readISO19139(metaFile)
      doPublish(md, geometa_inspire, INSPIRE_VALIDATOR)
      rm(md)
    }else{
      config$logger$WARN(sprintf("No ISO 19115 XML metadata file to publish for entity '%s, skipping action!", entity$identifiers[["id"]]))
    }
  }
  #geometa ISO 19110
  geometa_iso19110_action <- NULL
  actions <- config$actions[sapply(config$actions, function(x){x$id=="geometa-create-iso-19110"})]
  if(length(actions)>0) geometa_iso19110_action <- actions[[1]]
  if(!is.null(geometa_iso19110_action)){
    geometa_inspire <- FALSE
    metaFile <- file.path("metadata", paste0(entity$identifiers[["id"]],"_ISO-19110.xml"))
    if(file.exists(metaFile)){
      md <- geometa::readISO19139(metaFile)
      doPublish(md, geometa_inspire, INSPIRE_VALIDATOR)
      rm(md)
    }else{
      config$logger$WARN(sprintf("No ISO 19110 XML metadata file to publish for entity '%s, skipping action!", entity$identifiers[["id"]]))
    }
  }
  
  return(TRUE)
}
