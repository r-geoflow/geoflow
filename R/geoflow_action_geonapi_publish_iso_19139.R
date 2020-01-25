geonapi_publish_iso_19139 <- function(entity, config, options){
  
  geometa_inspire <- if(!is.null(options$geometa_inspire)) options$geometa_inspire else FALSE
  if(geometa_inspire){
    config$logger.info("INSPIRE geometa option enabled: The record will be checked against the INSPIRE reference validator prior its CSW-T publication")
  }
  
  #shortcut for gn config
  GN <- config$software$output$geonetwork
  
  if(is.null(GN)){
    errMsg <- "This action requires a Geonetwork software to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  privileges <- if(!is.null(options$privileges)) options$privileges else  c("view","dynamic","featured")
  
  #to insert or update a metadata into a geonetwork.
  #An insert has to be done in 2 operations (the insert itself, and the privilege setting to "publish" it either to a restrained group or to public)
  #An update has to be done based on the internal Geonetwork id (that can be queried as well
  #function doPublish
  doPublish <- function(mdfile, inspire){
    mdId <- NULL
    md <- readISO19139(metaFile)
    privs <- privileges
    if(is(md, "ISOMetadata")){
      mdId <- md$fileIdentifier
      privs <- privileges[privileges!="featured"]
    }
    if(is(md, "ISOFeatureCatalogue")){
      mdId <- md$attrs[["uuid"]]
      privs <- "view"
    }
    metaId <- GN$get(mdId, by = "uuid", output = "id")
    if(is.null(metaId)){
      #insert metadata (once inserted only visible to the publisher)
      group <- if(!is.null(options$group)) options$group else "1"
      category <- if(!is.null(options$category)) options$category else "datasets"
      created = GN$insertMetadata(geometa = md, group = group, category = category,
                                  geometa_inspire = inspire)
      
      #config privileges
      config <- GNPrivConfiguration$new()
      config$setPrivileges("all", privs)
      GN$setPrivConfiguration(id = created, config = config)
    }else{
      #update a metadata
      updated = GN$updateMetadata(id = metaId, geometa = md,
                                  geometa_inspire = inspire)
      
      #config privileges
      gn_config <- GNPrivConfiguration$new()
      gn_config$setPrivileges("all", privs)
      GN$setPrivConfiguration(id = metaId, config = gn_config)
    }
    rm(md)
  }
  
  #geometa ISO 19115
  geometa_iso19115_action <- NULL
  actions <- config$actions[sapply(config$actions, function(x){x$id=="geometa-create-iso-19115"})]
  if(length(actions)>0) geometa_iso19115_action <- actions[[1]]
  if(!is.null(geometa_iso19115_action)){
    metaFile <- file.path("metadata", paste0(entity$identifiers[["id"]],"_ISO-19115.xml"))
    if(file.exists(metaFile)) doPublish(metaFile, geometa_inspire)
  }
  #geometa ISO 19110
  geometa_iso19110_action <- NULL
  actions <- config$actions[sapply(config$actions, function(x){x$id=="geometa-create-iso-19110"})]
  if(length(actions)>0) geometa_iso19110_action <- actions[[1]]
  if(!is.null(geometa_iso19110_action)){
    geometa_inspire <- FALSE
    metaFile <- file.path("metadata", paste0(entity$identifiers[["id"]],"_ISO-19110.xml"))
    if(file.exists(metaFile)) doPublish(metaFile, geometa_inspire)
  }
  
  return(TRUE)
}