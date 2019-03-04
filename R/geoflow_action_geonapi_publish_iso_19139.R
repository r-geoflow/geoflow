geonapi_publish_iso_19139 <- function(entity, config, options){
  
  if(!require("geometa")){
    stop("Package 'geometa' is required for this action")
  }
  if(!require("geonapi")){
    stop("Package 'geonapi' is required for this action")
  }
  
  #shortcut for gn config
  GN <- config$software$geonetwork
  
  #to insert or update a metadata into a geonetwork.
  #An insert has to be done in 2 operations (the insert itself, and the privilege setting to "publish" it either to a restrained group or to public)
  #An update has to be done based on the internal Geonetwork id (that can be queried as well
  privileges <- if(!is.null(options$privileges)) options$privileges else  c("view","dynamic","featured")
  metaFile <- file.path("metadata", paste0(entity$id,".rds"))
  md <- readRDS(metaFile)
  if(is(md, "ISOMetadata")) privileges <- privileges[privileges!="featured"]
  metaId <- GN$get(entity$id, by = "uuid", output = "id")
  if(is.null(metaId)){
    #insert metadata (once inserted only visible to the publisher)
    group <- if(!is.null(options$group)) options$group else "1"
    category <- if(!is.null(options$category)) options$category else "datasets"
    created = GN$insertMetadata(geometa = md, group = group, category = category)
    
    #config privileges
    config <- GNPrivConfiguration$new()
    config$setPrivileges("all", privileges)
    GN$setPrivConfiguration(id = created, config = config)
  }else{
    #update a metadata
    updated = GN$updateMetadata(id = metaId, geometa = md)
    
    #config privileges
    gn_config <- GNPrivConfiguration$new()
    gn_config$setPrivileges("all", privileges)
    GN$setPrivConfiguration(id = metaId, config = gn_config)
  }
  
  return(metaId)
}