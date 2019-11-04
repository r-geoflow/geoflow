#'geoflow_action
#'@export
geoflow_action <- R6Class("geoflow_action",
  public = list(
    id = NA,
    type = NA,
    def = NA,
    fun = NA,
    script = NA,
    options = list(),
    initialize = function(id, type = "", def = "", fun = NULL, script = NULL, options = list()){
      self$id <- id
      self$type <- type
      self$def <- def
      self$fun <- fun
      self$script <- script
      self$options <- options
    }
  )
)

#'register_actions
#'@export
register_actions <- function(){
  
  objs <- list(
    geoflow_action$new(
      id = "geometa-create-iso-19115",
      type = "Metadata production",
      def = "Produce an ISO/OGC 19115/19139 metadata object",
      fun = geometa_create_iso_19115,
      options = list(
        doi = list(desc = "Add entity DOI - if defined - as metadata identifier and online resource", default = FALSE),
        inspire = list(desc = "Validates ISO 19139 metadata with INSPIRE reference validator", default = FALSE),
        logo = list(desc = "Add configure profile logo(s) - if defined - as metadata thumbnail(s)", default = FALSE),
        addfeatures = list(desc = "Add entity data features - if defined - as metadata bounding polygon(s)", default = FALSE),
        featureId = list(desc = "ID of entity data features used to identify bounding polygon(s) with option 'addfeatures'", default = NA)
      )
    ),
    geoflow_action$new(
      id = "geometa-create-iso-19110",
      type = "Metadata production",
      def = "Produce an ISO 19110/19139 metadata object",
      fun = geometa_create_iso_19110,
      options = list(
        doi = list(desc = "Add entity DOI - if defined - as metadata identifier and online resource", default = FALSE)
      )
    ),
    geoflow_action$new(
      id="ows4R-publish-iso-19139",
      type = "Metadata publication",
      def = "Publish/Update an ISO/OGC 19139 metadata object using OGC CSW Protocol",
      fun = ows4R_publish_iso_19139,
      options = list(
        geometa_inspire = list(desc = "Validates ISO 19139 metadata with INSPIRE reference validator before publication", default = FALSE)
      )
    ),
    geoflow_action$new(
      id = "geonapi-publish-iso-19139",
      type = "Metadata publication",
      def = "Publish/Update an ISO/OGC 19139 metadata object with GeoNetwork API",
      fun = geonapi_publish_iso_19139,
      options = list(
        geometa_inspire = list(desc = "Validates ISO 19139 metadata with INSPIRE reference validator before publication", default = FALSE),
        privileges = list(desc = "Geonetwork privileges to set for the metadata to be published", default = c("view","dynamic","featured")),
        group = list(desc = "Geonetwork user group to which the metadata should be associated", default = "1"),
        category = list(desc = "Category of metadata resources to which the metadata record should be associated", default = "datasets")
      )
    ),
    geoflow_action$new(
      id = "geosapi-publish-ogc-services",
      type = "Data publication",
      def = "Publish vector data to GeoServer OGC web-services (WMS/WFS)",
      fun = geosapi_publish_ogc_services
    ),
    geoflow_action$new(
      id = "zen4R-deposit-record",
      type = "Data publication",
      def = "Deposits/Publish data and/or metadata in the Zenodo infrastructure",
      fun = zen4R_deposit_record,
      options = list(
        depositWithFiles = list(desc = "Indicates if the action is uploading files", default = FALSE),
        publish = list(desc = "Indicates if the action should publish the deposit. Requires 'depositWithFiles' set to TRUE", default = FALSE),
        deleteOldFiles = list(desc = "Indicates if the action should delete old files prior upload new files", default = TRUE),
        update_metadata = list(desc = "For an existing deposit, indicates if metadata elements should be updated", default = TRUE),
        update_files = list(desc = "For an existing deposit, indicates if files should be updated", default = TRUE),
        communities = list(desc = "One or more communities to which the deposit should be associated", default = NA)
      )
    )
  )
  .geoflow$actions <- objs
}

#'list_actions
#'@export
list_actions <- function(raw = FALSE){
  actions <- .geoflow$actions 
  if(raw){
    return(actions)
  }else{
    actions <- do.call("rbind", lapply(actions, function(action){
      return(data.frame(
        id = action$id,
        type = action$type,
        definition = action$def,
        stringsAsFactors = FALSE
      ))
    }))
  }
  return(actions)
}

#'list_action_options
#'@export
list_action_options <- function(id){
  out <- NULL
  actions <- list_actions(raw = TRUE)
  action <- actions[sapply(actions, function(x){x$id == id})]
  if(length(action)==0) stop(sprintf("No action with id '%s'!", id))
  action <- action[[1]]
  if(length(action$options)>0){
    out <- data.frame(
      name = names(action$options),
      definition = sapply(action$options, function(x){x$desc}),
      default = sapply(action$options, function(x){x$default}),
      stringsAsFactors = FALSE
    )
    row.names(out) <- 1:nrow(out)
  }else{
    out <- data.frame(name = character(0), definition = character(0))
  }
  return(out)
}
