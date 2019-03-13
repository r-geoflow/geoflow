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

#'register_geoflow_actions
#'@export
register_geoflow_actions <- function(){
  
  objs <- list(
    geoflow_action$new(
      id = "geometa-create-iso-19115",
      type = "Metadata production",
      def = "Produce an ISO/OGC 19115/19139 metadata object",
      fun = geometa_create_iso_19115
    ),
    geoflow_action$new(
      id="ows4R-publish-iso-19139",
      type = "Metadata publication",
      def = "Publish/Update an ISO/OGC 19139 metadata object using OGC CSW Protocol",
      fun = ows4R_publish_iso_19139
    ),
    geoflow_action$new(
      id = "geonapi-publish-iso-19139",
      type = "Metadata publication",
      def = "Publish/Update an ISO/OGC 19139 metadata object with GeoNetwork API",
      fun = geonapi_publish_iso_19139
    ),
    geoflow_action$new(
      id = "zen4R-deposit-record",
      type = "Data publication",
      def = "Deposits/Publish data and/or metadata in the Zenodo infrastructure",
      fun = zen4R_deposit_record
    )
  )
  .geoflow$actions <- objs
}

#'list_geoflow_actions
#'@export
list_geoflow_actions <- function(raw = FALSE){
  actions <- .geoflow$actions 
  if(raw){
    actions <- sapply(actions, function(x){x$id})
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
