#' geoflow_action
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_action
#' @title Geoflow action class
#' @description This class models an action to be executed by geoflow
#' @keywords action
#' @return Object of \code{\link{R6Class}} for modelling an action
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   action <- geoflow_action$new(
#'    id = "some-id",
#'    type = "some purpose",
#'    def = "some definition",
#'    fun = function(config, entity){},
#'    options = list(
#'      option_name = list(desc = "option description", default = FALSE)
#'    )
#'  )
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(id, type, def, fun, script, options)}}{
#'    This method is used to instantiate a geoflow_action object
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
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

#' @name register_actions
#' @aliases register_actions
#' @title register_actions
#' @description \code{register_actions} registers default geoflow actions
#'
#' @usage register_actions()
#' 
#' @note Function called on load by geoflow
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
register_actions <- function(){
  
  objs <- list(
    geoflow_action$new(
      id = "geometa-create-iso-19115",
      type = "Metadata production",
      def = "Produce an ISO/OGC 19115/19139 metadata object",
      fun = geometa_create_iso_19115,
      options = list(
        doi = list(desc = "Add entity DOI - if defined - as metadata identifier and online resource", default = FALSE),
        doi_thumbnail = list(desc = "if option 'doi' is true and this option enabled, a DOI thumbnail will be added", default = FALSE),
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
        doi = list(desc = "Add entity DOI - if defined - as metadata identifier and online resource", default = FALSE),
        exclude_attributes = list(desc = "Attributes that should be excluded from the ISO 19110 production", default = NA),
        exclude_attributes_not_in_dictionary = list(desc = "Enable to exclude all attributes/variables not referenced as dictionary/featuretype", default = FALSE),
        exclude_values_for_attributes = list(desc = "Attribute names for which listed values should not be produced", default = NA),
        extra_attributes = list(desc = "Extra attributes to add as feature catalog attributes although not in data", default = NA),
        default_min_occurs = list(desc = "The default min occurs value for feature attributes cardinality", default = 1L),
        default_max_occurs = list(desc = "The default max occurs value for feature attribute cardinality", default = Inf)
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
    ),
    geoflow_action$new(
      id = "import_dbi",
      type = "Data publication",
      def = "Import features data into Postgres/Postgis",
      fun = action_import_dbi,
      options = list(
        createIndexes=list(desc = "create indexes for columns", default = FALSE)
      )
       ),
    geoflow_action$new(
      id = "import_shp",
      type = "Data publication",
      def = "Import features data and zip files",
      fun = action_import_shp,
       ),
    geoflow_action$new(
      id = "import_generic",
      type = "Data publication",
      def = "Import features data into several formats",
      fun = action_import_generic,
      options = list(
      type=list(desc = "format to convert", default = NA),
      createIndexes=list(desc = "create indexes for columns", default = FALSE)
      )
    )
  )
  .geoflow$actions <- objs
}

#' @name list_actions
#' @aliases list_actions
#' @title list_actions
#' @description \code{list_actions} lists the actions supported by geoflow.
#'
#' @usage list_actions(raw)
#' 
#' @param raw Default value is \code{FALSE}, meaning the actions will be listed as
#' \code{data.frame}. The output If \code{TRUE} the raw list of \link{geoflow_action} 
#' is returned.
#' 
#' @return an object of class \code{data.frame} (or \code{list} of \link{geoflow_action} if raw = FALSE)
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
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

#' @name list_action_options
#' @aliases list_action_options
#' @title list_action_options
#' @description \code{list_action_options} lists the options of a given action supported by geoflow.
#'
#' @usage list_action_options(id)
#' 
#' @param id An action identifier
#' 
#' @return an object of class \code{data.frame} listing the action options.
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
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
