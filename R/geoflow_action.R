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
#'    packages = list(),
#'    pid_generator = NULL,
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
#'  \item{\code{checkPackages()}}{
#'    Check that all packages required for the action are available, if yes,
#'    import them in the R session, and return a \code{data.frame} giving the 
#'    packages names and version. If one or more packages are unavailable,
#'    an error is thrown and user informed of the missing packages.
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_action <- R6Class("geoflow_action",
  inherit = geoflowLogger,
  public = list(
    id = NA,
    type = NA,
    def = NA,
    packages = list(),
    pid_generator = NULL,
    pid_types = list(),
    fun = NA,
    script = NA,
    options = list(),
    initialize = function(id, type = "", def = "",  
                          packages = list(), 
                          pid_generator = NULL, pid_types = list(),
                          fun = NULL, script = NULL, options = list()){
      self$id <- id
      self$type <- type
      self$def <- def
      self$packages <- packages
      self$pid_generator <- pid_generator
      self$pid_types <- pid_types
      self$fun <- fun
      self$script <- script
      self$options <- options
    },
    
    #checkPackages
    checkPackages = function(){
      #check package dependencies
      self$INFO(sprintf("Check package dependencies for action '%s'", self$id))
      out_pkgs <- try(check_packages(self$packages))
      if(class(out_pkgs)=="try-error"){
        errMsg <- sprintf("One or more packages are not imported although required for action '%s'", self$id)
        self$ERROR(errMsg)
        stop(errMsg)
      }else{
        if(is.null(out_pkgs)){
          self$INFO(sprintf("No additional package required for action '%s':", self$id))
        }else{
          self$INFO(sprintf("The following packages have been imported for action '%s':", self$id))
          print(out_pkgs)
        }
      }
    },
    
    #isPidGenerator
    isPIDGenerator = function(){
      return(!is.null(self$pid_generator))
    },
    
    #exportPIDs
    exportPIDs = function(config, entities){
      if(!self$isPIDGenerator()) return(FALSE);
      config$logger.info(sprintf("Exporting reference list of '%s' DOIs to job directory for action", self$pid_generator))
      out_pids <- do.call("rbind", lapply(entities, function(entity){
        out_entity <- data.frame(
          Identifier = entity$identifiers[["id"]], 
          Status = entity$status[[self$pid_generator]],
          stringsAsFactors = FALSE
        )
        for(pid_type in names(self$pid_types)){
          out_entity[,self$pid_types[[pid_type]]] <- entity$identifiers[[sprintf("%s_%s_to_save", self$pid_generator, pid_type)]]
        }
        return(out_entity)
      }))
      readr::write_csv(out_pids, file.path(getwd(),"metadata", paste0(self$pid_generator, "_pids.csv")))
      
      config$logger.info(sprintf("Exporting source entities table enriched with '%s' DOIs", self$pid_generator))
      src_entities <- config$src_entities
      src_entities$Identifier <- sapply(1:nrow(src_entities), function(i){
        identifier <- src_entities[i, "Identifier"]
        if(!endsWith(identifier, .geoflow$LINE_SEPARATOR)) identifier <- paste0(identifier, .geoflow$LINE_SEPARATOR)
        if(regexpr(.geoflow$LINE_SEPARATOR, identifier)>0) return(identifier)
        if(out_pids[i,"Status"] == "published") return(identifier)
        for(pid_type in names(self$pid_types)){
          if(!endsWith(identifier, .geoflow$LINE_SEPARATOR)) identifier <- paste0(identifier, .geoflow$LINE_SEPARATOR)
          identifier <- paste0(identifier, pid_type, ":", out_pids[i,self$pid_types[[pid_type]]]) 
        }
        return(identifier)
      })
      readr::write_csv(src_entities, file.path(getwd(),"metadata",paste0(self$pid_generator, "_entities_with_pids_for_publication.csv")))
      
      config$logger.info(sprintf("Exporting workflow configuration for '%s' DOI publication", self$pid_generator))
      src_config <- config$src_config
      
      
      #modifying handler to csv/exported table - to see with @juldebar
      src_config$metadata$entities$handler <- "csv"
      src_config$metadata$entities$source <- paste0(self$pid_generator, "_entities_with_pids_for_publication.csv")
      
      #altering publish option
      pid_action <- src_config$actions[sapply(src_config$actions, function(x){x$id==self$id})][[1]]
      pid_publish <- if(!is.null(pid_action$options$publish)) pid_action$options$publish else FALSE
      invisible(lapply(1:length(src_config$actions), function(i){
        action <- src_config$actions[[i]]
        if(action$id==self$id){
          src_config$actions[[i]]$options$publish <<- if(pid_publish) FALSE else TRUE
        }
      }))
      
      #modifying global option
      src_config$options$skipFileDownload <- if(pid_publish) FALSE else TRUE
      
      #export modified config
      jsonlite::write_json(
        src_config, file.path(getwd(),"metadata",paste0(self$pid_generator, "_geoflow_config_for_publication.json")),
        auto_unbox = TRUE, pretty = TRUE
      )
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
      packages = list("geometa"),
      fun = geometa_create_iso_19115,
      options = list(
        doi = list(desc = "Add entity DOI - if defined - as metadata identifier and online resource", default = FALSE),
        doi_thumbnail = list(desc = "if option 'doi' is true and this option enabled, a DOI thumbnail will be added", default = FALSE),
        inspire = list(desc = "Validates ISO 19139 metadata with INSPIRE reference validator", default = FALSE),
        logo = list(desc = "Add configure profile logo(s) - if defined - as metadata thumbnail(s)", default = FALSE),
        addfeatures = list(desc = "Add entity data features - if defined - as metadata bounding polygon(s)", default = FALSE),
        featureId = list(desc = "ID of entity data features used to identify bounding polygon(s) with option 'addfeatures'", default = NA),
        subject_geography = list(desc = "Identifier of the subject handling a Geographic coverage.", default = "geography")
      )
    ),
    geoflow_action$new(
      id = "geometa-create-iso-19110",
      type = "Metadata production",
      def = "Produce an ISO 19110/19139 metadata object",
      packages = list("geometa"),
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
      packages = list("ows4R"),
      fun = ows4R_publish_iso_19139,
      options = list(
        geometa_inspire = list(desc = "Validates ISO 19139 metadata with INSPIRE reference validator before publication", default = FALSE)
      )
    ),
    geoflow_action$new(
      id = "geonapi-publish-iso-19139",
      type = "Metadata publication",
      def = "Publish/Update an ISO/OGC 19139 metadata object with GeoNetwork API",
      packages = list("geonapi"),
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
      packages = list("geosapi"),
      fun = geosapi_publish_ogc_services
    ),
    geoflow_action$new(
      id = "zen4R-deposit-record",
      type = "Data publication",
      def = "Deposits/Publish data and/or metadata in the Zenodo infrastructure",
      pid_generator = "zenodo",
      pid_types = list(
        doi = "DOI_for_version",
        conceptdoi = "DOI_for_allversions"
      ),
      packages = list("zen4R"),
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
      id = "atom4R-dataverse-deposit-record",
      type = "Data publication",
      def = "Deposits/Publish data and/or metetadata on a Dataverse using the Sword API",
      pid_generator = "dataverse",
      pid_types = list(
        doi = "DOI_for_version"
      ),
      packages = list("atom4R"),
      fun = atom4R_dataverse_deposit_record,
      options = list(
        depositWithFiles = list(desc = "Indicates if the action is uploading files", default = FALSE),
        publish = list(desc = "Indicates if the action should publish the deposit. Requires 'depositWithFiles' set to TRUE", default = FALSE),
        deleteOldFiles = list(desc = "Indicates if the action should delete old files prior upload new files", default = TRUE),
        update_metadata = list(desc = "For an existing deposit, indicates if metadata elements should be updated", default = TRUE),
        update_files = list(desc = "For an existing deposit, indicates if files should be updated", default = TRUE)
      )
    ),
    geoflow_action$new(
      id = "dataone-upload-datapackage",
      type = "Data publication",
      def = "Uploads a data package to a DataOne metacat node",
      pid_generator = "dataone",
      pid_types = list(
        packageId = "PackageId"
      ),
      packages = list("mime", "datapack", "dataone"),
      fun = dataone_upload_datapackage,
      options = list()
    ),
    geoflow_action$new(
      id = "sf-write-generic",
      type = "Data writing",
      def = "Import features data into several formats",
      packages = list("sf", "DBI", "RSQLite", "RPostgres"),
      fun = sf_write_generic,
      options = list(
        type=list(desc = "format to convert", default = NA),
        createIndexes=list(desc = "create indexes for columns", default = FALSE)
      )
    ),
    geoflow_action$new(
      id = "sf-write-dbi",
      type = "Data writing",
      def = "Import features data into Postgres/Postgis",
      packages = list("sf", "DBI", "RSQLite", "RPostgres"),
      fun = sf_write_dbi,
      options = list(
        createIndexes=list(desc = "create indexes for columns", default = FALSE)
      )
    ),
    geoflow_action$new(
      id = "sf-write-shp",
      type = "Data writing",
      def = "Import features data and zip files",
      packages = list("sf"),
      fun = sf_write_shp
    ),
    geoflow_action$new(
      id = "eml-create-eml",
      type = "Metadata production",
      def = "Produce an EML metadata object",
      packages = list("EML", "emld"),
      fun = eml_create_eml,
      options = list(
        subject_taxonomy = list(desc = "Identifier of the subject handling the Taxonomic coverage.", default = "taxonomy")
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
        pid_generator = action$isPIDGenerator(),
        packages = paste(action$packages, collapse=","),
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
      default = sapply(action$options, function(x){paste0(x$default, collapse=",")}),
      stringsAsFactors = FALSE
    )
    row.names(out) <- 1:nrow(out)
  }else{
    out <- data.frame(name = character(0), definition = character(0))
  }
  return(out)
}
