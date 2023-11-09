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
#'    scope = "global",
#'    types = list("some purpose1", "some purpose2"),
#'    target = "entity",
#'    target_dir = "data",
#'    def = "some definition",
#'    packages = list(),
#'    pid_generator = NULL,
#'    generic_uploader = FALSE,
#'    fun = function(action, entity, config){},
#'    available_options = list(
#'      option_name = list(def = "option description", default = FALSE)
#'    ),
#'    options = list(option_name = TRUE)
#'  )
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_action <- R6Class("geoflow_action",
  inherit = geoflowLogger,
  public = list(
    #'@field id action ID
    id = NA,
    #'@field scope action scope
    scope = NULL,
    #'@field types types of action
    types = list(),
    #'@field def the action definition
    def = NA,
    #'@field target the action target
    target = NA,
    #'@field target_dir the action target directory
    target_dir = NA,
    #'@field packages list of packages required to perform the action
    packages = list(),
    #'@field pid_generator a name referencing the PID generator (if existing)
    pid_generator = NULL,
    #'@field pid_types types of PIDs to generate
    pid_types = list(),
    #'@field generic_uploader whether the action is a generic uploader or not.
    generic_uploader = FALSE,
    #'@field fun a function for the action
    fun = NA,
    #'@field script a script for the action
    script = NA,
    #'@field options action options
    options = list(),
    #'@field available_options a list of available options for the actions
    available_options= list(),
    
    #'@description Initialize a \link{geoflow_action}
    #'@param id action id
    #'@param scope action scope "global" or "local"
    #'@param types action types
    #'@param def action definition
    #'@param target the action target, e.g. "entity"
    #'@param target_dir the action target directory
    #'@param packages list of packages required to perform the action
    #'@param pid_generator a name referencing the PID generator (if existing)
    #'@param pid_types types of PIDs to generate by the action 
    #'@param generic_uploader whether the action is a generic uploader or not.
    #'@param fun action function
    #'@param script action script
    #'@param options action options
    #'@param available_options available options for the action
    initialize = function(id, scope = "global", types = list(), def = "", 
                          target = NA, target_dir = NA,
                          packages = list(), 
                          pid_generator = NULL, pid_types = list(),
                          generic_uploader = FALSE,
                          fun = NULL, script = NULL, options = list(),
                          available_options = list()){
      self$id <- id
      if(!scope %in% c("global", "local")){
        stop("Action should be either of 'global' or 'local' scope")
      }
      self$scope <- scope
      self$types <- types
      self$def <- def
      if(!is.na(target)) if(!target %in% c("entity","job")) stop("Action target should be either 'entity' or 'job'")
      self$target <- target
      self$target_dir <- target_dir
      self$packages <- packages
      self$pid_generator <- pid_generator
      self$pid_types <- pid_types
      self$generic_uploader <- generic_uploader
      self$fun <- fun
      self$script <- script
      self$options <- options
      self$available_options  <- available_options
    },
    
    #'@description Check that all packages required for the action are available, if yes,
    #'    import them in the R session, and return a \code{data.frame} giving the 
    #'    packages names and version. If one or more packages are unavailable,
    #'    an error is thrown and user informed of the missing packages.
    checkPackages = function(){
      #check package dependencies
      self$INFO(sprintf("Check package dependencies for action '%s'", self$id))
      out_pkgs <- try(check_packages(self$packages))
      if(is(out_pkgs,"try-error")){
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
    
    #'@description Runs the action
    #'@param entity entity
    #'@param config config
    run = function(entity, config){
      self$fun(self, entity, config)
    },
    
    #'@description Get action option value
    #'@param option option id
    #'@return the option value, either specified through a workflow, or the default value
    getOption = function(option){
      option_value <- self$options[[option]]
      if(is.null(option_value)){
        option_value <- self$available_options[[option]]$default
      }
      return(option_value)
    },
    
    #'@description Indicates if the action is PID generator
    #'@return \code{TRUE} if the action is a PID generator, \code{FALSE} otherwise
    isPIDGenerator = function(){
      return(!is.null(self$pid_generator))
    },
    
    #'@description Exports PIDs for the action. This function will export the PIDs in several ways. First, a simple CSV file 
    #' including the list of PIDs for each entity, and associated status (eg. draft/release) for the given PID resource. In 
    #' addition, for each metadata entities file, an equivalent metadata table will be produced as CSV to handle entities 
    #' enriched with the PID (added in the "Identifier" column), ready for use as workflow entities input. In addition, a new
    #' configuration file will be produced with name "<pid_generator>_geoflow_config_for_publication.json" turned as ready for
    #' publishing resources with PIDs (eg. publishing deposits in Zenodo).
    #'@param config a \pkg{geoflow} configuration
    #'@param entities one or more objects of class \link{geoflow_entity} 
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
      readr::write_csv(out_pids, file.path(getwd(),self$target_dir, paste0(self$pid_generator, "_pids.csv")))
      
      config$logger.info(sprintf("Exporting source entities table enriched with '%s' DOIs", self$pid_generator))
      
      src_entities <- config$src_entities
      for(i in 1:length(config$src_entities)){
        src_entities = config$src_entities[[i]]
        src_entities$Identifier <- sapply(1:nrow(src_entities), function(i){
          identifier <- src_entities[i, "Identifier"]
          if(!endsWith(identifier, .geoflow$LINE_SEPARATOR)) identifier <- paste0("id:", identifier, .geoflow$LINE_SEPARATOR)
          if(regexpr(.geoflow$LINE_SEPARATOR, identifier)>0 && !endsWith(identifier, .geoflow$LINE_SEPARATOR)) return(identifier)
          if(out_pids[i,"Status"] == "published") return(identifier)
          for(pid_type in names(self$pid_types)){
            if(!endsWith(identifier, .geoflow$LINE_SEPARATOR)) identifier <- paste0(identifier, .geoflow$LINE_SEPARATOR)
            identifier <- paste0(identifier, pid_type, ":", out_pids[i,self$pid_types[[pid_type]]]) 
          }
          return(identifier)
        })
        readr::write_csv(src_entities, file.path(getwd(),self$target_dir,paste0(self$pid_generator, "_entities_",i,"_with_pids_for_publication.csv")))
      }
        
      config$logger.info(sprintf("Exporting workflow configuration for '%s' DOI publication", self$pid_generator))
      src_config <- config$src_config
      
      
      #modifying handler to csv/exported table - to see with @juldebar
      src_config$metadata$entities <- lapply(1:length(src_config$metadata$entities), function(i){
        list(handler = "csv", source = paste0(self$pid_generator, "_entities_",i,"_with_pids_for_publication.csv"))
      })
      
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
      #note: this should be set by user
      #src_config$profile$options$skipDataDownload <- if(pid_publish) FALSE else TRUE
      
      #export modified config
      jsonlite::write_json(
        src_config, file.path(getwd(),self$target_dir,paste0(self$pid_generator, "_geoflow_config_for_publication.json")),
        auto_unbox = TRUE, pretty = TRUE
      )
    },

    #'@description Indicates if the action is a generic uploader
    #'@return \code{TRUE} if the action is a generic uploader, \code{FALSE} otherwise
    isGenericUploader = function(){
      return(self$generic_uploader)
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
      types = list("Metadata production"),
      def = "Produce an ISO/OGC 19115/19139 metadata object",
      target = "entity",
      target_dir = "metadata",
      packages = list("geometa","ows4R"),
      available_options = list(
        use_uuid = list(def = "Use UUID as metadata identifier, if not defined the UUID is pre-generated", class = "logical", default = FALSE),
        doi = list(def = "Add entity DOI - if defined - as metadata identifier and online resource", class = "logical", default = FALSE),
        doi_thumbnail = list(def = "if option 'doi' is true and this option enabled, a DOI thumbnail will be added", class = "logical", default = FALSE),
        inspire = list(def = "Validates ISO 19139 metadata with INSPIRE reference validator", class = "logical", default = FALSE),
        logo = list(def = "Add configure profile logo(s) - if defined - as metadata thumbnail(s)", class = "logical", default = FALSE),
        addfeatures = list(def = "Add entity data features - if defined - as metadata bounding polygon(s)", class = "logical", default = FALSE),
        featureid = list(def = "ID of entity data features used to identify bounding polygon(s) with option 'addfeatures'", class = "character", default = NA),
        subject_geography = list(def = "Identifier of the subject handling a Geographic coverage.", class = "character", default = "geography"),
        include_service_identification = list(def = "Include service identification info metadata block", class = "logical", default = FALSE),
        include_coverage_data_dimension_values = list(def = "Include data dimensions's range values to coverage description", class = "logical", default = FALSE),
        include_coverage_service_dimension_values = list(def = "Include ogc dimensions's range values to coverage description", class = "logical", default = FALSE)
      ),
      fun = source(system.file("actions", "geometa_create_iso_19115.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "geometa-create-iso-19110",
      types = list("Metadata production"),
      def = "Produce an ISO 19110/19139 metadata object",
      target = "entity",
      target_dir = "metadata",
      packages = list("geometa"),
      available_options = list(
        doi = list(def = "Add entity DOI - if defined - as metadata identifier and online resource", class = "logical", default = FALSE),
        exclude_attributes = list(def = "Attributes that should be excluded from the ISO 19110 production", class = "character", choices = list(), add_choices = TRUE, multiple = TRUE, default = c()),
        exclude_attributes_not_in_dictionary = list(def = "Enable to exclude all attributes/variables not referenced as dictionary/featuretype", class="logical", default = FALSE),
        exclude_values_for_attributes = list(def = "Attribute names for which listed values should not be produced", class = "character", choices = list(), add_choices = TRUE, multiple = TRUE, default = c()),
        extra_attributes = list(def = "Extra attributes to add as feature catalog attributes although not in data", class = "character", choices = list(), add_choices = TRUE, multiple = TRUE, default = c()),
        default_min_occurs = list(def = "The default min occurs value for feature attributes cardinality", class = "integer", default = 0L),
        default_max_occurs = list(def = "The default max occurs value for feature attribute cardinality", class = "numeric", default = Inf)
      ),
      fun = source(system.file("actions", "geometa_create_iso_19110.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id="ows4R-publish-iso-19139",
      types = list("Metadata publication"),
      def = "Publish/Update an ISO/OGC 19139 metadata object using OGC CSW Protocol",
      target = NA,
      target_dir = NA,
      packages = list("ows4R"),
      available_options = list(
        geometa_inspire = list(def = "Validates ISO 19139 metadata with INSPIRE reference validator before publication", class = "logical", default = FALSE),
        add_metadata_link = list(def = "Adds a link (as online resource) that points to the published metadata (as OGC CSW GetRecordById URL)", class = "logical", default = TRUE)
      ),
      fun = source(system.file("actions", "ows4R_publish_iso_19139.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "geonapi-publish-iso-19139",
      types = list("Metadata publication"),
      def = "Publish/Update an ISO/OGC 19139 metadata object with GeoNetwork API",
      target = NA,
      target_dir = NA,
      packages = list("geonapi"),
      available_options = list(
        geometa_inspire = list(def = "Validates ISO 19139 metadata with INSPIRE reference validator before publication", class = "logical", default = FALSE),
        privileges = list(def = "Geonetwork privileges to set for the metadata to be published", class = "character", choices = c("view","dynamic","download","editing", "notify", "featured"), default = c("view","dynamic","download","featured"), multiple = TRUE),
        group = list(def = "Geonetwork user group to which the metadata should be associated", class = "character", default = "2"),
        category = list(def = "Category of metadata resources to which the metadata record should be associated", class = "character", default = "datasets"),
        add_metadata_link = list(def = "Adds a link (as online resource) that points to the published metadata (as OGC CSW GetRecordById URL)", class = "logical", default = TRUE),
        publish_thumbnails = list(def = "Uploads local thumbnails as attachments and publish them as thumbnails / graphic overviews", class = "logical", default = FALSE),
        create_doi_on_datacite = list(def = "Creates DOIs on DataCite. Requires a DataCite account to be registered in the GeoNetwork administration panel.", class = "logical", default = FALSE)
      ),
      fun = source(system.file("actions", "geonapi_publish_iso_19139.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "geosapi-publish-ogc-services",
      types = list("Data upload", "Data publication", "Metadata publication"),
      def = "Publish data to GeoServer OGC web-services (WMS/WFS/WCS)",
      target = NA,
      target_dir = NA,
      packages = list("geosapi"),
      available_options = list(
        createWorkspace = list(def = "Create workspace if not already existing", class = "logical", default = FALSE),
        createStore = list(def = "Create data/coverage store if not already existing", class = "logical", default = FALSE),
        store_description = list(def = "Specify a description for the new data/coverage store", class = "character", default = ""),
        map_thumbnail_template = list(
          def = "Specify a Mustache/whisker template for a WMS GetMap request to be propagated in metadata (eg. ISO 19115) when the action is used. Only active when enrich_with_relations'/'enrich_with_relation_wms_thumbnail' are TRUE", class = "character",
          default = "{geoserver_url}/{workspace}/ows?service=WMS&version=1.1.0&request=GetMap&layers={layer}&bbox={bbox}&width=600&height=300&srs=EPSG:{srid}&format=image/png"
        ),
        enrich_with_relations = list(def = "When enabled, enrichs entity with OGC relations", class = "logical", default = TRUE),
        enrich_with_relation_wms = list(def = "When enabled, enrichs entity with a base WMS link relation", class = "logical", default = TRUE),
        enrich_with_relation_wms_thumbnail = list(def = "When enabled, enrichs entity with a WMS-based thumbnail relation", class = "logical", default = TRUE),
        describe_wms_relation_with_category = list(def = "When enabled, the WMS main relation description will be appended with the mention 'Map access'", class = "logical", default = TRUE),
        describe_wms_relation_with_ogc_service_description = list(def = "When enabled, the WMS main relation description will be appended with the mention 'OGC Web Map Service (WMS)'", class = "logical", default = TRUE),
        enrich_with_relation_wfs = list(def = "When enabled, enrichs entity with a base WFS link relation (applies to 'vector' only)", class = "logical", default = TRUE),
        enrich_with_relation_wfs_download_links = list(def = "When enabled, enrichs entity with WFS format-specific (GML, GeoJSON, SHAPE-ZIP, CSV) links for download purpose (applies to 'vector' only).", class = "logical", default = TRUE),
        describe_wfs_relation_with_category = list(def = "When enabled, the WFS relations description will be appended with the mention 'Data (features) access' for the base WFS link or 'Data download' in case of download links", class = "logical", default = TRUE),
        describe_wfs_relation_with_ogc_service_description = list(def = "When enabled, the WFS relations description will be appended with the mention 'OGC Web Feature Service (WFS)'", class = "logical", default = TRUE),
        describe_wfs_relation_with_format = list(def = "When enabled, the WFS download relations description will be appended with the mention of the download format", class = "logical", default = TRUE),
        enrich_with_relation_wcs = list(def = "When enabled, enrichs entity with a base WCS link relation (applies to 'grid' only)", class = "logical", default = TRUE),
        enrich_with_relation_wcs_download_links = list(def = "When enabled, enrichs entity with WCS format-specific links for download purpose (applies to 'grid' only). Only GeoTIFF at now.", class = "logical", default = TRUE),
        describe_wcs_relation_with_category = list(def = "When enabled, the WCS relations description will be appended with the mention 'Data (coverage) access' for the base WCS link or 'Data download' in case of download links", class = "logical", default = TRUE),
        describe_wcs_relation_with_ogc_service_description = list(def = "When enabled, the WCS relations description will be appended with the mention 'OGC Web Coverage Service (WCS)'", class = "logical", default = TRUE),
        describe_wcs_relation_with_format = list(def = "When enabled, the WCS download relations description will be appended with the mention of the download format", class = "logical", default = TRUE),
        overwrite_upload = list(def = "When set to TRUE (default), in case a layer already exists, data upload will be re-done (if upload is set to true in entity Data).", class = "logical", default = TRUE),
        overwrite_layer = list(def = "When set to TRUE (default), in case a layer already exists, layer will be republished.", class = "logical", default = TRUE),
        overwrite = list(def = "When set to TRUE (default), in case a layer already exists, data upload will be re-done (if upload is set to true in entity Data), and layer will be re-published. This option preveals over options 'overwrite_upload' and 'overwrite_layer'", class = "logical", default = TRUE)
      ),
      fun = source(system.file("actions", "geosapi_publish_ogc_services.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "geonode4R-publish-ogc-services",
      types = list("Data upload", "Data publication", "Metadata publication"),
      def = "Publish data to GeoNode OGC web-services (WMS/WFS/WCS)",
      target = NA,
      target_dir = NA,
      packages = list("geonode4R"),
      available_options = list(),
      fun = source(system.file("actions", "geonode4R_publish_ogc_services.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "zen4R-deposit-record",
      types = list("Data upload", "Data publication", "Metadata publication", "DOI assignment"),
      def = "Deposits/Publish data and/or metadata in the Zenodo infrastructure",
      target = "job",
      target_dir = "zenodo",
      pid_generator = "zenodo",
      pid_types = list(
        doi = "DOI_for_version",
        conceptdoi = "DOI_for_allversions"
      ),
      generic_uploader = TRUE,
      packages = list("zen4R"),
      available_options = list(
        depositWithFiles = list(def = "Indicates if the action is uploading files", class = "logical", default = FALSE),
        depositDataPattern = list(def = "A regular expression to filter data files to upload in Zenodo", class = "character", default = ""),
        depositMetadataPattern = list(def = "A regular expression to filter metadata files to upload in Zenodo", class = "character", default = ""),
        zipEachDataFile = list(def = "Indicates if each data file should be zipped (to be used in case of large data files", class = "logical", default = FALSE),
        publish = list(def = "Indicates if the action should publish the deposit. Requires 'depositWithFiles' set to TRUE", class = "logical", default = FALSE),
        strategy = list(def = "Strategy to use when handling published records, either 'newversion' (default) or 'edition'", class = "character", choices = list("newversion", "edition"), default = "newversion"),
        deleteOldFiles = list(def = "Indicates if the action should delete old files prior upload new files", class = "logical", default = TRUE),
        update_metadata = list(def = "For an existing deposit, indicates if metadata elements should be updated", class = "logical", default = TRUE),
        update_files = list(def = "For an existing deposit, indicates if files should be updated", class = "logical", default = TRUE),
        communities = list(def = "One or more communities to which the deposit should be associated", class = "character", choices = list(), add_choices = TRUE, multiple = TRUE, default = NA)
      ),
      fun = source(system.file("actions", "zen4R_deposit_record.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "atom4R-dataverse-deposit-record",
      types = list("Data upload", "Data publication", "Metadata publication", "DOI assignment"),
      def = "Deposits/Publish data and/or metetadata on a Dataverse using the Sword API",
      target = "job",
      target_dir = "dataverse",
      pid_generator = "dataverse",
      pid_types = list(
        doi = "DOI_for_version"
      ),
      generic_uploader = TRUE,
      packages = list("atom4R"),
      available_options = list(
        depositWithFiles = list(def = "Indicates if the action is uploading files", class = "logical", default = FALSE),
        publish = list(def = "Indicates if the action should publish the deposit. Requires 'depositWithFiles' set to TRUE", class = "logical", default = FALSE),
        deleteOldFiles = list(def = "Indicates if the action should delete old files prior upload new files", class = "logical", default = TRUE),
        update_metadata = list(def = "For an existing deposit, indicates if metadata elements should be updated", class = "logical", default = TRUE),
        update_files = list(def = "For an existing deposit, indicates if files should be updated", class = "logical", default = TRUE)
      ),
      fun = source(system.file("actions", "atom4R_dataverse_deposit_record.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "dataone-upload-datapackage",
      types =  list("Data upload", "Data publication", "Metadata publication", "DOI assignment"),
      def = "Uploads a data package to a DataOne metacat node",
      target = "job",
      target_dir = "dataone",
      pid_generator = "dataone",
      pid_types = list(
        packageId = "PackageId"
      ),
      packages = list("mime", "datapack", "dataone"),
      available_options = list(),
      fun = source(system.file("actions", "dataone_upload_datapackage.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "sf-write-generic",
      types = list("Data writing", "Data upload"),
      def = "Import features data into several formats",
      target = "entity",
      target_dir = "data",
      packages = list("sf", "DBI", "RSQLite", "RPostgres"),
      available_options = list(
        type=list(def = "format to convert", class = "character", choices = c("shp", "dbtable","csv","gpkg"), default = NA),
        createIndexes=list(def = "create indexes for columns", class = "logical", default = FALSE),
        overwrite=list(def = "Overwrite policy", class = "logical", default = TRUE),
        append=list(def = "Append policy", class = "logical", default = FALSE),
        chunk.size=list(def = "Size of DB upload data chunk. Default is 0L, meaning no chunking is operated.", class = "integer", default = 0L)
      ),
      fun = source(system.file("actions", "sf_write_generic.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "sf-write-dbi",
      types = list("Data writing", "Data upload"),
      def = "Import features data into Postgres/Postgis",
      target = NA,
      target_dir = NA,
      packages = list("sf", "DBI", "RSQLite", "RPostgres"),
      available_options = list(
        createIndexes=list(def = "create indexes for columns", class = "logical",  default = FALSE),
        overwrite=list(def = "Overwrite policy", class = "logical",  default = TRUE),
        append=list(def = "Append policy", class = "logical", default = FALSE),
        chunk.size=list(def = "Size of DB upload data chunk. Default is 0L, meaning no chunking is operated.", class = "integer", default = 0L)
      ),
      fun = source(system.file("actions", "sf_write_dbi.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "sf-write-shp",
      types = list("Data writing"),
      def = "Import features data and zip files",
      target = "entity",
      target_dir = "data",
      packages = list("sf"),
      fun = source(system.file("actions", "sf_write_shp.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "eml-create-eml",
      types = list("Metadata production"),
      def = "Produce an EML metadata object",
      target = "entity",
      target_dir = "metadata",
      packages = list("EML", "emld"),
      available_options = list(
        subject_taxonomy = list(def = "Identifier of the subject handling the Taxonomic coverage.", class = "character", default = "taxonomy")
      ),
      fun = source(system.file("actions", "eml_create_eml.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "d4storagehub4R-upload-data",
      types =  list("Data upload"),
      def = "Upload data/metadata to a D4Science Workspace",
      target = NA,
      target_dir = NA,
      generic_uploader = TRUE,
      packages = list("d4storagehub4R"),
      available_options = list(
        depositWithFiles = list(def = "Indicates if the action is uploading files", class = "logical", default = FALSE),
        otherUploadFolders = list(def = "List of Folders (other than 'data' and 'metadata') to upload and which may contain files which should enrich others actions" , class = "character", choices = list(), add_choices = TRUE, multiple = TRUE, default = c())
      ),
      fun = source(system.file("actions", "d4storagehub4R_upload_data.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "ocs4R-upload-data",
      types = list("Data upload"),
      def = "Upload data to an OCS Cloud (NextCloud/Owncloud) Workspace",
      target = NA,
      target_dir = NA,
      generic_uploader = FALSE,
      packages = list("ocs4R"),
      available_options = list(),
      fun = source(system.file("actions", "ocs4R_upload_data.R", package = "geoflow"))$value
    ),
    geoflow_action$new(
      id = "create-metadata-rmd",
      types =  list("Metadata production"),
      def = "Generate a Markdown out of a entity",
      target = "entity",
      target_dir = "markdown",
      packages = list("rmarkdown"),
      available_options = list(
        template = list(def = "Rmarkdown template", class = "character", default = "generic"),
        output_format = list(def = "output format generate by Rmarkdown template (e.g. 'html','pdf')", class = "character",choices = list("html","pdf","word","odt","rtf","md","github"), add_choices = FALSE, multiple = FALSE, default = "html")
      ),
      fun = source(system.file("actions", "rmarkdown_create_metadata.R", package = "geoflow"))$value
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
        types = paste(action$types, collapse=","),
        definition = action$def,
        target = action$target,
        target_dir = action$target_dir,
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
#' @description \code{list_action_options} lists the options available for a given action supported by geoflow.
#'
#' @usage list_action_options(id, raw)
#' 
#' @param id An action identifier
#' @param raw if raw list should be returned
#' 
#' @return an object of class \code{data.frame} (or \code{list} if raw is TRUE) listing the available action options.
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
list_action_options <- function(id, raw = FALSE){
  out <- NULL
  actions <- list_actions(raw = TRUE)
  action <- actions[sapply(actions, function(x){x$id == id})]
  if(length(action)==0) stop(sprintf("No action with id '%s'!", id))
  action <- action[[1]]
  if(raw) return(action$available_options)
  if(length(action$available_options)>0){
    out <- data.frame(
      name = names(action$available_options),
      definition = sapply(action$available_options, function(x){x$def}),
      default = sapply(action$available_options, function(x){paste0(x$default, collapse=",")}),
      stringsAsFactors = FALSE
    )
    row.names(out) <- 1:nrow(out)
  }else{
    out <- data.frame(name = character(0), definition = character(0))
  }
  return(out)
}
