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
#'    types = list("some purpose1", "some purpose2"),
#'    target = "entity",
#'    target_dir = "data",
#'    def = "some definition",
#'    packages = list(),
#'    pid_generator = NULL,
#'    generic_uploader = FALSE,
#'    fun = function(config, entity){},
#'    options = list(
#'      option_name = list(def = "option description", default = FALSE)
#'    )
#'  )
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(id, types, target, target_dir, def, fun, script, options)}}{
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
    types = list(),
    def = NA,
    target = NA,
    target_dir = NA,
    packages = list(),
    pid_generator = NULL,
    pid_types = list(),
    generic_uploader = FALSE,
    fun = NA,
    script = NA,
    options = list(),
    initialize = function(id, types = list(), def = "", 
                          target = NA, target_dir = NA,
                          packages = list(), 
                          pid_generator = NULL, pid_types = list(),
                          generic_uploader = FALSE,
                          fun = NULL, script = NULL, options = list()){
      self$id <- id
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
      readr::write_csv(out_pids, file.path(getwd(),self$target_dir, paste0(self$pid_generator, "_pids.csv")))
      
      config$logger.info(sprintf("Exporting source entities table enriched with '%s' DOIs", self$pid_generator))
      
      src_entities <- config$src_entities
      for(i in 1:length(config$src_entities)){
        src_entities = config$src_entities[[i]]
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
        readr::write_csv(src_entities, file.path(getwd(),self$target_dir,paste0(self$pid_generator, "_entities_",i,"_with_pids_for_publication.csv")))
      }
        
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
      src_config$profile$options$skipFileDownload <- if(pid_publish) FALSE else TRUE
      
      #export modified config
      jsonlite::write_json(
        src_config, file.path(getwd(),self$target_dir,paste0(self$pid_generator, "_geoflow_config_for_publication.json")),
        auto_unbox = TRUE, pretty = TRUE
      )
    },
    
    #isGenericUploader
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
      packages = list("geometa"),
      fun = geometa_create_iso_19115,
      options = list(
        doi = list(def = "Add entity DOI - if defined - as metadata identifier and online resource", class = "logical", default = FALSE),
        doi_thumbnail = list(def = "if option 'doi' is true and this option enabled, a DOI thumbnail will be added", class = "logical", default = FALSE),
        inspire = list(def = "Validates ISO 19139 metadata with INSPIRE reference validator", class = "logical", default = FALSE),
        logo = list(def = "Add configure profile logo(s) - if defined - as metadata thumbnail(s)", class = "logical", default = FALSE),
        addfeatures = list(def = "Add entity data features - if defined - as metadata bounding polygon(s)", class = "logical", default = FALSE),
        featureId = list(def = "ID of entity data features used to identify bounding polygon(s) with option 'addfeatures'", class = "character", default = NA),
        subject_geography = list(def = "Identifier of the subject handling a Geographic coverage.", class = "character", default = "geography")
      )
    ),
    geoflow_action$new(
      id = "geometa-create-iso-19110",
      types = list("Metadata production"),
      def = "Produce an ISO 19110/19139 metadata object",
      target = "entity",
      target_dir = "metadata",
      packages = list("geometa"),
      fun = geometa_create_iso_19110,
      options = list(
        doi = list(def = "Add entity DOI - if defined - as metadata identifier and online resource", class = "logical", default = FALSE),
        exclude_attributes = list(def = "Attributes that should be excluded from the ISO 19110 production", class = "character", choices = list(), add_choices = TRUE, multiple = TRUE, default = NA),
        exclude_attributes_not_in_dictionary = list(def = "Enable to exclude all attributes/variables not referenced as dictionary/featuretype", class="logical", default = FALSE),
        exclude_values_for_attributes = list(def = "Attribute names for which listed values should not be produced", class = "character", choices = list(), add_choices = TRUE, multiple = TRUE, default = NA),
        extra_attributes = list(def = "Extra attributes to add as feature catalog attributes although not in data", class = "character", choices = list(), add_choices = TRUE, multiple = TRUE, default = NA),
        default_min_occurs = list(def = "The default min occurs value for feature attributes cardinality", class = "integer", default = 1L),
        default_max_occurs = list(def = "The default max occurs value for feature attribute cardinality", class = "numeric", default = Inf)
      )
    ),
    geoflow_action$new(
      id="ows4R-publish-iso-19139",
      types = list("Metadata publication"),
      def = "Publish/Update an ISO/OGC 19139 metadata object using OGC CSW Protocol",
      target = NA,
      target_dir = NA,
      packages = list("ows4R"),
      fun = ows4R_publish_iso_19139,
      options = list(
        geometa_inspire = list(def = "Validates ISO 19139 metadata with INSPIRE reference validator before publication", class = "logical", default = FALSE)
      )
    ),
    geoflow_action$new(
      id = "geonapi-publish-iso-19139",
      types = list("Metadata publication"),
      def = "Publish/Update an ISO/OGC 19139 metadata object with GeoNetwork API",
      target = NA,
      target_dir = NA,
      packages = list("geonapi"),
      fun = geonapi_publish_iso_19139,
      options = list(
        geometa_inspire = list(def = "Validates ISO 19139 metadata with INSPIRE reference validator before publication", class = "logical", default = FALSE),
        privileges = list(def = "Geonetwork privileges to set for the metadata to be published", class = "character", choices = c("view","dynamic","featured"), default = c("view","dynamic","featured"), multiple = TRUE),
        group = list(def = "Geonetwork user group to which the metadata should be associated", class = "character", default = "1"),
        category = list(def = "Category of metadata resources to which the metadata record should be associated", class = "character", default = "datasets")
      )
    ),
    geoflow_action$new(
      id = "geosapi-publish-ogc-services",
      types = list("Data upload", "Data publication", "Metadata publication"),
      def = "Publish vector data to GeoServer OGC web-services (WMS/WFS)",
      target = NA,
      target_dir = NA,
      packages = list("geosapi"),
      fun = geosapi_publish_ogc_services,
      options = list(
        create_workspace = list(def = "Create Workspace if no already exist", class = "logical", default = FALSE),
        create_datastore = list(def = "Create datastore if no already exist", class = "logical", default = FALSE),
        datastore_description = list(def = "Specify a decription for the new datastore", class = "character", default = "")
      )
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
      fun = zen4R_deposit_record,
      options = list(
        depositWithFiles = list(def = "Indicates if the action is uploading files", class = "logical", default = FALSE),
        publish = list(def = "Indicates if the action should publish the deposit. Requires 'depositWithFiles' set to TRUE", class = "logical", default = FALSE),
        deleteOldFiles = list(def = "Indicates if the action should delete old files prior upload new files", class = "logical", default = TRUE),
        update_metadata = list(def = "For an existing deposit, indicates if metadata elements should be updated", class = "logical", default = TRUE),
        update_files = list(def = "For an existing deposit, indicates if files should be updated", class = "logical", default = TRUE),
        communities = list(def = "One or more communities to which the deposit should be associated", class = "character", choices = list(), add_choices = TRUE, multiple = TRUE, default = NA)
      )
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
      fun = atom4R_dataverse_deposit_record,
      options = list(
        depositWithFiles = list(def = "Indicates if the action is uploading files", class = "logical", default = FALSE),
        publish = list(def = "Indicates if the action should publish the deposit. Requires 'depositWithFiles' set to TRUE", class = "logical", default = FALSE),
        deleteOldFiles = list(def = "Indicates if the action should delete old files prior upload new files", class = "logical", default = TRUE),
        update_metadata = list(def = "For an existing deposit, indicates if metadata elements should be updated", class = "logical", default = TRUE),
        update_files = list(def = "For an existing deposit, indicates if files should be updated", class = "logical", default = TRUE)
      )
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
      fun = dataone_upload_datapackage,
      options = list()
    ),
    geoflow_action$new(
      id = "sf-write-generic",
      types = list("Data writing", "Data upload"),
      def = "Import features data into several formats",
      target = "entity",
      target_dir = "data",
      packages = list("sf", "DBI", "RSQLite", "RPostgres"),
      fun = sf_write_generic,
      options = list(
        type=list(def = "format to convert", class = "character", choices = c("shp", "dbtable","csv","gpkg"), default = NA),
        createIndexes=list(def = "create indexes for columns", class = "logical", default = FALSE),
        overwrite=list(def = "Overwrite policy", class = "logical", default = TRUE),
        append=list(def = "Append policy", class = "logical", default = FALSE),
        chunk.size=list(def = "Size of DB upload data chunk. Default is 0L, meaning no chunking is operated.", class = "integer", default = 0L)
      )
    ),
    geoflow_action$new(
      id = "sf-write-dbi",
      types = list("Data writing", "Data upload"),
      def = "Import features data into Postgres/Postgis",
      target = NA,
      target_dir = NA,
      packages = list("sf", "DBI", "RSQLite", "RPostgres"),
      fun = sf_write_dbi,
      options = list(
        createIndexes=list(def = "create indexes for columns", class = "logical",  default = FALSE),
        overwrite=list(def = "Overwrite policy", class = "logical",  default = TRUE),
        append=list(def = "Append policy", class = "logical", default = FALSE),
        chunk.size=list(def = "Size of DB upload data chunk. Default is 0L, meaning no chunking is operated.", class = "integer", default = 0L)
      )
    ),
    geoflow_action$new(
      id = "sf-write-shp",
      types = list("Data writing"),
      def = "Import features data and zip files",
      target = "entity",
      target_dir = "data",
      packages = list("sf"),
      fun = sf_write_shp
    ),
    geoflow_action$new(
      id = "eml-create-eml",
      types = list("Metadata production"),
      def = "Produce an EML metadata object",
      target = "entity",
      target_dir = "metadata",
      packages = list("EML", "emld"),
      fun = eml_create_eml,
      options = list(
        subject_taxonomy = list(def = "Identifier of the subject handling the Taxonomic coverage.", class = "character", default = "taxonomy")
      )
    ),
    geoflow_action$new(
      id = "d4storagehub4R-upload-data",
      types =  list("Data upload"),
      def = "Upload data/metadata to a D4Science Workspace",
      target = NA,
      target_dir = NA,
      generic_uploader = TRUE,
      packages = list("d4storagehub4R"),
      fun = d4storagehub4R_upload_data,
      options = list(
        depositWithFiles = list(def = "Indicates if the action is uploading files", class = "logical", default = FALSE),
        otherUploadFolders = list(def = "List of Folders (other than 'data' and 'metadata') to upload and which may contain files which should enrich others actions" , class = "character", choices = list(), add_choices = TRUE, multiple = TRUE, default = c())
      )
    ),
    geoflow_action$new(
      id = "create_metadata_Rmd",
      types =  list("Metadata production"),
      def = "Generate a Markdown out of a entity",
      target = "entity",
      target_dir = "markdown",
      packages = list("rmarkdown"),
      fun = create_metadata_Rmd,
      options = list(
        template = list(def = "Rmarkdown template", class = "character", default = "generic"),
        output_format = list(def = "output format generate by Rmarkdown template (e.g. 'html','pdf')", class = "character",choices = list("html","pdf","word","odt","rtf","md","github"), add_choices = FALSE, multiple = FALSE, default = "html")
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
#' @description \code{list_action_options} lists the options of a given action supported by geoflow.
#'
#' @usage list_action_options(id, raw)
#' 
#' @param id An action identifier
#' @param raw if raw list should be returned
#' 
#' @return an object of class \code{data.frame} (or \code{list} if raw is TRUE) listing the action options.
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
  if(raw) return(action$options)
  if(length(action$options)>0){
    out <- data.frame(
      name = names(action$options),
      definition = sapply(action$options, function(x){x$def}),
      default = sapply(action$options, function(x){paste0(x$default, collapse=",")}),
      stringsAsFactors = FALSE
    )
    row.names(out) <- 1:nrow(out)
  }else{
    out <- data.frame(name = character(0), definition = character(0))
  }
  return(out)
}
