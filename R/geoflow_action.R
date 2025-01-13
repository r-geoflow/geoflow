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
    #'@field funders funders
    funders = list(),
    #'@field authors authors
    authors = list(),
    #'@field maintainer maintainer
    maintainer = NULL,
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
    #'@field status status
    status = "stable",
    #'@field notes notes
    notes = "",
    
    #'@description Initialize a \link{geoflow_action}
    #'@param yaml a yaml file
    #'@param id action id
    #'@param funders funders
    #'@param authors authors
    #'@param maintainer maintainer
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
    #'@param status status of the action (experimental, stable, deprecated, superseded)
    #'@param notes notes
    initialize = function(yaml = NULL,
                          id = NULL, 
                          funders = list(), authors = list(), maintainer = NULL,
                          scope = "global", types = list(), def = "", 
                          target = NA, target_dir = NA,
                          packages = list(), 
                          pid_generator = NULL, pid_types = list(),
                          generic_uploader = FALSE,
                          fun = NULL, script = NULL, options = list(),
                          available_options = list(),
                          status = "stable", notes = ""){
      
      if(!is.null(yaml)){
        self$fromYAML(yaml) 
      }else{
        self$id <- id
        self$funders = funders
        self$authors = authors
        self$maintainer = maintainer
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
        self$status = status
        self$notes = notes
      }
    },
    
    #'@description Reads action properties from YAML file
    #'@param file file
    fromYAML = function(file){
      yml = yaml::read_yaml(file)
      self$id = yml$id
      self$funders = yml$funders
      self$authors = yml$authors
      self$maintainer = yml$maintainer
      self$def = yml$def
      self$types = yml$types
      self$target = if(yml$target=="NA") NA else yml$target
      self$target_dir = if(yml$target_dir=="NA") NA else yml$target_dir
      if(!is.na(self$target)) if(!self$target %in% c("entity","job")) stop("Action target should be either 'entity' or 'job'")
      self$packages = yml$packages
      self$pid_generator = if(!is.null(yml$pid_generator)) yml$pid_generator else FALSE
      self$pid_types = yml$pid_types
      self$generic_uploader = if(!is.null(yml$generic_uploader)) yml$generic_uploader else FALSE
      self$fun = source(system.file("actions", yml$fun, package = "geoflow"))$value
      self$available_options = lapply(yml$available_options, function(opt){
        if(is.null(opt$default)){
          opt$default = c()
        }else{
          if(length(opt$default)>1){
            opt$default = unlist(opt$default)
          }else{
            opt$default = switch(as.character(opt$default),
              "NA" = NA,
              "Inf" = Inf,
              as(opt$default, opt$class)                  
            )
          }
        }
        return(opt)
      })
      self$status = if(!is.null(yml$status)) yml$status else "<unknown>"
      self$notes = if(!is.null(yml$notes)) yml$notes else ""
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
  yml_files = list.files(system.file("actions", package = "geoflow"), pattern = "yml")
  objs <- lapply(yml_files, function(file){
    geoflow_action$new(yaml = system.file("actions", file, package = "geoflow"))
  })
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
        status = action$status,
        notes = action$notes,
        maintainer = if(!is.null(action$maintainer$name)){action$maintainer$name}else{ if(action$maintainer$orphaned) "<orphaned>" else "<unknown>" },
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
