#' geoflow_handler
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_handler
#' @title Geoflow handler class
#' @description This class models a content handler. An handler is a method to handle
#' some content (eg entity or contact). It is mainly driven by a function that takes
#' as argument the \code{handler} considered (as self accessible object), a \code{source} 
#' which identifiers the source to be handled, that can be of a different type (eg a URL, a file path) 
#' depending on the handler, and a \code{config} object, as the overall configuration created by geoflow 
#' \code{initWorkflow} function. 
#' @keywords handler
#' @return Object of \code{\link{R6Class}} for modelling a handler
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   handler <- geoflow_handler$new(
#'    id = "some-id",
#'    def = "some definition",
#'    packages = list(),
#'    fun = function(handler, source, config){},
#'    available_options = list()
#'  )
#' }
#' 
#' @note This class is essentially called internally by geoflow to register default handlers
#' for entities and contacts.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_handler <- R6Class("geoflow_handler",
  public = list(
    #'@field id handler id
    id = NA,
    #'@field type handler type (entity,contact,dictionary)
    type = NULL,
    #'@field funders funders
    funders = list(),
    #'@field authors authors
    authors = list(),
    #'@field maintainer maintainer
    maintainer = NULL,
    #'@field def handler definition
    def = NA,
    #'@field packages handler packages
    packages = list(),
    #'@field fun handler function
    fun = NA,
    #'@field script handler script
    script = NA,
    #'@field options options
    options = list(),
    #'@field available_options available options
    available_options = list(),
    #'@field status status
    status = "stable",
    #'@field notes notes
    notes = "",
    
    #'@description Initializes a \link{geoflow_handler}
    #'@param yaml a YAML file
    #'@param id id
    #'@param type type
    #'@param funders funders
    #'@param authors authors
    #'@param maintainer maintainer
    #'@param def def
    #'@param packages list of packages required for the handler
    #'@param fun the handler \code{function} having 2 arguments \code{config} and \code{source}
    #'@param script a handler script
    #'@param options action options
    #'@param available_options available options for the action
    #'@param status status (experimental/stable/deprecated/superseded)
    #'@param notes notes
    initialize = function(yaml = NULL,
                          id = NULL, type = c("entity", "contact", "dictionary"),
                          funders = list(), authors = list(), maintainer = NULL,
                          def = "",  
                          packages = list(), fun = NULL, script = NULL, 
                          options = list(), available_options = list(),
                          status = "stable", notes = ""){
      if(!is.null(yaml)){
        self$fromYAML(yaml)
      }else{
        self$id <- id
        type = match.arg(type)
        self$type = type
        self$funders = funders
        self$authors = authors
        self$maintainer = maintainer
        self$def <- def
        self$packages <- packages
        self$fun <- fun
        self$script <- script
        self$options <- options
        self$available_options <- available_options
        self$status = status
        self$notes = notes
      }
    },
    
    #'@description Reads handler properties from YAML file
    #'@param file file
    fromYAML = function(file){
      yml = yaml::read_yaml(file)
      self$id = yml$id
      self$type = yml$type
      self$funders = yml$funders
      self$authors = yml$authors
      self$maintainer = yml$maintainer
      self$def = yml$def
      self$packages = yml$packages
      self$fun = source(system.file(paste0("metadata/", self$type), yml$fun, package = "geoflow"))$value
      self$status = if(!is.null(yml$status)) yml$status else "<unknown>"
      self$notes = if(!is.null(yml$notes)) yml$notes else ""
      self$available_options = lapply(yml$available_options, function(opt){
        if(is.null(opt$default)){
          opt$default = c()
        }else{
          if(length(opt$default)>1){
            opt$default = unlist(opt$default)
          }else{
            opt$default = switch(opt$default,
                                 "NA" = NA,
                                 "Inf" = Inf,
                                 as(opt$default, opt$class)                  
            )
          }
        }
        return(opt)
      })
    },
    
    #'@description Check that all packages required for the handler are available, if yes,
    #'    import them in the R session, and return a \code{data.frame} giving the 
    #'    packages names and version. If one or more packages are unavailable,
    #'    an error is thrown and user informed of the missing packages.
    checkPackages = function(){
      #check package dependencies
      self$INFO(sprintf("Check package dependencies for handler '%s'", self$id))
      out_pkgs <- try(check_packages(self$packages))
      if(is(out_pkgs,"try-error")){
        errMsg <- sprintf("One or more packages are not imported although required for handler '%s'", self$id)
        self$ERROR(errMsg)
        stop(errMsg)
      }else{
        if(is.null(out_pkgs)){
          self$INFO(sprintf("No additional package required for handler '%s':", self$id))
        }else{
          self$INFO(sprintf("The following packages have been imported for handler '%s':", self$id))
          print(out_pkgs)
        }
      }
    },
    
    #'@description Get handler option value
    #'@param option option id
    #'@return the option value, either specified through a workflow, or the default value
    getOption = function(option){
      option_value <- self$options[[option]]
      if(is.null(option_value)){
        option_value <- self$available_options[[option]]$default
      }
      return(option_value)
    }
  )
)
