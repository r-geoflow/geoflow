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
    
    #'@description Initializes a \link{geoflow_handler}
    #'@param id id
    #'@param def def
    #'@param packages list of packages required for the handler
    #'@param fun the handler \code{function} having 2 arguments \code{config} and \code{source}
    #'@param script a handler script
    #'@param options action options
    #'@param available_options available options for the action
    initialize = function(id, def = "", packages = list(), fun = NULL, script = NULL, 
                          options = list(), available_options = list()){
      self$id <- id
      self$def <- def
      self$packages <- packages
      self$fun <- fun
      self$script <- script
      self$options <- options
      self$available_options <- available_options
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
