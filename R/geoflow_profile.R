#' geoflow_profile
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_profile
#' @title Geoflow profile class
#' @description This class models an profile
#' @keywords profile
#' @return Object of \code{\link{R6Class}} for modelling an profile
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   profile <- geoflow_profile$new()
#'   profile$setId("workflow1")
#'   profile$setName("Workflow 1")
#'   profile$setProject("My project")
#'   profile$setOrganization("My organization")
#'   provfile$addLogo("https://via.placeholder.com/300x150.png/09f/fff?text=geoflow")
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate a geoflow_profile object
#'  }
#'  \item{\code{setId(id)}}{
#'    Set identifier
#'  }
#'  \item{\code{setName(name)}}{
#'    Set name
#'  }
#'  \item{\code{setProject(project)}}{
#'    Set project
#'  }
#'  \item{\code{setOrganization(organization)}}{
#'    Set organization
#'  }
#'  \item{\code{addLogo(logo)}}{
#'    Adds a logo
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_profile <- R6Class("geoflow_profile",
 public = list(
   id = NULL,
   name = NULL,
   project = NULL,
   organization = NULL,
   logos = list(),
   initialize = function(){},
   
   #setId
   setId = function(id){
      self$id <- id
   },
   
   #setName
   setName = function(name){
      self$name <- name
   },
   
   #setProject
   setProject = function(project){
     self$project <- project
   },
   
   #setOrganization
   setOrganization = function(organization){
     self$organization <- organization
   },
   
   #addLogo
   addLogo = function(logo){
     self$logos <- c(self$logos, logo) 
   }
   
 )                                  
)