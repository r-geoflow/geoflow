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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_profile <- R6Class("geoflow_profile",
 public = list(
   #'@field id profile id
   id = NULL,
   #'@field name profile name
   name = NULL,
   #'@field project profile project
   project = NULL,
   #'@field organization profile organization
   organization = NULL,
   #'@field logos profile logo(s)
   logos = list(),
   #'@field mode mode of execution (Default is "raw")
   mode = "raw",
   
   #'@description Initializes an object of class \link{geoflow_profile}
   initialize = function(){},
   
   #'@description Sets profile ID
   #'@param id id
   setId = function(id){
      self$id <- id
   },
   
   #'@description Sets profile name
   #'@param name name
   setName = function(name){
      self$name <- name
   },
   
   #'@description Sets profile project
   #'@param project project
   setProject = function(project){
     self$project <- project
   },
   
   #'@description Sets profile organization
   #'@param organization organization
   setOrganization = function(organization){
     self$organization <- organization
   },
   
   #'@description Adds a profile organization
   #'@param logo logo
   addLogo = function(logo){
     self$logos <- c(self$logos, logo) 
   },
   
   #'@description Sets profile mode
   #'@param mode profile mode
   setMode = function(mode){
      self$mode <- mode
   }
   
 )                                  
)