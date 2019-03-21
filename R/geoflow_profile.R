#'geoflow_profile
#'@export
geoflow_profile <- R6Class("geoflow_profile",
 public = list(
   project = NULL,
   organization = NULL,
   logos = list(),
   initialize = function(){},
   
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