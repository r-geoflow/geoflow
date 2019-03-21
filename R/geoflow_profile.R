#'geoflow_profile
#'@export
geoflow_profile <- R6Class("geoflow_profile",
 public = list(
   project = NULL,
   organization = NULL,
   logo = NULL,
   initialize = function(){},
   
   #setProject
   setProject = function(project){
     self$project <- project
   },
   
   #setOrganization
   setOrganization = function(organization){
     self$organization <- organization
   },
   
   #setLogo
   setLogo = function(logo){
     self$logo <- logo 
   }
   
 )                                  
)