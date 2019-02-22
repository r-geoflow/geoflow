#'geoflow_keyword
#'@export
geoflow_keyword <- R6Class("geoflow_keyword",
 public = list(
   name = NULL,
   uri = NULL,
   initialize = function(){},
   
   #setName
   setName = function(name){
     self$name <- name
   },
   
   #setUri
   setUri = function(uri){
     self$uri <- uri
   }
 )                                  
)