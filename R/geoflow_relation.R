#'geoflow_relation
#'@export
geoflow_relation <- R6Class("geoflow_relation",
 public = list(
   key = NULL,
   value = NULL,
   initialize = function(str = NULL){
     if(!is.null(str)){
       #TODO parse contact from str
     }
   },
   
   #setKey
   setKey = function(key){
     self$key <- key
   },
   
   #setValue
   setValue = function(value){
     self$value <- value
   }
 )                                  
)