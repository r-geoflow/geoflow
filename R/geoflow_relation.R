#'geoflow_relation
#'@export
geoflow_relation <- R6Class("geoflow_relation",
 public = list(
   key = NULL,
   link = NULL,
   name = NULL,
   description = NULL,
   initialize = function(str = NULL){
     if(!is.null(str)){
       kvp <- extract_kvp(str)
       self$setKey(kvp$key)
       name <- kvp$values[[1]]
       link <- attr(name, "uri")
       description <- attr(name, "description")
       attr(name, "uri") <- NULL
       attr(name, "description") <- NULL
       self$setLink(link)
       self$setName(name)
       self$setDescription(description)
     }
   },
   
   #setKey
   setKey = function(key){
     self$key <- key
   },
   
   #setLink
   setLink = function(link){
     self$link <- link
   },
   
   #setName
   setName = function(name){
     self$name <- name
   },
   
   #setDescription
   setDescription = function(description){
     self$description <- description
   }
 )                                  
)