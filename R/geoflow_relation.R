#'geoflow_relation
#'@export
geoflow_relation <- R6Class("geoflow_relation",
 public = list(
   key = NULL,
   link = NULL,
   label = NULL,
   initialize = function(str = NULL){
     if(!is.null(str)){
       kvp <- extract_kvp(str)
       self$setKey(kvp$key)
       label <- kvp$values[[1]]
       link <- attr(label, "uri")
       attr(label, "uri") <- NULL
       self$setLink(link)
       self$setLabel(label)
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
   
   #setLabel
   setLabel = function(label){
     self$label <- label
   }
 )                                  
)