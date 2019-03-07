#'geoflow_kvp
#'@export
geoflow_kvp <- R6Class("geoflow_kvp",
  public = list(
    key = NULL,
    value = NULL,
    initialize = function(str = NULL){
      if(!is.null(str)){
        kvp <- unlist(strsplit(str,":"))
        if(length(kvp)!=2) stop("Invalid Key-value pair string")
        self$setKey(kvp[1])
        self$setValue(kvp[2])
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