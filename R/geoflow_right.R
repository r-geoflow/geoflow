#'geoflow_right
#'@export
geoflow_right <- R6Class("geoflow_right",
  public = list(
    key = NULL,
    value = NULL,
    initialize = function(str = NULL){
      if(!is.null(str)){
        kvp <- extract_kvp(str)
        self$setKey(kvp$key)
        self$setValue(kvp$values[[1]])
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