#'geoflow_subject
#'@export
geoflow_subject <- R6Class("geoflow_subject",
  public = list(
    name = NULL,
    url = NULL,
    keywords = list(),
    initialize = function(str = NULL){
      if(!is.null(str)){
        #TODO parse contact from str
      }
    },
    
    #setName
    setName = function(name){
      self$name <- name
    },
    
    #setUrl
    setUrl = function(url){
      self$url <- url
    },
    
    #addKeyword
    addKeyword = function(keyword, uri = NULL){
      if(!is.null(uri)){
        attr(keyword, "uri") <- uri
      }
      if(!any(sapply(self$keywords, function(x){ 
        return(x == keyword & attr(keyword, "uri") == uri) 
      }))){
        self$keywords <- c(self$keywords, keyword)
      }
    }
  )                                  
)