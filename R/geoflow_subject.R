#' geoflow_subject
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_subject
#' @title Geoflow subject class
#' @description This class models a subject
#' @keywords subject
#' @return Object of \code{\link{R6Class}} for modelling a subject
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   subject <- geoflow_subject$new()
#'   subject$setName("General")
#'   subject$setUri("http://somelink/general")
#'   subject$addKeyword("keyword1", "http://somelink/keyword1")
#'   subject$addKeyword("keyword2", "http://somelink/keyword2")
#'   subject$addKeyword("keyword3", "http://somelink/keyword3")
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new)}}{
#'    This method is used to instantiate a geoflow_subject object
#'  }
#'  \item{\code{setName(name)}}{
#'    Sets name 
#'  }
#'  \item{\code{setUri(uri)}}{
#'    Sets URI
#'  }
#'  \item{\code{setDate(dateType, date)}}{
#'    Sets a date (with a given date type)
#'  }
#'  \item{\code{addKeyword(keyword, uri)}}{
#'    Adds a keyword
#'  }
#'  \item{\code{getKeywords(pretty = FALSE)}}{
#'    Gets the keywords. If \code{pretty} is \code{TRUE}, then the method
#'    returns a \code{data.frame}, else it returns a list of \code{geoflow_keyword}
#'    (default behavior).
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_subject <- R6Class("geoflow_subject",
  public = list(
    name = NULL,
    uri = NULL,
    dates = list(),
    keywords = list(),
    initialize = function(str = NULL){
      if(!is.null(str)){
        subject_kvp <- extract_kvp(str)
        self$setName(subject_kvp$key)
        self$setUri(attr(subject_kvp$key,"uri"))
        invisible(lapply(subject_kvp$values, self$addKeyword))
      }
    },
    
    #setName
    setName = function(name){
      self$name <- name
    },
    
    #setUri
    setUri = function(uri){
      self$uri <- uri
    },
    
    #setDate
    setDate = function(dateType, date){
      self$dates[[dateType]] <- date
    },
    
    #addKeyword
    addKeyword = function(keyword, uri = NULL){
      if(!is.null(uri)){
        attr(keyword, "uri") <- uri
      }
      if(!any(sapply(self$keywords, function(x){
        kwd_added <- x$name == keyword
        if(!is.null(x$uri) & !is.null(attr(keyword,"uri"))){
          kwd_added <- kwd_added & x$uri == attr(keyword, "uri")
        }
        return(kwd_added) 
      }))){
        uri <- attr(keyword,"uri")
        attr(keyword, "uri") <- NULL
        kwd <- geoflow_keyword$new()
        kwd$setName(keyword)
        kwd$setUri(uri)
        self$keywords <- c(self$keywords, kwd)
      }
    },
    
    #getKeywords
    getKeywords = function(pretty = FALSE){
      if(pretty){
        out <- do.call("rbind", lapply(self$keywords, function(kwd){
          kwd.df <- data.frame(
            keyword_name = kwd$name, 
            keyword_uri = ifelse(is.null(kwd$uri),NA,kwd$uri),
            stringsAsFactors=FALSE
          )
          return(kwd.df)
        }))
        return(out)
      }else{
        return(self$keywords)
      }
    }
  )                                  
)