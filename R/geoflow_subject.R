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
#'   subject$setKey("theme")
#'   subject$setName("General")
#'   subject$setUri("http://somelink/general")
#'   subject$addKeyword("keyword1", "http://somelink/keyword1")
#'   subject$addKeyword("keyword2", "http://somelink/keyword2")
#'   subject$addKeyword("keyword3", "http://somelink/keyword3")
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_subject <- R6Class("geoflow_subject",
  public = list(
    #'@field key subject key
    key = NULL,
    #'@field name subject name
    name = NULL,
    #'@field uri subject URI
    uri = NULL,
    #'@field dates subject date(s)
    dates = list(),
    #'@field keywords subject keywords
    keywords = list(),
    
    #'@description Initializes an object of class \link{geoflow_subject}
    #'@param str a character string to initialize from, using key-based syntax
    initialize = function(str = NULL){
      if(!is.null(str)){
        subject_kvp <- extract_kvp(str)
        key <- subject_kvp$key
        self$setKey(key)
        uri <- attr(key, "uri")
        name <- attr(key, "description")
        attr(key, "uri") <- NULL
        attr(key, "description") <- NULL
        self$setUri(uri)
        self$setName(name)
        invisible(lapply(subject_kvp$values, self$addKeyword))
      }
    },
    
    #'@description Sets subject key
    #'@param key key
    setKey = function(key){
      self$key <- key
    },
    
    #'@description Sets subject name
    #'@param name name
    setName = function(name){
      self$name <- name
    },
    
    #'@description Sets subject URI
    #'@param uri uri
    setUri = function(uri){
      self$uri <- uri
    },
    
    #'@description Sets date
    #'@param dateType type of date
    #'@param date date
    setDate = function(dateType, date){
      self$dates[[dateType]] <- date
    },
    
    #'@description Adds a keyword
    #'@param keyword keyword
    #'@param uri keyword URI. Default is \code{NULL}
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
    
    #'@description Get keywords associated with the subject
    #'@param pretty whether the output has to prettyfied as \code{data.frame}
    #'@return the list of keywords as \code{list} of \code{geoflow_keyword} objects or \code{data.frame}
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