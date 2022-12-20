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
    #'@param kvp an object of class \link{geoflow_kvp}
    initialize = function(str = NULL, kvp = NULL){
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
      }else if(!is.null(kvp)){
        #key
        subject_key <- kvp$key
        key_attrs <- attributes(subject_key)
        attributes(subject_key) <- NULL
        self$setKey(subject_key)
        #name
        name <- key_attrs$description
        if(!is.null(name)){
          key_desc_attrs <- key_attrs[startsWith(names(key_attrs),"description") & names(key_attrs)!="description"]
          for(attr_name in names(key_desc_attrs)){
            locale <- unlist(strsplit(attr_name,"description#"))[2]
            attr(name, paste0("locale#",locale)) <- key_desc_attrs[[attr_name]]
          }
          self$setName(name)
        }
        #uri
        uri <- key_attrs$uri
        if(!is.null(uri)){
          key_uri_attrs <- key_attrs[startsWith(names(key_attrs),"uri") & names(key_attrs)!="uri"]
          for(attr_name in names(key_uri_attrs)){
            locale <- unlist(strsplit(attr_name, "uri#"))[2]
            attr(uri, paste0("locale#",locale)) <- key_uri_attrs[[attr_name]]
          }
          self$setUri(uri)
        }
        #keywords
        for(i in 1:length(kvp$values)){
          kwd = kvp$values[[i]]
          kwd_uri <- attr(kwd,"uri")
          attributes(kwd) <- NULL
          
          val_locale_attrs <- attributes(kvp$values)
          for(attr_name in names(val_locale_attrs)){
            locale_value <- val_locale_attrs[[attr_name]][[i]]
            if(!is.null(kwd_uri)) attr(kwd_uri, attr_name) <- attr(locale_value, "uri")
            attributes(locale_value) <- NULL
            attr(kwd, attr_name) <- locale_value
          }
          self$addKeyword(keyword = kwd, uri = kwd_uri)
        }
        
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