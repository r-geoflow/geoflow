#' geoflow_relation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_relation
#' @title Geoflow relation class
#' @description This class models an relation
#' @keywords relation
#' @return Object of \code{\link{R6Class}} for modelling an relation
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   relation <- geoflow_relation$new()
#'   relation$setKey("wms")
#'   relation$setLink("http://somelink/wms")
#'   relation$setName("layername")
#'   relation$setDescription("layer description")
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(str)}}{
#'    This method is used to instantiate a geoflow_relation object
#'  }
#'  \item{\code{setKey(key)}}{
#'    Set relation key
#'  }
#'  \item{\code{setLink(link)}}{
#'    Set relation link
#'  }
#'  \item{\code{setName(name)}}{
#'    Set relation name
#'  }
#'  \item{\code{setDescription(description)}}{
#'   Set relation description
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
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