#' geoflow_featuretype
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_featuretype
#' @title Geoflow feature type class
#' @description This class models a feature type to be executed by geoflow
#' @keywords contact
#' @return Object of \code{\link{R6Class}} for modelling a dictionary feature type
#' @format \code{\link{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_featuretype <- R6Class("geoflow_featuretype",
  public = list(
    #'@field id feature type ID
    id = NULL,
    #'@field members feature type members
    members = list(),
    
    #'@description Initializes a \link{geoflow_featuretype}
    #'@param id id
    initialize = function(id = NULL){
      self$id = id
    },

    #'@description Adds a member
    #'@param fm object of class \link{geoflow_featuremember}
    addMember = function(fm){
      if(!is(fm, "geoflow_featuremember")){
        stop("The feature member should be an object of class 'geoflow_featuremember'")
      }
      if(!fm$id %in% sapply(self$members, function(x){x$id})){
        self$members <- c(self$members, fm)
      }
    },
    
    #'@description Get members
    #'@return the list of members, as objects of class \link{geoflow_featuremember}
    getMembers = function(){
      return(self$members)
    },
    
    #'@description Get member by ID
    #'@param id id
    #'@return an object of class \link{geoflow_featuremember}, \code{NULL} otherwise
    getMemberById = function(id){
      out <- NULL
      if(length(self$members)>0){
        members <- self$members[sapply(self$members, function(x){x$id == id})]
        if(length(members)>0) out <- members[[1]]
      }
      return(out)
    },
    
    #'@description Converts to data frame
    #'@return an object of class \link{data.frame}
    asDataFrame = function(){
      if(length(self$members)==0){
        return(data.frame(
          FeatureType = if(!is.null(self$id)) self$id else "",
          MemberCode = "",
          MemberName = "",
          MemberType = "",
          MinOccurs = "",
          MaxOccurs = "",
          Definition = "",
          DefinitionSource = "",
          MeasurementUnit = "",
          Register = "",
          RegisterScript = "",
          stringsAsFactors = F
        ))
      }else{
        return(cbind(
          FeatureType = if(!is.null(self$id)) self$id else "",
          do.call("rbind", lapply(self$members, function(x){
            x$asDataFrame()
          }))
        ))
      }
    }
  )                                  
)