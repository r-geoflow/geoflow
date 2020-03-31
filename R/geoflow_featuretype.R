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
#' @examples
#' \dontrun{
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate a geoflow_featuretype object
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_featuretype <- R6Class("geoflow_featuretype",
  public = list(
    id = NULL,
    members = list(),
    initialize = function(id){
      self$id = id
    },

    #addMember
    addMember = function(fm){
      if(!is(fm, "geoflow_featuremember")){
        stop("The feature member should be an object of class 'geoflow_featuremember'")
      }
      if(!fm$id %in% sapply(self$members, function(x){x$id})){
        self$members <- c(self$members, fm)
      }
    },
    
    #getMembers
    getMembers = function(){
      return(self$members)
    },
    
    #getMemberById
    getMemberById = function(id){
      out <- NULL
      if(length(self$members)>0){
        members <- self$members[sapply(self$members, function(x){x$id == id})]
        if(length(members)>0) out <- members[[1]]
      }
      return(out)
    }
  )                                  
)