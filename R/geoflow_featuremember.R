#' geoflow_featuremember
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_featuremember
#' @title Geoflow feature type class
#' @description This class models a feature type to be executed by geoflow
#' @keywords contact
#' @return Object of \code{\link{R6Class}} for modelling a dictionary feature type member
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate a geoflow_featuremember object
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_featuremember <- R6Class("geoflow_featuremember",
   public = list(
     id = NULL,
     type = list(),
     code = NULL,
     name = NULL,
     def = NULL,
     defSource = NULL,
     registerId = NULL,
     initialize = function(type, code, name, def, defSource = NULL, 
                           registerId = NULL){
       if(!type %in% c("attribute", "variable")){
         stop("The member type should be either 'attribute' or 'variable'")
       }
       self$id = code
       self$type = type
       self$code = code
       self$name = name
       self$def = def
       self$defSource = defSource
       self$registerId = registerId
     }
   )                                  
)