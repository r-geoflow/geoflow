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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_featuremember <- R6Class("geoflow_featuremember",
   public = list(
     #'@field id feature member ID
     id = NULL,
     #'@field type feature member type
     type = list(),
     #'@field code feature member code
     code = NULL,
     #'@field name feature member name
     name = NULL,
     #'@field def feature member definition
     def = NULL,
     #'@field defSource feature member definition source
     defSource = NULL,
     #'@field minOccurs feature member minOccurs
     minOccurs = NULL,
     #'@field maxOccurs feature member maxOccurs
     maxOccurs = NULL,
     #'@field uom feature member unit of measure (uom)
     uom = NULL,
     #'@field registerId feature member register ID
     registerId = NULL,
     
     #'@description Initializes a \link{geoflow_featuremember}
     #'@param type type
     #'@param code code
     #'@param name name
     #'@param def definition
     #'@param defSource definition source. Default is \code{NULL}
     #'@param minOccurs minOccurs. Default is \code{NULL}
     #'@param maxOccurs maxOccurs. Default is \code{NULL}
     #'@param uom unit of measure. Default is \code{NULL}
     #'@param registerId ID of the register associated to the feature type. Default is \code{NULL}
     initialize = function(type, code, name, def, defSource = NULL,
                           minOccurs = NULL, maxOccurs = NULL, uom = NULL,
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
       self$minOccurs = minOccurs
       self$maxOccurs = maxOccurs
       self$uom = uom
       self$registerId = registerId
     }
   )                                  
)