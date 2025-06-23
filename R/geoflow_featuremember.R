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
     type = NULL,
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
     #'@field registerScript feature member register script
     registerScript = NULL,
     
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
     #'@param registerScript source script providing the register functions. Default is \code{NULL}
     initialize = function(type = "attribute", code = NULL, name = NULL, def = NULL, defSource = NULL,
                           minOccurs = NULL, maxOccurs = NULL, uom = NULL,
                           registerId = NULL, registerScript = NULL){
       if(!(type %in% c("attribute", "variable") | startsWith(type, "gml:"))){
         stop("The member type should be either 'attribute' or 'variable' or be a GML geometry type")
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
       self$registerScript = registerScript
     },
     
     #'@description Converts as data.frame
     #'@return an object of class \link{data.frame}
     asDataFrame = function(){
       return(data.frame(
         MemberCode = if(!is.null(self$code)) self$code else "",
         MemberName = if(!is.null(self$name)) self$name else "",
         MemberType = if(!is.null(self$type)) self$type else "",
         MinOccurs = if(!is.null(self$minOccurs)) self$minOccurs else "",
         MaxOccurs = if(!is.null(self$maxOccurs)) self$maxOccurs else "",
         Definition = if(!is.null(self$def)) self$def else "",
         DefinitionSource = if(!is.null(self$defSource)) self$defSource else "",
         MeasurementUnit = if(!is.null(self$uom)) self$uom else "",
         RegisterId = if(!is.null(self$registerId)) self$registerId else "",
         RegisterScript = if(!is.null(self$registerScript)) self$registerScript else "",
         stringsAsFactors = F
       ))
     }
   )                                  
)