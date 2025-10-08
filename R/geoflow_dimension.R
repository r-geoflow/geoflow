#' geoflow_dimension
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_dimension
#' @title Geoflow dimension class
#' @description This class models a dimension
#' @keywords dimension
#' @return Object of \code{\link[R6]{R6Class}} for modelling a dimension
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'  \dontrun{
#'   dimension <- geoflow_dimension$new()
#'   dimension$setLongName("longname")
#'   dimension$setResolution(uom="s",value=1)
#'   dimension$setSize(10)
#'   dimension$setValues(c(1,2,3))
#'   dimension$setMinValue(1)
#'   dimension$setMaxValue(3)
#'  }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_dimension <- R6Class("geoflow_dimension",
 public = list(
   #'@field longName dimension longName
   longName = NULL,
   #'@field resolution dimension resolution
   resolution = list(),
   #'@field size dimension size
   size = NULL,
   #'@field values dimension values
   values = NULL,
   #'@field minValue dimension min value
   minValue = NULL,
   #'@field maxValue dimension max value
   maxValue = NULL,
   
   #'@description Initializes the \link{geoflow_dimension}
   initialize = function(){},

   #'@description Sets the dimension long name
   #'@param longName dimension long name
   setLongName = function(longName){
     self$longName <- longName
   },
   
   #'@description Sets the resolution 
   #'@param uom unit of measure
   #'@param value resolution value
   setResolution = function(uom,value){
     self$resolution <- list(uom=uom,value=value)
   },
                                
   #'@description Sets the dimension size
   #'@param size dimension size
   setSize = function(size){
     self$size <- size
   },
   
   #'@description Sets dimension values
   #'@param values dimension values
   setValues = function(values){
     self$values <- values
   },
   
   #'@description Sets dimension min value
   #'@param minValue min value
   setMinValue = function(minValue){
     self$minValue <- minValue
   },
   
   #'@description Sets dimension max value
   #'@param maxValue max value
   setMaxValue = function(maxValue){
     self$maxValue <- maxValue
   }
 )                                  
)
