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
#' @return Object of \code{\link{R6Class}} for modelling a dimension
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   dimension <- geoflow_dimension$new()
#'   dimension$setLongName("longname")
#'   dimension$setResolution(uom="s',value=1)
#'   dimension$setSize(10)
#'   dimension$setValues(c(1,2,3))
#'   dimension$setMinValue(1)
#'   dimension$setMaxValue(3)
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new)}}{
#'    This method is used to instantiate a geoflow_dimension object
#'  }
#'  \item{\code{setLongName(longName)}}{
#'    Set longname
#'  }
#'  \item{\code{setResolution(uom, value)}}{
#'    Sets resolution (with step and unit)
#'  }
#'  \item{\code{setSize(size)}}{
#'    Set size 
#'  }
#'  \item{\code{setValues(values)}}{
#'    Sets values
#'  }
#'  \item{\code{setMinValue(minValue)}}{
#'    Set minimal value
#'  }
#'  \item{\code{setMaxValue(maxValue)}}{
#'    Set maximal value
#'  }
#'  }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_dimension <- R6Class("geoflow_dimension",
                           public = list(
                             longName = NULL,
                             resolution = list(),
                             size = NULL,
                             values = NULL,
                             minValue = NULL,
                             maxValue = NULL,
                             initialize = function(){},

                             #setLongName
                             setLongName = function(longName){
                               self$longName <- longName
                             },
                             
                             #setResolution
                             setResolution = function(uom,value){
                               self$resolution <- list(uom=uom,value=value)
                             },
                                                          
                             #setSize
                             setSize = function(size){
                               self$size <- size
                             },
                             
                             #setValues
                             setValues = function(values){
                               self$values <- values
                             },
                             
                             #setMinValue
                             setMinValue = function(minValue){
                               self$minValue <- minValue
                             },
                             
                             #setMaxValue
                             setMaxValue = function(maxValue){
                               self$maxValue <- maxValue
                             }
                           )                                  
)

