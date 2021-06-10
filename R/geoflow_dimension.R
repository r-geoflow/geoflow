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
#'   dimension$setResolution(uom="s',value=1)
#'   dimension$setSize(10)
#'   dimension$setValues(c(1,2,3))
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new)}}{
#'    This method is used to instantiate a geoflow_keyword object
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_dimension <- R6Class("geoflow_dimension",
                           public = list(
                             resolution = list(),
                             size = NULL,
                             values = NULL,
                             initialize = function(){},

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
                             }
                           )                                  
)

addDimension<-