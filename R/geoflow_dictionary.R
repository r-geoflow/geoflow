#' geoflow_dictionary
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_dictionary
#' @title Geoflow dictionary class
#' @description This class models a dictionary to be executed by geoflow
#' @keywords contact
#' @return Object of \code{\link{R6Class}} for modelling a dictionary
#' @format \code{\link{R6Class}} object.
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate a geoflow_dictionary_element object
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_dictionary <- R6Class("geoflow_dictionary",
   public = list(
     #'@field source dictionary source, object of class \code{data.frame}
     source = NULL,
     #'@field featuretypes list of objects of class \code{geoflow_featuretype}
     featuretypes = list(),
     #'@field registers list of objects of class \code{geoflow_register}
     registers = list(),
     
     #'@description Initializes a \link{geoflow_dictionary} object
     initialize = function(){},
     
     #'@description Sets dictionnary source
     #'@param source object of class \code{data.frame}
     setSource = function(source){
       self$source <- source
     },
     
     #'@description Get the list of \link{geoflow_featuretype} defined in the dictionary
     #'@return a \code{list} of \code{geoflow_featuretype}
     getFeatureTypes = function(){
        return(self$featuretypes)
     },
     
     #'@description Get an object of class \link{geoflow_featuretype} given an ID
     #'@param id id
     #'@return an object of class \link{geoflow_featuretype}, otherwise \code{NULL}
     getFeatureTypeById = function(id){
        out <- NULL
        if(length(self$featuretypes)>0){
           fts <- self$featuretypes[sapply(self$featuretypes, function(x){x$id == id})]
           if(length(fts)>0) out <- fts[[1]]
        }
        return(out)
     },
     
     #'@description Adds a feature type to the dictionnary
     #'@param ft object of class \link{geoflow_featuretype}
     addFeatureType = function(ft){
       if(!is(ft, "geoflow_featuretype")){
         stop("The feature type should be an object of class 'geoflow_featuretype'")
       }
       if(!(ft$id %in% sapply(self$featuretypes, function(x){x$id}))){
         self$featuretypes <- c(self$featuretypes, ft)
       }
     },
     
     #'@description Get the list of registers associated with the dictionnary
     #'@return a list of \link{geoflow_register} objects
     getRegisters = function(){
        return(self$registers)
     },
     
     #'@description Get register by ID
     #'@param id id
     #'@return an object of class \link{geoflow_register}, otherwise \code{NULL}
     getRegisterById = function(id){
        out <- NULL
        if(length(self$registers)>0){
           regs <- self$registers[sapply(self$registers, function(x){x$id == id})]
           if(length(regs)>0) out <- regs[[1]]
        }
        return(out)
     },
     
     #'@description Adds a register to the dictionnary
     #'@param register object of class \link{geoflow_register}
     addRegister = function(register){
        if(!is(register, "geoflow_register")){
           stop("The argument should be an object of class 'geoflow_register'")
        }
        if(!register$id %in% sapply(self$registers, function(x){x$id})){
           self$registers <- c(self$registers, register)
        }
     }
   )                                  
)