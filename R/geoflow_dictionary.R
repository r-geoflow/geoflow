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
     source = NULL,
     featuretypes = list(),
     registers = list(),
     initialize = function(){},
     
     #setSource
     setSource = function(source){
       self$source <- source
     },
     
     #getFeatureTypes
     getFeatureTypes = function(){
        return(self$featuretypes)
     },
     
     #getFeatureTypeById
     getFeatureTypeById = function(id){
        out <- NULL
        if(length(self$featuretypes)>0){
           fts <- self$featuretypes[sapply(self$featuretypes, function(x){x$id == id})]
           if(length(fts)>0) out <- fts[[1]]
        }
        return(out)
     },
     
     #addFeatureType
     addFeatureType = function(ft){
       if(!is(ft, "geoflow_featuretype")){
         stop("The feature type should be an object of class 'geoflow_featuretype'")
       }
       if(!(ft$id %in% sapply(self$featuretypes, function(x){x$id}))){
         self$featuretypes <- c(self$featuretypes, ft)
       }
     },
     
     #getRegisters
     getRegisters = function(){
        return(self$registers)
     },
     
     #getRegisterById
     getRegisterById = function(id){
        out <- NULL
        if(length(self$registers)>0){
           regs <- self$registers[sapply(self$registers, function(x){x$id == id})]
           if(length(regs)>0) out <- regs[[1]]
        }
        return(out)
     },
     
     #addRegister
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