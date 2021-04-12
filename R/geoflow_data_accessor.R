#' geoflow_data_accessor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_data_accessor
#' @title Geoflow data accessor class
#' @description This class models a data accessor to be used by geoflow
#' @keywords data access accessor
#' @return Object of \code{\link{R6Class}} for modelling a data accessor
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'   access <- geoflow_data_accessor$new(
#'    id = "some-id",
#'    software_type = "some-software",
#'    definition = "definition",
#'    packages = list(),
#'    handler = function(file, path){}
#'  )
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(id, software_type, definition, packages download)}}{
#'    This method is used to instantiate a geoflow_data_accessor object
#'  }
#'  \item{\code{setId(id)}}{
#'    Set id
#'  }
#'  \item{\code{setSoftwareType(software_type)}}{
#'    Set software type
#'  }
#'  \item{\code{setDefinition(definition)}}{
#'    Set definition
#'  }
#'  \item{\code{setPackages(packages)}}{
#'    Set packages
#'  }
#'  \item{\code{setDownload(download)}}{
#'    Set download handler (a function with arguments \code{file} and \code{path})
#'  }
#'  \item{\code{checkPackages()}}{
#'    Check that all packages required for the software are available, if yes,
#'    import them in the R session, and return a \code{data.frame} giving the 
#'    packages names and version. If one or more packages are unavailable,
#'    an error is thrown and user informed of the missing packages.
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_data_accessor <- R6Class("geoflow_data_accessor",
  inherit = geoflowLogger,
  public = list(
    id = NULL,
    software_type = NA,
    definition = NULL,
    packages = list(),
    download = NULL,
    initialize = function(id = NULL, software_type = NULL, definition, 
                          packages = list(), download){
      self$setId(id)
      if(!is.null(software_type)) self$setSoftwareType(software_type)
      self$setPackages(packages)
      self$setDefinition(definition)
      self$setDownload(download)
    },
    
    #setId
    setId = function(id){
      self$id <- id
    },
    
    #setSoftwareType
    setSoftwareType = function(software_type){
      self$software_type <- software_type
    },
    
    #setPackages
    setPackages = function(packages){
      self$packages <- packages
    },
    
    #setDefinition
    setDefinition = function(definition){
      self$definition <- definition
    },
    
    #setDownload
    setDownload = function(download){
      self$download = download
    },
    
    #checkPackages
    checkPackages = function(){
      self$INFO(sprintf("Check package dependencies for data accessor '%s'", self$id))
      out_pkgs <- try(check_packages(self$packages))
      if(class(out_pkgs)=="try-error"){
        errMsg <- sprintf("One or more packages are not imported although required for data accessor '%s'", self$id)
        self$ERROR(errMsg)
        stop(errMsg)
      }else{
        if(is.null(out_pkgs)){
          self$INFO(sprintf("No additional package required for data accessor '%s':", self$id))
        }else{
          self$INFO(sprintf("The following packages have been imported for data accessor '%s':", self$id))
          print(out_pkgs)
        }
      }
    }
    
  )
)

#' @name register_data_accessors
#' @aliases register_data_accessors
#' @title register_data_accessors
#' @description \code{register_data_accessors} registers default geoflow data accessors
#'
#' @usage register_data_accessors()
#' 
#' @note Function called on load by geoflow
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
register_data_accessors <- function(){
  
  data_accessors <- list(
    #-------------------------------------------------------------------------------------------------------
    #DEFAULT
    #-------------------------------------------------------------------------------------------------------
    geoflow_data_accessor$new(
      id = "default",
      definition = "A default HTTP(S) data accessor",
      download = function(resource, file, path){
        cat(sprintf("[geoflow][INFO] Default HTTP(S) data accessor: Download data '%s' from '%s' to '%s'\n", file, resource, path))
        download.file(resource, destfile = path, mode = "wb")
        if(endsWith(path, "zip")){
          utils::unzip(zipfile = path, exdir = getwd(), unzip = getOption("unzip"))
        }
      }
    ),
    #-------------------------------------------------------------------------------------------------------
    #GOOGLE DRIVE
    #-------------------------------------------------------------------------------------------------------    
    geoflow_data_accessor$new(
      id = "googledrive",
      software_type = "googledrive",
      definition = "A Google Drive data accessor",
      packages = list("googledrive"),
      download = function(resource, file, path){
        cat(sprintf("[geoflow] Google Drive data accessor: Download data '%s' from '%s' to '%s'\n", file, resource, path))
        gdr <- googledrive::drive_get(resource)
        if(!is.null(gdr)){
          cat(sprintf("[geoflow][INFO] Google Drive resource ID: %s\n", gdr$id[1]))
          googledrive::drive_download(file = gdr, path = path)
        }else{
          cat(sprintf("No Google Drive resource ID for resource/file '%s'\n", resource))
        }
        if(endsWith(path, "zip")){
          utils::unzip(zipfile = path, exdir = getwd(), unzip = getOption("unzip"))
        }
      }
    ),
    #-------------------------------------------------------------------------------------------------------
    #ZENODO
    #------------------------------------------------------------------------------------------------------- 
    geoflow_data_accessor$new(
      id = "zenodo",
      software_type = "zenodo",
      definition = "A Zenodo data accessor",
      packages = list("zen4R"),
      download = function(resource, file, path){
        if(startsWith(resource, "https://dx.doi.org/")) resource <- unlist(strsplit(resource, "https://dx.doi.org/"))[2]
        cat(sprintf("[geoflow] Zenodo data accessor: Download data '%s' from '%s' to '%s'\n", file, resource, path))
        zen4R::download_zenodo(doi = resource, files = file, path = dirname(path))
        file.rename(from = file.path(getwd(), file), to = path)
        if(endsWith(path, "zip")){
          utils::unzip(zipfile = path, exdir = getwd(), unzip = getOption("unzip"))
        }
      }
    )
  )
  .geoflow$data_accessors <- data_accessors
}

#' @name list_data_accessors
#' @aliases list_data_accessors
#' @title list_data_accessors
#' @description \code{list_data_accessors} lists the data accessors supported by geoflow.
#'
#' @usage list_data_accessors(raw)
#' 
#' @param raw Default value is \code{FALSE}, meaning the data accessors will be listed as
#' \code{data.frame}. The output If \code{TRUE} the raw list of \link{geoflow_data_accessor} 
#' is returned.
#' 
#' @return an object of class \code{data.frame} (or \code{list} of \link{geoflow_data_accessor} if raw = FALSE)
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
list_data_accessors <- function(raw = FALSE){
  data_accessors <- .geoflow$data_accessors
  if(raw){
    return(data_accessors)
  }else{
    data_accessors <- do.call("rbind", lapply(data_accessors, function(obj){
      obj.out <- data.frame(
        id = obj$id,
        software_type = obj$software_type,
        definition = obj$definition,
        packages = paste(obj$packages, collapse=","),
        stringsAsFactors = FALSE
      )
      return(obj.out)
    }))
  }
  return(data_accessors)
}

