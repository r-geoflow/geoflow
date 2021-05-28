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
      download = function(resource, file, path, software = NULL){
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
      download = function(resource, file, path, software = NULL){
        if(is.null(software)){
          errMsg <- sprintf("[geoflow] Google Drive data accessor requires a 'googledrive' software declaration in the geoflow configuration\n")
          cat(errMsg)
          stop(errMsg)
        }
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
      software_type = NA,
      definition = "A Zenodo public data accessor",
      packages = list("zen4R"),
      download = function(resource, file, path, software = NULL){
        if(startsWith(resource, "https://dx.doi.org/")) resource <- unlist(strsplit(resource, "https://dx.doi.org/"))[2]
        cat(sprintf("[geoflow] Zenodo data accessor: Download data '%s' from '%s' to '%s'\n", file, resource, path))
        zen4R::download_zenodo(doi = resource, files = file, path = dirname(path))
        file.rename(from = file.path(getwd(), file), to = path)
        if(endsWith(path, "zip")){
          utils::unzip(zipfile = path, exdir = getwd(), unzip = getOption("unzip"))
        }
      }
    ),
    #-------------------------------------------------------------------------------------------------------
    #D4SCIENCE STORAGE HUB
    #-------------------------------------------------------------------------------------------------------    
    geoflow_data_accessor$new(
      id = "d4storagehub",
      software_type = "d4storagehub",
      definition = "A D4science Storage Hub data accessor",
      packages = list("d4storagehub4R"),
      download = function(resource, file, path, software = NULL){
        if(is.null(software)){
          errMsg <- sprintf("[geoflow] D4science Storage Hub data accessor requires a 'd4storagehub' software declaration in the geoflow configuration\n")
          cat(errMsg)
          stop(errMsg)
        }
        
        cat(sprintf("[geoflow] D4science Storage Hub data accessor: Download data '%s' from '%s' to '%s'\n", file, resource, path))
        link = try(software$getPublicFileLink(resource))
        if(class(link)!="try-error"){
          cat(sprintf("[geoflow][INFO] D4science Storage Hub resource ID: %s\n", link))
          download.file(url = link, destfile = path)
        }else{
          errMsg<-sprintf("No D4science Storage Hub resource ID for resource/file '%s'\n", resource)
          cat(errMsg)
          stop(errMsg)
        }
        
        if(endsWith(path, "zip")){
          utils::unzip(zipfile = path, exdir = getwd(), unzip = getOption("unzip"))
        }
      }
    ),
    #-------------------------------------------------------------------------------------------------------
    #GBIF
    #------------------------------------------------------------------------------------------------------- 
    geoflow_data_accessor$new(
      id = "gbif",
      software_type = "gbif",
      definition = "A gbif public data accessor",
      packages = list("rgbif"),
      download = function(resource, file, path, software = NULL){

        if(is.null(software)){
          errMsg <- sprintf("[geoflow] Gbif data accessor requires a 'gbif' software declaration in the geoflow configuration\n")
          cat(errMsg)
          stop(errMsg)
        }
        
        query<- readr::read_file(resource)
        cat(sprintf("[geoflow][INFO] gbif Searching in 'gbif' database records corresponding to the query: %s ...\n" , file))        
        data<-rgbif::occ_download(
          body=query,
          user=software$user,pwd=software$pwd,email=software$email
        )
        
        rgbif::occ_download_wait(data)
        download <- rgbif::occ_download_get(data)
        df <- rgbif::occ_download_import(download)
        cat("[geoflow][INFO] gbif Write csv file to data job directory\n")
        readr::write_csv(df,gsub(".json",".csv",path))
        #sapply(query_file$scientificName, function(x) rgbif::name_suggest(x)$data$key[1], USE.NAMES=FALSE))#NB: to convert scientificName to taxonKey
        
      }
    ),
    #-------------------------------------------------------------------------------------------------------
    #THREDDS
    #-------------------------------------------------------------------------------------------------------    
    geoflow_data_accessor$new(
      id = "thredds",
      software_type = "thredds",
      definition = "A Thredds data server accessor",
      packages = list("thredds"),
      download = function(resource, file, path, software = NULL){
        if(is.null(software)){
          errMsg <- sprintf("[geoflow] Thredds data accessor requires a 'thredds' software declaration in the geoflow configuration\n")
          cat(errMsg)
          stop(errMsg)
        }

        decode_path<-unlist(stringr::str_split(resource,pattern="/"))
        dataset<-tail(decode_path,1)
        nodes<-decode_path[1:length(decode_path)-1]
        top_url<-software$url
        child<-software
        for(node in nodes){
          if(node %in% child$get_catalog_names()){
            child<-child$get_catalogs(node)[[node]]
          }else{
            errMsg <- sprintf("[geoflow] node '%s' not existing, dataset '%s' can't be download\n",node,dataset)
            cat(errMsg)
            stop(errMsg)
          }
        }
        datasetNode<-NULL
        if(dataset %in% child$get_dataset_names()){
          data<-child$get_datasets(dataset)[[dataset]]
        }else{
          errMsg <- sprintf("[geoflow] dataset '%s' not existing and  can't be download\n",dataset)
          cat(errMsg)
          stop(errMsg)
        }
        dataset_dest<-file.path(getwd(),paste0(dataset,".nc"))
        dataset_uri<-paste0(unlist(strsplit(top_url,"/catalog.xml"))[1],data$url,".nc")
        cat(sprintf("[geoflow] Thredds data accessor: Download data '%s' from '%s' to '%s'\n", dataset, dataset_uri, dataset_dest))
        download.file(url = dataset_uri, destfile = dataset_dest)
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

