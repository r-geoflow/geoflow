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
#'    download = function(resource, file, path, software, unzip){},
#'    list = function(resource, software){}
#'  )
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_data_accessor <- R6Class("geoflow_data_accessor",
  inherit = geoflowLogger,
  public = list(
    #'@field id accessor ID
    id = NULL,
    #'@field software_type accessor software type
    software_type = NA,
    #'@field definition accessor definition
    definition = NULL,
    #'@field packages list of packages required for the accessor
    packages = list(),
    #'@field download a download function handler
    download = NULL,
    #'@field list a function handler to list resources in case of a data directory
    list = NULL,
    
    #'@description Initializes the data ccessor
    #'@param id accessor ID
    #'@param software_type accessor software type
    #'@param definition accessor definition
    #'@param packages list of packages required for the accessor
    #'@param download download function handler
    #'@param list list function handler
    initialize = function(id = NULL, software_type = NULL, definition, 
                          packages = list(), download, list = NULL){
      self$setId(id)
      if(!is.null(software_type)) self$setSoftwareType(software_type)
      self$setPackages(packages)
      self$setDefinition(definition)
      self$setDownload(download)
      self$setList(list)
    },
    
    #'@description Sets accessor ID
    #'@param id accessor ID to set
    setId = function(id){
      self$id <- id
    },
    
    #'@description Sets software type
    #'@param software_type software type
    setSoftwareType = function(software_type){
      self$software_type <- software_type
    },
    
    #'@description Sets list of packages required for the accessor
    #'@param packages a vecto of package names
    setPackages = function(packages){
      self$packages <- packages
    },
    
    #'@description Sets accessor definition
    #'@param definition accessor definition
    setDefinition = function(definition){
      self$definition <- definition
    },
    
    #'@description Set download handler (a function with arguments \code{resource},
    #' \code{file}, \code{path}, \code{unzip} (TRUE/FALSE) and optional \code{software})
    #'@param download an object of class \code{function}
    setDownload = function(download){
      self$download = download
    },
    
    #'@description Set list handler (a function with no arguments)
    #'@param list an object of class \code{function}
    setList = function(list){
      self$list = list
    },
    
    #'@description Check that all packages required for the software are available, if yes,
    #'    import them in the R session, and return a \code{data.frame} giving the 
    #'    packages names and version. If one or more packages are unavailable,
    #'    an error is thrown and user informed of the missing packages.
    checkPackages = function(){
      self$INFO(sprintf("Check package dependencies for data accessor '%s'", self$id))
      out_pkgs <- try(check_packages(self$packages))
      if(is(out_pkgs,"try-error")){
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
      download = function(resource, file, path, software = NULL, unzip = TRUE){
        cat(sprintf("[geoflow][INFO] Default HTTP(S) data accessor: Download data '%s' from '%s' to '%s'\n", file, resource, path))
        download.file(resource, destfile = path, mode = "wb")
        if(unzip & endsWith(path, "zip")){
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
      download = function(resource, file, path, software = NULL, unzip = TRUE){
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
        if(unzip & endsWith(path, "zip")){
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
      download = function(resource, file, path, software = NULL, unzip = TRUE){
        if(startsWith(resource, "https://dx.doi.org/")) resource <- unlist(strsplit(resource, "https://dx.doi.org/"))[2]
        cat(sprintf("[geoflow] Zenodo data accessor: Download data '%s' from '%s' to '%s'\n", file, resource, path))
        zen4R::download_zenodo(doi = resource, files = file, path = dirname(path))
        file.rename(from = file.path(getwd(), file), to = path)
        if(unzip & endsWith(path, "zip")){
          utils::unzip(zipfile = path, exdir = getwd(), unzip = getOption("unzip"))
        }
      }
    ),
    #-------------------------------------------------------------------------------------------------------
    #DATAVERSE
    #------------------------------------------------------------------------------------------------------- 
    geoflow_data_accessor$new(
      id = "dataverse",
      software_type = "dataverse",
      definition = "A Dataverse public data accessor",
      packages = list("dataverse"),
      download = function(resource, file, path, software = NULL, unzip = TRUE){
        if(is.null(software)){
          errMsg <- sprintf("[geoflow] Dataverse data accessor requires a 'dataverse' software declaration in the geoflow configuration\n")
          cat(errMsg)
          stop(errMsg)
        }
        if(startsWith(resource, "https://dx.doi.org/")) resource <- unlist(strsplit(resource, "https://dx.doi.org/"))[2]
        cat(sprintf("[geoflow] Dataverse data accessor: Download data '%s' from '%s' to '%s'\n", file, resource, path))
        file_raw <- dataverse::get_file(file = file, dataset = resource, server = software$server)
        writeBin(file_raw, file.path(getwd(), file))
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
      download = function(resource, file, path, software = NULL, unzip = TRUE){
        if(is.null(software)){
          errMsg <- sprintf("[geoflow] D4science Storage Hub data accessor requires a 'd4storagehub' software declaration in the geoflow configuration\n")
          cat(errMsg)
          stop(errMsg)
        }
        
        cat(sprintf("[geoflow] D4science Storage Hub data accessor: Download data '%s' from '%s' to '%s'\n", file, resource, path))
        link = try(software$getPublicFileLink(resource))
        if(!is(link, "try-error")){
          cat(sprintf("[geoflow][INFO] D4science Storage Hub resource ID: %s\n", link))
          download.file(url = link, destfile = path)
        }else{
          errMsg<-sprintf("No D4science Storage Hub resource ID for resource/file '%s'\n", resource)
          cat(errMsg)
          stop(errMsg)
        }
        
        if(unzip & endsWith(path, "zip")){
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
      download = function(resource, file, path, software = NULL, unzip = TRUE){

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
      packages = list("thredds","httr","XML"),
      download = function(resource, file, path, software = NULL, unzip = TRUE){
        if(is.null(software)){
          errMsg <- sprintf("[geoflow] Thredds data accessor requires a 'thredds' software declaration in the geoflow configuration\n")
          cat(errMsg)
          stop(errMsg)
        }
        decode_path<-resource
        if(!is.null(decode_path)) decode_path<-unlist(strsplit(resource,"/"))
        dataset<-file
        nodes<-if(length(decode_path)>1){decode_path[1:length(decode_path)-1]} else{NULL}
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
        }else if(dataset %in% child$get_dataset_names(xpath=".//d1:dataset")){
          data<-child$get_datasets(dataset,xpath=".//d1:dataset")[[dataset]]
        }else{
          errMsg <- sprintf("[geoflow] dataset '%s' not existing and  can't be download\n",dataset)
          cat(errMsg)
          stop(errMsg)
        }
        dataset_dest<-file.path(getwd(),paste0(dataset,".nc"))
        uri<-XML::parseURI(software$url)
        base_uri<-paste0(uri$scheme,"://",uri$server)
        http<-unlist(sapply(names(software$list_services()), function(x) if(software$list_services()[[x]]["serviceType"]=="HTTPServer") software$list_services()[[x]]["base"]))[1]
        if(!is.null(http)){
          dataset_uri<-paste0(base_uri,http,data$url)
          if(httr::GET(dataset_uri)$status=="200") download.file(url = dataset_uri, destfile = dataset_dest,mode="wb")
        }
      }
    ),
    #-------------------------------------------------------------------------------------------------------
    #OPENAPI
    #------------------------------------------------------------------------------------------------------- 
    geoflow_data_accessor$new(
      id = "openapi",
      software_type = "openapi",
      definition = "An OpenAPI data accessor",
      packages = list("rapiclient"),
      download = function(resource, file, path, software = NULL, unzip = TRUE){
        
        if(is.null(software)){
          errMsg <- sprintf("[geoflow] OpenAPI data accessor requires a 'openapi' software declaration in the geoflow configuration\n")
          cat(errMsg)
          stop(errMsg)
        }
        
        openapi_query <- jsonlite::read_json(resource)
        if(!all(names(openapi_query) %in% c("api_method_name", "api_method_args"))){
          errMsg <- "[geoflow] OpenAPI data accessor requires a source json file that includes the 'api_method_name' to invoke and the 'api_method_args' to apply to this method"
          cat(errMsg)
          stop(errMsg)
        }
        
        my_arg_names <- names(openapi_query$api_method_args)
        api_method_arg_names <- names(formals(software[[openapi_query$api_method_name]]))
        if(!all(my_arg_names %in% api_method_arg_names)){
          errMsg <- sprintf("[geoflow] OpenAPI data accessor: arguments [%s] are not valid for method '%'", 
                            paste0(setdiff(my_arg_names, api_method_arg_names),collapse=","),
                            openapi_query$api_method_name)
          cat(errMsg)
          stop(errMsg)
        }
        
        req <- do.call(software[[openapi_query$api_method_name]], openapi_query$api_method_args)
        if(httr::status_code(req)== 200){
          writeLines(content(req, type = "text"), path)
        }else{
          errMsg <- "[geoflow] OpenAPI data accessor: bad request"
          cat(errMsg)
          stop(errMsg)
        }
        
      }
    ),
    #-------------------------------------------------------------------------------------------------------
    #OCS
    #------------------------------------------------------------------------------------------------------- 
    geoflow_data_accessor$new(
      id = "ocs",
      software_type = "ocs",
      definition = "An OCS API-based (Owncloud/Nextcloud) data accessor",
      packages = list("ocs4R"),
      download = function(resource, file, path, software = NULL, unzip = TRUE){
        if(is.null(software)){
          errMsg <- sprintf("[geoflow] OCS data accessor requires a 'ocs' software declaration in the geoflow configuration\n")
          cat(errMsg)
          stop(errMsg)
        }
        cat(sprintf("[geoflow] OCS data accessor: Download data '%s' from '%s' to '%s'\n", file, resource, path))
        software$downloadFile(relPath = dirname(resource), filename = basename(resource), outdir = dirname(path))
        if(unzip & endsWith(path, "zip")){
          utils::unzip(zipfile = path, exdir = getwd(), unzip = getOption("unzip"))
        }
      },
      list = function(resource, software = NULL){
        outfiles <- list()
        if(is.null(software)){
          errMsg <- sprintf("[geoflow] OCS data accessor requires a 'ocs' software declaration in the geoflow configuration\n")
          cat(errMsg)
          stop(errMsg)
        }
        files <- try(software$listFiles(resource))
        if(!is(files, "try-error")){
          if(nrow(files)>0){
            outfiles <- file.path(resource, files$name)
          }
        }else{
          errMsg <- sprintf("[geoflow] Error while listing files for remote OCS cloud folder '%s'\n", resource)
          cat(errMsg)
          stop(errMsg)
        }
        return(outfiles)
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

