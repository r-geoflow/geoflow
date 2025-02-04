#' @name geoflow
#' @aliases geoflow-package
#' @aliases geoflow
#' 
#' @import R6
#' @import methods
#' @importFrom utils sessionInfo
#' @importFrom utils capture.output
#' @importFrom utils read.csv write.csv
#' @importFrom utils unzip
#' @importFrom utils packageDescription
#' @importFrom utils packageVersion
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils download.file
#' @importFrom benchmarkme get_ram
#' @importFrom benchmarkme get_cpu
#' @importFrom zip zipr
#' @importFrom png readPNG
#' @importFrom curl curl_fetch_memory
#' @importFrom whisker whisker.render
#' @import dotenv
#' @import uuid
#' @import httr
#' @import mime
#' @import jsonlite
#' @import yaml
#' @import XML
#' @import xml2
#' @import mime
#' @import plyr
#' @import readr
#' @import arrow
#' @import sf
#' @import sfarrow
#' @importFrom terra rast
#' @import geometa
#' @import geosapi
#' @import geonapi
#' @import ows4R
#' 
#' @title Tools to Orchestrate and Run (Meta)Data Management Workflows
#' @description  Provides an engine to manage Spatial Data Infrastructures (SDI) and to orchestrate, run and automate 
#' geospatial (meta)data workflows in compliance with international standards (ISO, OGC, INSPIRE) with a range of actions such as 
#' data upload in spatial databases, publication in GeoServer and metadata publication in GeoNetwork. It includes actions 
#' to manage domain-specific resources usch ecological metadata (EML) and its publication on tools such as Metacat. It also allows to
#' publish data on cloud data infrastructures such as Zenodo or Dataverse. Through a pivot metadata model, geoflow allows to manage 
#' a unique source dataset metadata while offering a way to target various repositories for their publication. The execution of several 
#' actions will allow to cross-share (meta)data resources in each action performed, offering a way to bind resources between each other 
#' (eg. reference Zenodo DOIS in Geonetwork/Geoserver metadata, reference Geonetwork/Geoserver links in Zenodo or EML metadata). The use of
#' standardized configuration files allows fully reproducible workflows, in compliance with FAIR (Findable, Accessible, Interoperable, Reusable) 
#' principles.
#'
#'  
#'@author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
"_PACKAGE"