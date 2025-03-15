#' geoflow_vocabulary
#' 
#' @name geoflow_vocabulary
#' @title Geoflow vocabulary class
#' @description This class models a vocabulary
#' @docType class
#' @importFrom R6 R6Class
#' @export
#'
#' @keywords vocabulary
#' @return Object of \code{\link[R6]{R6Class}} for modelling a vocabulary
#' @format \code{\link[R6]{R6Class}} object.
#'
geoflow_vocabulary <- R6Class("geoflow_vocabulary",
  public = list(
    
    #'@field id id
    id = NA,
    #'@field def def
    def = NA,
    #'@field uri uri
    uri = NA,
    #'@field software_type software_type
    software_type = NA,
    #'@field software software
    software = NULL,
    
    #'@description Initializes a vocabulary
    #'@param id id
    #'@param def def
    #'@param uri uri
    #'@param software_type software type
    initialize = function(id, def, uri, software_type){
      self$id = id
      self$def = def
      self$uri = uri
      self$software_type = software_type
    },
    
    #'@description Set software
    #'@param software software
    setSoftware = function(software){
      #TODO
    }
  )
)

#' geoflow_skos_vocabulary
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_skos_vocabulary
#' @title Geoflow SKOS vocabulary class
#' @description This class models a SKOS vocabulary
#' @keywords skos vocabulary
#' @return Object of \code{\link{R6Class}} for modelling a SKOS vocabulary
#' @format \code{\link{R6Class}} object.
#'
geoflow_skos_vocabulary <- R6Class("geoflow_skos_vocabulary",
  inherit = geoflow_vocabulary,
  public = list(
    #'@field endpoint endpoint
    endpoint = NA,
    
    #'@description Initializes a vocabulary
    #'@param id id
    #'@param def def
    #'@param uri uri
    #'@param endpoint endpoint
    initialize = function(id, def, uri, endpoint){
      super$initialize(id, def, uri, software_type = "sparql")
      self$endpoint = endpoint
    },
    
    #'@description query
    #'@param str str
    #'@param graphUri graphUri
    #'@param mimetype mimetype
    #'@return the response of the SPARQL query
    query = function(str, graphUri = NULL, mimetype = "text/csv"){
      req_body = list(query = str)
      if(!is.null(graphUri)) req_body$graphUri = graphUri 
      
      req = httr::with_verbose(httr::POST(
        url = self$endpoint,
        encode = "form",
        body = req_body,
        httr::add_headers(
          "Content-Type" = "application/x-www-form-urlencoded",
          "User-Agent" = paste("geoflow", packageVersion("geoflow"), sep = "_"),
          "Accept" = mimetype
        )
      ))
      httr::content(req)
    },
    
    #'@description Ping query
    ping = function(){
      str = "SELECT ?s ?p ?o WHERE { 
                  	?s ?p ?o 
                  } LIMIT 10"
      self$query(str)
    },
    
    #'@description list_collections
    #'@param mimetype mimetype
    #'@return the response of the SPARQL query
    list_collections = function(mimetype = "text/csv", 
                                count_sub_collections = TRUE,
                                count_concepts = TRUE){
      str = "
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

      SELECT ?collection ?label (COUNT(DISTINCT ?subCollection) AS ?count_sub_collections) (COUNT(DISTINCT ?concept) AS ?count_concepts) WHERE {
        ?collection a skos:Collection .
        OPTIONAL { ?collection skos:prefLabel ?label }
        
        # Count sub-collections
        OPTIONAL {
          ?collection skos:member ?subCollection .
          ?subCollection a skos:Collection .
        }
        
        # Count concepts
        OPTIONAL {
          ?collection skos:member ?concept .
          ?concept a skos:Concept .
        }
      }
      GROUP BY ?collection ?label
      "
      out = self$query(str = str, mimetype = mimetype)
      if(!count_sub_collections) out$count_sub_collections = NULL
      if(!count_concepts) out$count_concepts = NULL
      return(out)
    },
    
    #'@description query_from_uri
    #'@param uri uri
    #'@param graphUri graphUri
    #'@param mimetype mimetype
    #'@return the response of the SPARQL query
    query_from_uri = function(uri, graphUri = NULL, mimetype = "text/csv"){
      
      str = paste0(
       "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        SELECT ?concept ?lang ?prefLabel
        WHERE { 
          BIND(<",uri,"> AS ?concept) 
          ?concept skos:prefLabel ?prefLabel . 
          BIND(lang(?prefLabel) AS ?lang) 
        } 
        GROUP BY ?concept ?lang ?prefLabel 
        ORDER BY ?lang "
      )
      
      out = self$query(str = str, graphUri = graphUri, mimetype = mimetype)
      if(nrow(out)>0){
        out = do.call("rbind", lapply(unique(out$lang), function(lang){
          rec= out[out$lang == lang,]
          if(any(is.na(rec$collection))){
            newrec = rec[!is.na(rec$collection),]
            if(nrow(newrec)==0){
              rec = rec[1,]
            }else{
              rec = newrec[1,]
            }
          }
          rec
        }))
      }
    },
    
    #'@description query_from_term
    #'@param term term
    #'@param graphUri graphUri
    #'@param mimetype mimetype
    #'@return the response of the SPARQL query
    query_from_term = function(term, graphUri = NULL, mimetype = "text/csv"){
      
      str = paste0(
        "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
         SELECT ?concept ?lang ?prefLabel ?collection ?collectionLabel
         WHERE {
          ?concept skos:prefLabel ?searchLabel .
          ?concept skos:prefLabel ?prefLabel .
          FILTER (STR(?searchLabel) = \"", term, "\")
          FILTER (LANG(?prefLabel) != \"\")
          BIND (LANG(?prefLabel) AS ?lang)
          
          # Optional block to get the collection and its label
          OPTIONAL {
            ?collection skos:member ?concept .
            OPTIONAL { ?collection skos:prefLabel ?collectionLabel }
          }
          
         }
         GROUP BY ?concept ?lang ?prefLabel ?collection ?collectionLabel
         ORDER BY ?lang "
      )
      
      out = self$query(str = str, graphUri = graphUri, mimetype = mimetype)
      if(nrow(out)>0){
        out = do.call("rbind", lapply(unique(out$lang), function(lang){
          rec= out[out$lang == lang,]
          if(any(is.na(rec$collection))){
            newrec = rec[!is.na(rec$collection),]
            if(nrow(newrec)==0){
              rec = rec[1,]
            }else{
              rec = newrec[1,]
            }
          }
          rec
        }))
      }
      return(out)
    }
  )
)

#' @name register_vocabularies
#' @aliases register_vocabularies
#' @title register_vocabularies
#' @description \code{register_vocabularies} registers default geoflow vocabularies
#'
#' @usage register_vocabularies()
#' 
#' @note Function called on load by geoflow
#' @export
#'
register_vocabularies = function(){
  vocabularies <- list(
    geoflow_skos_vocabulary$new(
      id = "agrovoc",
      def = "AGROVOC Thesaurus",
      uri = "https://aims.fao.org/aos/agrovoc/",
      endpoint = "https://agrovoc.fao.org/sparql"
    ),
    geoflow_skos_vocabulary$new(
      id = "edmo.seadatanet",
      def = "EDMO Seadatanet Thesaurus",
      uri = "https://edmo.seadatanet.org",
      endpoint = "https://edmo.seadatanet.org/sparql/sparql"
    ),
    geoflow_skos_vocabulary$new(
      id = "nvs",
      def = "NERC Vocabulary Server",
      uri = "https://vocab.nerc.ac.uk",
      endpoint = "https://vocab.nerc.ac.uk/sparql/sparql"
    )
  )
  .geoflow$vocabularies <- vocabularies
}

#' @name list_vocabularies
#' @aliases list_vocabularies
#' @title list_vocabularies
#' @description \code{list_vocabularies} lists the vocabularies supported by geoflow.
#'
#' @usage list_vocabularies(raw)
#' 
#' @param raw Default value is \code{FALSE}, meaning the vocabularies will be listed as
#' \code{data.frame}. The output If \code{TRUE} the raw list of \link{geoflow_vocabulary} 
#' is returned.
#' 
#' @return an object of class \code{data.frame} (or \code{list} of \link{geoflow_vocabulary} if raw = FALSE)
#' @export
#'
list_vocabularies <- function(raw = FALSE){
  vocabularies <- .geoflow$vocabularies
  if(raw){
    return(vocabularies)
  }else{
    vocabularies <- do.call("rbind", lapply(vocabularies, function(obj){
      obj.out <- data.frame(
        id = obj$id,
        def = obj$def,
        uri = obj$uri,
        endpoint = if(!is.null(obj$endpoint)) obj$endpoint else NA,
        stringsAsFactors = FALSE
      )
      return(obj.out)
    }))
  }
  return(vocabularies)
}
