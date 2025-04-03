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
    #'@field rdf rdf
    rdf = NULL,
    #'@field endpoint endpoint
    endpoint = NULL,
    
    #'@description Initializes a vocabulary
    #'@param id id
    #'@param def def
    #'@param uri uri
    #'@param endpoint A Sparql endpoint
    #'@param file a RDF file
    initialize = function(id, def, uri, endpoint = NULL, file = NULL){
      super$initialize(id, def, uri, software_type = "sparql")
      self$endpoint = endpoint
      
      #case of RDF resource
      if(!is.null(file)){
        if(startsWith(file, "http")){
          download.file(url = file, destfile = file.path(tempdir(), basename(file)), mode = "wb")
          file = file.path(tempdir(), basename(file))
        }
        if(mime::guess_type(file) %in% c("application/gzip", "application/zip")){
          switch(mime::guess_type(file),
            "application/gzip" = {
              trg_file = file.path(tempdir(), paste0(id, ".rdf"))
              readr::write_lines(readLines(gzfile(file, "r"), warn = F), file = trg_file)
              self$rdf = rdflib::rdf_parse(trg_file)
            },
            "application/zip" = {
              trg_file = as.character(unzip(zipfile = file, list = T)[1])
              unzip(zipfile = file, exdir = tempdir())
              self$rdf = rdflib::rdf_parse(file.path(tempdir(), trg_file))
            }
          )
        }
      }
    },
    
    #'@description query
    #'@param str str
    #'@param graphUri graphUri
    #'@param mimetype mimetype
    #'@return the response of the SPARQL query
    query = function(str, graphUri = NULL, mimetype = "text/csv"){
      if(!is.null(self$endpoint)){
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
      }else if(!is.null(self$rdf)){
        rdflib::rdf_query(rdf = self$rdf, query = str, data.frame = T)
      }
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
    #'@param count_sub_collections count_sub_collections. Default is TRUE
    #'@param count_concepts count_concepts. Default is TRUE
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
    
    #'@description list_concepts
    #'@param lang lang
    #'@param method method used to build the hierarchy, either "SPARQL" or "R"
    #'@param out_format output format (tibble or list). Default is "tibble"
    #'@return the response of the SPARQL query
    get_concepts_hierarchy = function(lang = "en",
                                      method = c("SPARQL","R"),
                                      out_format = c("tibble","list")){
      
      method = match.arg(method)
      out_format = match.arg(out_format)
      
      out <-switch(method,
        "SPARQL" = {             
          str = paste0("
            PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    
            SELECT ?broaderConcept ?broaderPrefLabel ?concept ?prefLabel WHERE {
                ?concept a skos:Concept .
                OPTIONAL { 
                    ?concept skos:prefLabel ?prefLabel .
                    FILTER (LANG(?prefLabel) = \"",lang,"\")
                }
                OPTIONAL { 
                    ?concept skos:broader ?broaderConcept .
                    OPTIONAL { 
                        ?broaderConcept skos:prefLabel ?broaderPrefLabel .
                        FILTER (LANG(?broaderPrefLabel) = \"",lang,"\")
                    }
                }
            }
            ORDER BY ?concept
          ")
          out = self$query(str = str, mimetype = "text/csv")
          out[is.na(out$broaderConcept),]$broaderConcept = "<root>"
          out[is.na(out$broaderPrefLabel),]$broaderPrefLabel = "<root>"
          out
        },
        "R" = {
          filter_by_language <- function(df, language) {
            df[!is.na(df$lang),] %>%
              dplyr::filter(lang == language)
          }
          #perform base sparql result
          sparql_result = self$query(
            str = "SELECT ?s ?p ?o ?lang WHERE { 
                      ?s ?p ?o .
                      OPTIONAL {
                          BIND(LANG(?o) AS ?lang)
                      }
                  }",
            mimetype = "text/csv"
          )
          # Create a hierarchy data.frame
          out1 = sparql_result %>%
            dplyr::filter(p == "http://www.w3.org/2004/02/skos/core#broader") %>%
            dplyr::rename(concept = s, broaderConcept = o) %>%
            dplyr::select(concept, broaderConcept) %>%
            dplyr::left_join(
              filter_by_language(sparql_result %>% filter(p == "http://www.w3.org/2004/02/skos/core#prefLabel"), lang) %>% rename(concept = s, prefLabel = o),
              by = "concept"
            ) %>%
            dplyr::left_join(
              filter_by_language(sparql_result %>% filter(p == "http://www.w3.org/2004/02/skos/core#prefLabel"), lang) %>% rename(broaderConcept = s, broaderPrefLabel = o),
              by = "broaderConcept"
            ) %>%
            dplyr::select(broaderConcept, broaderPrefLabel, concept, prefLabel)
          #add broader concepts as concepts for root
          out2 = do.call("rbind", lapply(unique(out1$broaderConcept), function(broaderConcept){
            res = NULL
            if(nrow(out1[out1$concept == broaderConcept,])==0){
              res = out1[out1$broaderConcept == broaderConcept,][1,]
              res$concept = res$broaderConcept
              res$prefLabel = res$broaderPrefLabel
              res$broaderConcept = NA
              res$broaderPrefLabel = NA
            }
            res
          }))
          out = rbind(out1,out2)
          
          out[is.na(out$broaderConcept),]$broaderConcept = "<root>"
          out[is.na(out$broaderPrefLabel),]$broaderPrefLabel = "<root>"
          
          #manage exclusion of reciprocal parent-child relationships
          out = do.call("rbind",lapply(unique(out$concept), function(concept){
            ref_children = out[out$concept == concept,]
            ref_parent = out[out$broaderConcept == concept & out$concept %in% ref_children$broaderConcept,]
            if(nrow(ref_parent)>0){
              print(ref_parent)
              pairs = out[(out$concept == concept & out$broaderConcept == ref_parent$concept) | 
                             (out$concept == ref_parent$concept & out$broaderConcept == concept),]
              new_out = rbind(
                pairs[1,],
                ref_children[!ref_children$broaderConcept %in% ref_parent$concept,]
              )
            }else{
              new_out = ref_children
            }
            return(new_out)
          }))
          out
        }
      )
      
      
      if(out_format == "list"){
        relationships <- precompute_relationships(as.data.frame(out), "broaderConcept", "concept", "prefLabel")
        out <- build_hierarchical_list("<root>", relationships)
      }
      return(out)
    },
    
    #'@description list_concepts
    #'@param lang lang
    #'@param mimetype mimetype
    #'@return the response of the SPARQL query
    list_concepts = function(lang = "en", mimetype = "text/csv"){
      str = paste0("
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

        SELECT ?collection ?collectionLabel ?concept ?prefLabel WHERE {
          ?collection a skos:Collection .
          OPTIONAL { ?collection skos:prefLabel ?collectionLabel }
        
          ?collection skos:member ?concept .
          ?concept a skos:Concept .
          ?concept skos:prefLabel ?prefLabel .
          
          FILTER (LANG(?prefLabel) = \"", lang, "\")
        }
        ORDER BY ?collection ?concept
      ")
      out = self$query(str = str, mimetype = mimetype)
      out = out[with(out, order(collection, prefLabel)),]
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
      id = "gemet",
      def = "GEMET Thesaurus",
      uri = "https://www.eionet.europa.eu/gemet",
      file = "https://www.eionet.europa.eu/gemet/latest/gemet.rdf.gz"
    ),
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
