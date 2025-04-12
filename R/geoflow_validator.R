#' geoflow_validator_cell
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_cell <- R6Class("geoflow_validator_cell",
  private = list(
    na_authorized = FALSE,
    use_key_syntax =TRUE,
    key_required = TRUE,
    valid_keys = list(),
    default_key = NULL,
    error_if_invalid_key = TRUE,
    exclude_http_keys = TRUE,
    multiple = TRUE,
    str = NA
  ),
  public = list(
    #'@field i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    i = NA,
    #'@field j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    j = NA,
    
    #'@description Initializes a \link{geoflow_validator_cell}
    #'@param na_authorized if validator cell authorizes NAs or empty strings. Default is \code{FALSE}
    #'@param use_key_syntax if validator cell uses key-based syntax. Default is \code{TRUE}
    #'@param key_required if validator cell has a key required. Default is \code{TRUE}
    #'@param valid_keys valid keys for the validator cell. Default is an empty list
    #'@param default_key default_key to use if key is omitted. Default is \code{NULL}
    #'@param error_if_invalid_key raise an error if key is invalid. Default is \code{TRUE}
    #'@param exclude_http_keys if 'http' keys have to be excluded from validation checks. Default is \code{TRUE}
    #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param multiple if cell may contain multiple values. Deprecated
    #'@param str character string to validate
    initialize = function(na_authorized, use_key_syntax, key_required, 
                          valid_keys, default_key,error_if_invalid_key, exclude_http_keys, 
                          multiple, i, j, str){
      
      private$na_authorized <- na_authorized
      private$use_key_syntax <- use_key_syntax
      private$key_required <- key_required
      private$valid_keys <- valid_keys
      private$error_if_invalid_key <-error_if_invalid_key
      private$default_key <- default_key
      private$exclude_http_keys <- exclude_http_keys
      private$multiple <- multiple
      private$str <- str
      self$i <- i
      self$j <- j
    },
    
    #'@description Indicates if the validator cell authorizes NAs and empty strings
    #'@return \code{TRUE} if authorizes NAs and empty strings, \code{FALSE} otherwise
    isNaAuthorized = function(){
      return(private$na_authorized)
    },
    
    #'@description Indicates if the validator cell makes use of key-based syntax
    #'@return \code{TRUE} if using key-based syntax, \code{FALSE} otherwise
    isKeySynthaxUser = function(){
      return(private$key_synthax)
    },
    
    #'@description Indicates if a key is required for the validator cell
    #'@return \code{TRUE} if requires a key, \code{FALSE} otherwise
    isKeyRequired = function(){
      return(private$key_required)
    },
    
    #'@description Gives the list of valid keys for the validator cell
    #'@return the list of valid keys
    getValidKeys = function(){
      return(private$valid_keys)
    },
    
    #'@description Indicates if a report error will be given in case of invalid key
    #'@return \code{TRUE} if a report error will be given in case of invalid key, \code{FALSE} otherwise
    isErrorIfInvalidKey = function(){
      return(private$error_if_invalid_key)
    },
    
    #'@description Gets the default key
    #'@return the default key
    getDefaultKey = function(){
      return(private$default_key)
    },
    
    #'@description Indicates if 'http' keys are excluded from the validation
    #'@return \code{TRUE} if 'http' keys are excluded from the validation, \code{FALSE} otherwise
    isExcludeHttpKeys = function(){
      return(private$exclude_http_keys)
    },
    
    #'@description indicates if multiple key-based components can be used within a same cell
    #'@return \code{TRUE} if supporting multiple key-based components by cell, \code{FALSE} otherwise
    isMultiple = function(){
      return(private$Multiple)
    },
    
    #'@description Proceeds with syntactic validation for the cell considered
    #'@return an object of class \code{data.frame} including possible errors/warnings
    validate = function(){
      
      report = data.frame(
        type = character(0),
        message = character(0),
        stringsAsFactors = FALSE
      )
      
      #Check if empty cell is authorized 
      #If cell is empty and can be empty it's okay, nothing to validate
      if(private$na_authorized){
        if(is.na(private$str)) return(report)
        if(as(private$str, "character") == "") return(report)
      }
      #If cell is empty and should't be empty return a error
      if(!private$na_authorized){
        raise_na_report <- FALSE
        if(is.na(private$str)){
          raise_na_report <- TRUE
        }else{
          if(private$str == "") raise_na_report <- TRUE
        }
        if(raise_na_report){
          report <- data.frame(type = "ERROR", message = "NA or empty string is not authorized")
          return(report)
        }
      }
      
      #If column not use key synthax not proceed to control of the cell content
      if(!private$use_key_syntax) return(report)
      
      #If column use key synthax proceed to control of the cell content
      
      #extract components
      strs <- extract_cell_components(sanitize_str(as(private$str,"character")))
      
      if(length(private$valid_keys)>0){
      
        #if valid keys are associated we use them to detect key issues and line separator detection
        #iterate over str (if multiple)
        for(str in strs){
          #check match length with keys
          #if no valid keys are associated, we need to identify line separator issues based on any prefix: except http:/https:
          valid_key_regexp <- ""
          if(length(private$valid_keys)>0) valid_key_regexp <- paste0("(",paste0(private$valid_keys, collapse="|"),")+[1-9]?[1-9]?")
          http_exclude_regexp <- ""
          if(private$exclude_http_keys) http_exclude_regexp <- "(?![\\w_]*(http|https)[\\w_]*)" 
          str_keys_m <- gregexpr(paste0("\\b", valid_key_regexp, http_exclude_regexp, ":\\b"), text = str, perl = TRUE)[[1]]
          str_keys_m <- str_keys_m[str_keys_m > -1]
          if(length(str_keys_m) == 0){
            kvp <- try(extract_kvp(str), silent = TRUE)
            if(is(kvp, "try-error")){
              if(!is.null(private$default_key)){
                #warning --> indicate default key considered
                report <- rbind(report, data.frame(type = "WARNING", message = sprintf("Key is omitted, default key '%s' will be applied", private$default_key)))
              }else{
                #error --> indicate no default key is available
                if(private$key_required) report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Key is omitted, with no default key, please check the documentation")))
              }
            }else{
              if(!kvp$key %in% private$valid_keys){
                #warning --> indicate key will be ignored
                if(private$error_if_invalid_key){
                  report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Key '%s' is invalid, allowed key values are [%s]", kvp$key, paste0(private$valid_keys, collapse = ","))))
                }else{
                  if(gregexpr("\\d", text = kvp$key)[[1]][1] != -1){
                  report <- rbind(report, data.frame(type = "WARNING", message = sprintf("Key '%s' is not a recognised geoflow key [%s]", kvp$key, paste0(private$valid_keys, collapse = ","))))
                  }
                }
              }
              if("locale" %in% names(kvp)) if(!is.null(kvp$locale)){
                if(nchar(kvp$locale)!= 2){
                  report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Locale value '%s' is invalid, it should be a locale ISO 2 code", kvp$locale)))
                }
                if(kvp$locale != toupper(kvp$locale)){
                  report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Locale value '%s' is invalid, locale ISO 2 code should be uppercase", kvp$locale)))
                }
              }
            }
          }
          
          if(length(str_keys_m)==1){
            #additional checks in case of http keys
            kvp <- try(extract_kvp(str), silent = TRUE)
            if(kvp$key == "http") for(value in kvp$values){
              if(!is.null(attr(value,"uri"))){
                str_http_key_m <- gregexpr("(_http|\nhttp)", attr(value,"uri"))[[1]]
                str_http_key_m <- str_http_key_m[str_http_key_m > -1]
                if(length(str_http_key_m) > 0){
                  for(i in 1:length(str_http_key_m)){
                    httr_key_m_start <- 1
                    if(i > 1) httr_key_m_start <- str_http_key_m[i-1]
                    httr_key_m_end <- str_http_key_m[i]
                    #error --> possibly missing line splitting for http key
                    txt_loc <- substr(attr(value,"uri"),httr_key_m_start,httr_key_m_end)
                    report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Line separator missing at '%s'", txt_loc)))
                  }
                }
              }
            }
          }
    
          if(length(str_keys_m)>1){
            for(i in 1:(length(str_keys_m)-1)){
              #error --> possibly missing line splitting
              txt_loc <- substr(str,str_keys_m[i],str_keys_m[i+1]-1)
              report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Line separator missing at '%s'", txt_loc)))
            }
          }
        }
      }else{
        #iterate over str (if multiple)
        for(str in strs){
          #identify line separator issues without any known key --> supposes \n is handled but not the character (default '_')
          str_keys_m <- gregexpr("\\b(\\\\?):\\b", text = str, perl = TRUE)[[1]] #only identifies backslash not \n
          str_keys_m <- str_keys_m[str_keys_m > -1]
          if(length(str_keys_m) == 0){
            kvp <- try(extract_kvp(str), silent = TRUE)
            if(is(kvp, "try-error")){
              if(!is.null(private$default_key)){
                #warning --> indicate default key considered
                report <- rbind(report, data.frame(type = "WARNING", message = sprintf("Key is omitted, default key '%s' will be applied", private$default_key)))
              }else{
                #error --> indicate no default key is available
                if(private$key_required) report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Key is omitted, with no default key, please check the documentation")))
              }
            }
          }
          if(length(str_keys_m)>1){
            for(i in 1:(length(str_keys_m)-1)){
              #error --> possibly missing line splitting
              txt_loc <- substr(str,str_keys_m[i],str_keys_m[i+1]-1)
              report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Line separator missing at '%s'", txt_loc)))
            }
          }
        }
      }
      return(report)
    }
    
  )
  
)

#' geoflow_validator_contact_Identifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_contact_Identifier <- R6Class("geoflow_validator_contact_Identifier",
   inherit = geoflow_validator_cell,
   public = list(
     
     #'@description Initializes a contact 'Identifier' cell
     #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
     #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
     #'@param str string to validate
     initialize = function(i, j, str){
       valid_keys <- list("id", "orcid")
       super$initialize(FALSE, TRUE, TRUE, valid_keys, "id", TRUE, TRUE, TRUE, i, j, str)
     },
     
     #'@description Validates a contact identifier. Proceeds with syntactic validation and content (ORCID) validation.
     #'@return an validation report, as object of class \code{data.frame}
     validate = function(){
       report <- super$validate()
       cids <- if(!is.na(private$str)) extract_cell_components(private$str) else list()
       for(cid in cids){
         cid_kvp <- extract_kvp(cid)
         #validity of ORCID ID
         if(cid_kvp$key=="orcid"){
           isValidOrcid<-grepl(x = cid_kvp$values[[1]], pattern = '^\\s*(?:(?:https?://)?orcid.org/)?([0-9]{4})\\-?([0-9]{4})\\-?([0-9]{4})\\-?([0-9]{4})\\s*$',perl = TRUE, ignore.case = TRUE)
           if(!isValidOrcid){
             report <- rbind(report, data.frame(type = "WARNING", message = sprintf("ORCID '%s' is not recognized as valid ORCID id format",cid_kvp$values[[1]])))
           }
         }
       }
       return(report)
     }
   )
)

#' geoflow_validator_entity_Identifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Identifier <- R6Class("geoflow_validator_entity_Identifier",
  inherit = geoflow_validator_cell,
  public = list(
    
    #'@description Initializes an entity 'Identifier' cell
    #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param str string to validate
    initialize = function(i, j, str){
      valid_keys <- list(
        "id","id_version", "uuid", "doi", "conceptdoi",
        "zenodo_doi_to_save", "zenodo_conceptdoi_to_save",
        "dataverse_doi_to_save", "dataverse_conceptdoi_to_save",
        "packageId", "dataone_packageId_to_save"
      )
      super$initialize(FALSE, TRUE, TRUE, valid_keys, "id",TRUE, TRUE, TRUE, i, j, str)
    },
    
    #'@description Validates an entity identifier. Proceeds with syntactic validation and 
    #' content validation for DOIs and UUIDs.
    #'@return an validation report, as object of class \code{data.frame}
    validate = function(){
      #Syntax validation
      report = super$validate()
      #Business validation
      ids <- if(!is.na(private$str)) extract_cell_components(private$str) else list()
      if(length(ids)==1){
        hasIdentifierKey <- any(sapply(self$getValidKeys(), function(x){startsWith(ids, x)}))
        if(!hasIdentifierKey) ids <- paste0(self$getDefaultKey(),":", ids)
      }
      for(id in ids){
        id_kvp <- extract_kvp(id)
        #validity of DOI
        if(id_kvp$key=="doi"){
          isValidDoi<-grepl(x = id_kvp$values[[1]], pattern = '^10\\.\\d{4,9}/[-._;()/:A-Z0-9]+$',perl = TRUE, ignore.case = TRUE)
          if(!isValidDoi){
            report <- rbind(report, data.frame(type = "WARNING", message = sprintf("DOI '%s' is not recognized as valid doi format",id_kvp$values[[1]])))
          }
        }
        #validity of UUID
        if(id_kvp$key=="uuid"){
          isValidUuid<-grepl(x = id_kvp$values[[1]], pattern = '^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$',perl = TRUE, ignore.case = TRUE)
          if(!isValidUuid){
            report <- rbind(report, data.frame(type = "WARNING", message = sprintf("UUID '%s' is not recognized as valid uuid format",id_kvp$values[[1]])))
          }
        }
      }
      return(report)
    }
  )
)

#' geoflow_validator_entity_Title
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Title <- R6Class("geoflow_validator_entity_Title",
  inherit = geoflow_validator_cell,
  public = list(
    
    #'@description Initializes an entity 'Title' cell
    #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param str string to validate
    initialize = function(i, j, str){
      valid_keys <- list("title", "alternative")
      super$initialize(FALSE,TRUE, TRUE, valid_keys, "title",TRUE, TRUE, TRUE, i, j, str)
    }
  )
)

#' geoflow_validator_entity_Description
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Description <- R6Class("geoflow_validator_entity_Description",
   inherit = geoflow_validator_cell,
   public = list(
     
     #'@description Initializes an entity 'Description' cell
     #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
     #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
     #'@param str string to validate
     initialize = function(i, j, str){
       valid_keys <- list("abstract", "purpose", "credit", "info", "edition", "status", "maintenance")
       super$initialize(FALSE,TRUE, TRUE, valid_keys, "abstract",TRUE, TRUE, TRUE, i, j, str)
     }
   )
)

#' geoflow_validator_entity_Subject
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Subject <- R6Class("geoflow_validator_entity_Subject",
   inherit = geoflow_validator_cell,
   public = list(
     
     #'@description Initializes an entity 'Subject' cell
     #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
     #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
     #'@param str string to validate
     initialize = function(i, j, str){
       valid_keys <- list()
       super$initialize(TRUE,TRUE, TRUE, valid_keys, NULL,FALSE, TRUE, TRUE, i, j, str)
     },
     
     #'@description Validates a Subject. Proceeds with syntactic validation and content validation.
     #'@return an validation report, as object of class \code{data.frame}  
     validate = function(){
      #Synthax validation
       report <- super$validate()
      #Business validation
      #Keyword topics are ISO keyword topics ?
       
       subjects <- if(!is.na(private$str)) extract_cell_components(private$str) else list()
       if(length(subjects)>0){
          topics<-sapply(subjects, function(subject){
             return(geoflow_subject$new(str = subject)$key)
          })
           
         for(topic in topics){
           if(!topic %in% geometa::ISOKeywordType$values()){
             report <- rbind(report, data.frame(type = "WARNING", message = sprintf("Keyword topic's '%s' is not a recognized ISO keyword topic.", topic)))
           }
         }
       }
       return(report)
     }
   )
)

#' geoflow_validator_entity_Creator
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Creator <- R6Class("geoflow_validator_entity_Creator",
  inherit = geoflow_validator_cell,
  public = list(
    
    #'@description Initializes an entity 'Creator' cell
    #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param str string to validate
    initialize = function(i, j, str){
      valid_keys <- c("metadata", geometa::ISORole$values())
      super$initialize(FALSE,TRUE, TRUE, valid_keys, NULL,FALSE, TRUE, TRUE, i, j, str)
    }
  )
)

#' geoflow_validator_entity_Date
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Date <- R6Class("geoflow_validator_entity_Date",
   inherit = geoflow_validator_cell,
   public = list(
     
     #'@description Initializes an entity 'Date' cell
     #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
     #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
     #'@param str string to validate
     initialize = function(i, j, str){
       valid_keys <- c(geometa::ISODateType$values(),"edition","embargo")
       super$initialize(TRUE,TRUE, TRUE, valid_keys, "creation",FALSE, TRUE, TRUE, i, j, as(str,"character"))
     },
     
     #'@description Validates a date. Proceeds with syntactic validation and content validation.
     #'@return an validation report, as object of class \code{data.frame}    
     validate = function(){
       #Synthax validation
       report = super$validate()
       #Business validation
       dates <- if(!is.na(private$str)) extract_cell_components(private$str) else list()
       if(length(dates)==0){
         #If no date, inform about added of automatic creation date
         report <- rbind(report, data.frame(type = "WARNING", message = "Creation date is missing and will be automaticaly completed at present day and hour"))
       }else{
         if(length(dates)==1){
           hasDateKey <- any(sapply(self$getValidKeys(), function(x){startsWith(dates, x)}))
           if(!hasDateKey) dates <- paste0(self$getDefaultKey(),":", dates)
         }
         for(date in dates){
           date_kvp <- extract_kvp(date)
         
           #Check if date key is a ISO key
           if(!date_kvp$key %in% geometa::ISODateType$values()){
             report <- rbind(report, data.frame(type = "WARNING", message = sprintf("key '%s' is not a recognized ISO date key.", date_kvp$key)))
           }
           #Check if date value is an accepted date format
           for(value in date_kvp$values){
             if(is(value, "character")){
               value <- try(sanitize_date(value),silent=T)
             }
             if(!(is(value, "Date") | inherits(value, "POSIXt"))){
               report <- rbind(report, data.frame(type = "ERROR", message = sprintf("date value '%s' is not a recognized date format", value)))
             }
           }
        }
       }
       return(report)
     }
   )
)

#' geoflow_validator_entity_Type
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Type <- R6Class("geoflow_validator_entity_Type",
  inherit = geoflow_validator_cell,
  public = list(
    
    #'@description Initializes an entity 'Type' cell
    #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param str string to validate
    initialize = function(i, j, str){
      valid_keys <- list("generic", "zenodoResourceType")
      super$initialize(FALSE,T, TRUE, valid_keys, "generic",T, TRUE, TRUE, i, j, str)
    }
  )
)

#' geoflow_validator_entity_Language
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Language <- R6Class("geoflow_validator_entity_Language",
  inherit = geoflow_validator_cell,
  public = list(
    
    #'@description Initializes an entity 'Language' cell
    #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param str string to validate
    initialize = function(i, j, str){
      valid_keys <- geometa::ISOLanguage$values()
      super$initialize(TRUE,FALSE, FALSE, valid_keys, "eng",TRUE, TRUE, TRUE, i, j, str)
    },
    
    #'@description Validates a language. Proceeds with syntactic validation and language validation.
    #'@return an validation report, as object of class \code{data.frame}    
    validate = function(){
      report = data.frame(
        type = character(0),
        message = character(0),
        stringsAsFactors = FALSE
      )
      #Business validation
      #language are ISO language ?
      if(!private$str %in% geometa::ISOLanguage$values()){
        report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Language '%s' is not a recognized language ISO3 code.", private$str)))
      }
      return(report)
    }
  )
)

#' geoflow_validator_entity_SpatialCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_SpatialCoverage <- R6Class("geoflow_validator_entity_SpatialCoverage",
  inherit = geoflow_validator_cell,
  public = list(
    
    #'@description Initializes an entity 'SpatialCoverage' cell
    #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param str string to validate
    initialize = function(i, j, str){
      valid_keys <- list("ewkt", "wkt", "srid")
      super$initialize(TRUE,TRUE, TRUE, valid_keys, "ewkt",TRUE, TRUE, TRUE, i, j, str)
    },
    
    #'@description Validates a spatial coverage. Proceeds with syntactic validation and 
    #' spatial coverage validation (including EWKT, WKT and SRID).
    #'@return an validation report, as object of class \code{data.frame}  
    validate = function(){
      #Synthax validation
      report <- super$validate()
      #Business validation
      #Control validity of wkt and srid format
      spatial_props <- if(!is.na(private$str)) extract_cell_components(private$str) else list()
      if(length(spatial_props)>0){
        if(length(spatial_props)==1){
          hasEWKTKey <- any(sapply(self$getValidKeys(), function(x){startsWith(spatial_props, x)}))
          if(!hasEWKTKey) spatial_props <- paste0(self$getDefaultKey(),":", spatial_props)
        }
        kvps <- lapply(spatial_props, extract_kvp)
        kvps <- lapply(kvps, function(x){out <- x; out$values <- list(paste0(out$values, collapse=",")); return(out)})
        names(kvps) <- sapply(kvps, function(x){x$key})
        for(kvpname in names(kvps)){
          switch(kvpname,
                 "ewkt" = {
                   spatial_cov <- kvps$ewkt$values[[1]]
                   if(!is.na(spatial_cov)){
                     if(!startsWith(spatial_cov,"SRID=")){
                       report <- rbind(report, data.frame(type = "ERROR", message = "SRID is missing! The spatial coverage should be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry"))
                     }
                     spatial_cov <- unlist(strsplit(spatial_cov, ";"))
                     if(length(spatial_cov)!=2){ 
                       report <- rbind(report, data.frame(type = "ERROR", message = "The spatial coverage should be a valid EWKT string, starting with the SRID definition (e.g. SRID=4326), followed by a semicolon and the WKT geometry"))
                     }
                     spatial_srid <- as.integer(unlist(strsplit(spatial_cov[1],"SRID="))[2])
                     spatial_cov <- spatial_cov[2]
                     spatial_extent<-try(sf::st_as_sfc(x = spatial_cov, crs = spatial_srid))
                     if(class(spatial_extent)[1]=="try-error"){
                       report <- rbind(report, data.frame(type = "ERROR", message = "The spatial extent is invalid!"))
                     }
                   }
                 },
                 "wkt" = {
                   spatial_cov <- kvps$wkt$values[[1]]
                   if("srid" %in% names(kvps)){
                     spatial_srid <- as.integer(kvps$srid$values[[1]])
                     if(is.na(spatial_srid)){
                       report <- rbind(report, data.frame(type = "ERROR", message ="The spatial SRID should be an integer."))
                     }
                   }else{
                     report <- rbind(report, data.frame(type = "WARNING", message ="A WKT geometry is specified but without SRID!"))
                     spatial_extent<-try(sf::st_as_sfc(wkt=spatial_cov, crs = NA))
                     if(class(spatial_extent)[1]=="try-error"){
                       report <- rbind(report, data.frame(type = "ERROR", message = "The spatial extent is invalid!"))
                     }
                   }
                 },
                 "srid" = {
                   spatial_srid <- as.integer(kvps$srid$values[[1]])
                   if(is.na(spatial_srid))
                     report <- rbind(report, data.frame(type = "ERROR", message ="The spatial SRID should be an integer."))
                 }
          )
        }
      }
      return(report)
    }
  )
)

#' geoflow_validator_entity_TemporalCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_TemporalCoverage <- R6Class("geoflow_validator_entity_TemporalCoverage",
   inherit = geoflow_validator_cell,
   public = list(
     
     #'@description Initializes an entity 'TemporalCoverage' cell
     #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
     #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
     #'@param str string to validate
     initialize = function(i, j, str){
       valid_keys <- list()
       super$initialize(TRUE,FALSE, FALSE, valid_keys, NULL,FALSE, TRUE, TRUE, i, j, str)
     },
     
     #'@description Validates temporal coverage. Proceeds with syntactic validation and temporal coverage validation.
     #'@return an validation report, as object of class \code{data.frame}
     validate = function(){
       #validate syntax (essentially for NA case)
       report <- super$validate()
       #Business validation
       tmp_cov <- private$str
       if(length(tmp_cov)!=0){
        value <- tmp_cov
        if(is(tmp_cov, "character")) value <- unlist(strsplit(tmp_cov,"/"))
          if(length(value)==1){
            #check instant date
            if(is(value, "character")){
              value <- try(sanitize_date(value),silent=T)
              if(!(is(value, "Date") | inherits(value, "POSIXt"))){
                report <- rbind(report, data.frame(type = "ERROR", message = sprintf("instant date value '%s' is not a recognized date format", tmp_cov)))
              }
            }
          }else if(length(value)==2){
            #check start date
            start<-value[1]
            if(is(start, "character") && is.na(as(start,"numeric"))){
               start<- try(sanitize_date(start),silent=T)
               if(!(is(start, "Date") | inherits(start, "POSIXt"))){
                 report <- rbind(report, data.frame(type = "ERROR", message = sprintf("start date value '%s' is not a recognized date format", value[1])))
               }
            }
            #check end date
            end<-value[2]
            if(is(end, "character")  && is.na(as(end,"numeric"))){
              end <- try(sanitize_date(end),silent=T)
              if(!(is(end, "Date") | inherits(end, "POSIXt"))){
                report <- rbind(report, data.frame(type = "ERROR", message = sprintf("end date value '%s' is not a recognized date format", value[2])))
              }
            }
            
          }
        }
       return(report)
     }

   )
)

#' geoflow_validator_entity_Format
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Format <- R6Class("geoflow_validator_entity_Format",
  inherit = geoflow_validator_cell,
  public = list(
    
    #'@description Initializes an entity 'Format' cell
    #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param str string to validate
    initialize = function(i, j, str){
      valid_keys <- list("resource", "distribution")
      super$initialize(TRUE,TRUE, TRUE, valid_keys, "resource",TRUE, TRUE, TRUE, i, j, str)
    }
  )
)

#' geoflow_validator_entity_Relation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Relation <- R6Class("geoflow_validator_entity_Relation",
    inherit = geoflow_validator_cell,
    public = list(
      
      #'@description Initializes an entity 'Relation' cell
      #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
      #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
      #'@param str string to validate
      initialize = function(i, j, str){
        valid_keys <- list(
          "ftp", "http", "download",
          "parent","thumbnail",
          "ref", "grant",
          "csw", "csw202", "csw30",
          "wcs", "wcs100", "wcs11", "wcs110", "wcs111", "wcs201",
          "wfs", "wfs100", "wfs110", "wfs200",
          "wms", "wms110", "wms111", "wms130"
        )
        super$initialize(TRUE,TRUE, TRUE, valid_keys, NULL,TRUE, FALSE, TRUE, i, j, str)
      }
    )
)

#' geoflow_validator_entity_Rights
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Rights <- R6Class("geoflow_validator_entity_Rights",
    inherit = geoflow_validator_cell,
    public = list(
      
      #'@description Initializes an entity 'Rights' cell
      #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
      #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
      #'@param str string to validate
      initialize = function(i, j, str){
        valid_keys <- list("license","use","useLimitation", "useConstraint", "accessConstraint", "otherConstraint", "accessRight", "accessConditions")
        super$initialize(TRUE,TRUE, TRUE, valid_keys, NULL,TRUE, FALSE, TRUE, i, j, str)
      }
    )
)

#' geoflow_validator_entity_Provenance
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Provenance <- R6Class("geoflow_validator_entity_Provenance",
    inherit = geoflow_validator_cell,
    public = list(
      
      #'@description Initializes an entity 'Provenance' cell
      #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
      #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
      #'@param str string to validate
      initialize = function(i, j, str){
        valid_keys <- list("statement", "process")
        super$initialize(TRUE,TRUE, TRUE, valid_keys, NULL,TRUE, FALSE, TRUE, i, j, str)
      },
      
      #'@description Validates a Provenance. Proceeds with syntactic validation and content validation.
      #'@return an validation report, as object of class \code{data.frame}  
      validate = function(){
        report <- super$validate()
        if(is.na(private$str)) return(report)
        #Validity of corresponding number of processors vs. processes   
        data_props <- extract_cell_components(sanitize_str(private$str))
        if(length(data_props>0)){
          state_prop <- data_props[[1]]
          if(!startsWith(state_prop, "statement")){
            report <- rbind(report, data.frame(type = "ERROR", message = "The data 'statement' is mandatory"))
          }
          state_prop <- unlist(strsplit(state_prop,"statement:"))[2]
          if(length(data_props)>1){
            data_props <- data_props[2:length(data_props)]
            #processes
            processes <- data_props[sapply(data_props, function(x){startsWith(x, "process:")})]
            processes <- lapply(processes, function(process){
              return(extract_kvp(process))
            })
            #processors
            #processors <- data_props[sapply(data_props, function(x){startsWith(x,"processor:")})]
            #processors_splits <- unlist(strsplit(processors, ":"))
            #processors <- unlist(strsplit(processors_splits[2],","))
            #control processors vs. processes
            #if(length(processors)!=length(processes)){
            #  report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Number of processors [%s] doesn't match the number of processes [%s]",length(processors), length(processes))))
            #}
          }
        }
        return(report)
      }
    )
)

#' geoflow_validator_entity_Data
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entity_Data <- R6Class("geoflow_validator_entity_Data",
  inherit = geoflow_validator_cell,
  public = list(
    
    #'@description Initializes an entity 'Data' cell
    #'@param i row index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param j col index (internal index to be used for graphical \pkg{geoflow} validation handlers)
    #'@param str string to validate
    initialize = function(i, j, str){
      valid_keys <- list()
      super$initialize(TRUE,TRUE, TRUE, valid_keys, NULL,FALSE, FALSE, TRUE, i, j, str)
    }
  )
)

#' geoflow_validator
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator <- R6Class("geoflow_validator",
   private = list(
     model = NULL,
     valid_columns = c()
   ),
   public = list(
     #'@field source object of class \code{data.frame} handling metadata objects to validate
     source = NULL,
     
     #'@description Initializes a table validator for a given metadata model
     #'@param model the data model name, eg. "entity", "contact"
     #'@param valid_columns a vector of valid columns for the data model
     #'@param source an object of class \code{data.frame} handling the contacts
     initialize = function(model, valid_columns, source){
       private$model <- model
       private$valid_columns <- valid_columns
       if(!is(source, "data.frame")){
         errMsg <- "Error in 'geoflow_validator': source parameter should be an object of class 'data.frame'"
         stop(errMsg)
       }
       self$source <- source
     },
     
     #'@description Validates a source table against a data model structure
     #'@return \code{TRUE} if valid, \code{FALSE} otherwise.
     validate_structure = function(){
       status <- TRUE
       #check that all columns are available
       if(!all(private$valid_columns %in% colnames(self$source))){
         missing_columns <- private$valid_columns[which(!private$valid_columns %in% colnames(self$source))]
         errMsg <- sprintf("Error in 'geoflow_validator': missing mandatory columns [%s]", paste0(missing_columns, collapse = ","))
         status <- FALSE
         attr(status, "message") <- errMsg
         return(status)
       }
       #in case additional columns are available but not needed we raise a warning
       if(any(!colnames(self$source) %in% private$valid_columns)){
         misc_columns <- colnames(self$source)[!colnames(self$source) %in% private$valid_columns] 
         warnMsg <- sprintf("Warning in 'geoflow_validator': miscellaneaous columns ignored by geoflow [%s]", paste0(misc_columns, collapse = ","))
         attr(status,"message") <- warnMsg
       }
       return(status)
     },
     
     #'@description Validates a source table using syntactic and content validation rules
     #'@param raw indicates whether to return a \code{list} of \code{geoflow_validator_cell} objects or a \code{data.frame}
     #'@param debug debug validation
     #'@return a \code{list} of \code{geoflow_validator_cell} objects, or \code{data.frame} 
     validate_content = function(raw = FALSE){
       content_validation_report <- NULL
       if(!self$validate_structure()) return(NULL)
       source <- self$source[,colnames(self$source)[colnames(self$source) %in% private$valid_columns]]
       cell_reports <- lapply(1:nrow(source), function(i){
         src_obj <- source[i,]
         out_row_report <- lapply(colnames(src_obj), function(colname){
           out_col <- NULL
           col_validator_class <- try(eval(parse(text=paste0("geoflow_validator_", private$model,"_", colname))),silent=T)
           if(is.R6Class(col_validator_class)){
             out_col <- col_validator_class$new(i, which(colnames(src_obj)==colname), src_obj[,colname])
             if(!raw){
          
               out_col <- try(out_col$validate(), silent = T)
               if(is(out_col,"try-error")){
                 stop(sprintf("Unexpected validation error for row %s column %s ('%s'), value '%s':\n%s", 
                              i, which(colnames(src_obj)==colname), colname, src_obj[,colname], out_col[1]))
               }
               out_col <- cbind(col = rep(colname,nrow(out_col)), out_col)
             }
           }
           out_col
         })
         if(!raw){
           out_row_report <- do.call("rbind", out_row_report)
           out_row_report <- cbind(row = rep(i,nrow(out_row_report)), out_row_report)
         }
         return(out_row_report)
       })
       if(raw){
         content_validation_report <- do.call("c", cell_reports)
       }else{
         content_validation_report <- do.call("rbind", cell_reports)
       }
       return(content_validation_report)
     }
   )
                             
)

#' geoflow_validator_contacts
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_contacts <- R6Class("geoflow_validator_contacts",
    inherit = geoflow_validator,
    public = list(
      
      #'@description Initializes a contacts table validator
      #'@param source an object of class \code{data.frame} handling the contacts
      initialize = function(source){
        valid_columns <- c("Identifier", "Email", "OrganizationName", "PositionName", "LastName", "FirstName",
                              "PostalAddress", "PostalCode", "City", "Country", "Voice", "Facsimile", "WebsiteUrl", "WebsiteName")
        super$initialize(model = "contact", valid_columns = valid_columns, source = source)
      }
    )
)

#' geoflow_validator_entities
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_entities <- R6Class("geoflow_validator_entities",
  inherit = geoflow_validator,
  public = list(
    
    #'@description Initializes an entities table validator
    #'@param source an object of class \code{data.frame} handling the entities
    initialize = function(source){
      valid_columns <- c("Identifier", "Title", "Description", "Subject", "Creator", "Date", "Type", "Language", 
                            "SpatialCoverage", "TemporalCoverage", "Format", "Relation", "Rights", "Provenance", "Data")
      super$initialize(model = "entity", valid_columns = valid_columns, source = source)
    }
  )
)
