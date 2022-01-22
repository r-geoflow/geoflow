#' geoflow_validator_cell
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
geoflow_validator_cell <- R6Class("geoflow_validator_cell",
  private = list(
    key_required = TRUE,
    valid_keys = list(),
    default_key = NULL,
    exclude_http_keys = TRUE,
    multiple = TRUE,
    str = NA
  ),
  public = list(
    i = NA,
    j = NA,
    initialize = function(key_required, valid_keys, default_key, exclude_http_keys, multiple, i, j, str){
      private$key_required <- key_required
      private$valid_keys <- valid_keys
      private$default_key <- default_key
      private$exclude_http_keys <- exclude_http_keys
      private$multiple <- multiple
      private$str <- str
      self$i <- i
      self$j <- j
    },
    
    #validate
    validate = function(){
      
      report = data.frame(
        type = character(0),
        message = character(0),
        stringsAsFactors = FALSE
      )
      
      #extract components
      strs <- extract_cell_components(sanitize_str(as(private$str,"character")))
      
      if(length(private$valid_keys)>0){
      
        #if valid keys are associated we use them to detect key issues and line separator detection
        #iterate over str (if multiple)
        for(str in strs){
          #check match length with keys
          #if no valid keys are associated, we need to identify line separator issues based on any prefix: except http:/https:
          valid_key_regexp <- ""
          if(length(private$valid_keys)>0) valid_key_regexp <- paste0("(",paste0(private$valid_keys, collapse="|"),")")
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
                report <- rbind(report, data.frame(type = "ERROR", message = sprintf("Key '%s' is invalid, allowed key values are [%s]", kvp$key, paste0(private$valid_keys, collapse = ","))))
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
     initialize = function(i, j, str){
       valid_keys <- list("id", "orcid")
       super$initialize(TRUE, valid_keys, "id", TRUE, TRUE, i, j, str)
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
    initialize = function(i, j, str){
      valid_keys <- list("id", "uuid", "doi", "packageId")
      super$initialize(TRUE, valid_keys, "id", TRUE, TRUE, i, j, str)
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
    initialize = function(i, j, str){
      valid_keys <- list("title", "alternative")
      super$initialize(TRUE, valid_keys, "title", TRUE, TRUE, i, j, str)
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
     initialize = function(i, j, str){
       valid_keys <- list("abstract", "purpose", "credit", "info", "edition", "status")
       super$initialize(TRUE, valid_keys, "abstract", TRUE, TRUE, i, j, str)
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
     initialize = function(i, j, str){
       valid_keys <- list()
       super$initialize(TRUE, valid_keys, NULL, TRUE, TRUE, i, j, str)
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
    initialize = function(i, j, str){
      valid_keys <- list()
      super$initialize(TRUE, valid_keys, NULL, TRUE, TRUE, i, j, str)
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
     initialize = function(i, j, str){
       valid_keys <- list()
       super$initialize(TRUE, valid_keys, "creation", TRUE, TRUE, i, j, str)
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
    initialize = function(i, j, str){
      valid_keys <- list()
      super$initialize(TRUE, valid_keys, "generic", TRUE, TRUE, i, j, str)
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
    initialize = function(i, j, str){
      valid_keys <- list()
      super$initialize(FALSE, valid_keys, NULL, TRUE, TRUE, i, j, str)
    },
    validate = function(){
      report = data.frame(
        type = character(0),
        message = character(0),
        stringsAsFactors = FALSE
      )
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
    initialize = function(i, j, str){
      valid_keys <- list("ewkt", "wkt", "srid")
      super$initialize(TRUE, valid_keys, "ewkt", TRUE, TRUE, i, j, str)
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
     initialize = function(i, j, str){
       valid_keys <- list()
       super$initialize(FALSE, valid_keys, NULL, TRUE, TRUE, i, j, str)
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
    initialize = function(i, j, str){
      valid_keys <- list("resource", "distribution")
      super$initialize(TRUE, valid_keys, "resource", TRUE, TRUE, i, j, str)
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
      initialize = function(i, j, str){
        valid_keys <- list("thumbnail", "http", "parent", "doi", "wms", "wfs", "wcs", "csw")
        super$initialize(TRUE, valid_keys, NULL, FALSE, TRUE, i, j, str)
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
      initialize = function(i, j, str){
        valid_keys <- list("license","use","useLimitation", "useConstraint", "accessConstraint", "otherConstraint")
        super$initialize(TRUE, valid_keys, NULL, FALSE, TRUE, i, j, str)
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
      initialize = function(i, j, str){
        valid_keys <- list("statement", "process", "processor")
        super$initialize(TRUE, valid_keys, NULL, FALSE, TRUE, i, j, str)
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
    initialize = function(i, j, str){
      valid_keys <- list()
      super$initialize(TRUE, valid_keys, NULL, FALSE, TRUE, i, j, str)
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
     source = NULL,
     initialize = function(model, valid_columns, source){
       private$model <- model
       private$valid_columns <- valid_columns
       if(!is(source, "data.frame")){
         errMsg <- "Error in 'geoflow_validator': source parameter should be an object of class 'data.frame'"
         stop(errMsg)
       }
       self$source <- source
     },
     
     #validate_structure
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
     
     #validate_content
     validate_content = function(raw = FALSE){
       content_validation_report <- NULL
       if(!self$validate_structure()) return(NULL)
       source <- self$source[,private$valid_columns]
       cell_reports <- lapply(1:nrow(source), function(i){
         src_obj <- source[i,]
         out_row_report <- lapply(colnames(src_obj), function(colname){
           out_col <- NULL
           col_validator_class <- try(eval(parse(text=paste0("geoflow_validator_", private$model,"_", colname))),silent=T)
           if(is.R6Class(col_validator_class)){
             out_col <- col_validator_class$new(i, which(colnames(src_obj)==colname), src_obj[,colname])
             if(!raw){
               out_col <- out_col$validate()
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
      initialize = function(source){
        valid_columns <- c("Identifier", "Email", "OrganizationName", "PositionName", "LastName", "FirstName",
                              "PostalAddress", "PostalCode", "City", "Country", "Voice", "Facsimile", "WebsiteUrl", "WebsiteName")
        super$initialize("contact", valid_columns, source)
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
    initialize = function(source){
      valid_columns <- c("Identifier", "Title", "Description", "Subject", "Creator", "Date", "Type", "Language", 
                            "SpatialCoverage", "TemporalCoverage", "Format", "Relation", "Rights", "Provenance", "Data")
      super$initialize("entity", valid_columns, source)
    }
  )
)
