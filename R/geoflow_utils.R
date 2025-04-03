#' @name sanitize_str
#' @aliases sanitize_str
#' @title sanitize_str
#' @description \code{sanitize_str} sanitizes a string definition in geoflow
#'
#' @usage sanitize_str(str)
#'                 
#' @param str a string as object of class \code{character}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
sanitize_str <- function(str){
  if(!is(str, "character")) return(str)
  if(is.na(str)) return(NA)
  if(!is.na(str) & str=="") return(NA)
  startwith_n <- startsWith(str, "\n")
  while(startwith_n){
    str <- substr(str, 2, nchar(str))
    startwith_n <- startsWith(str, "\n")
  }
  str <- gsub(";;", ";", str)
  str <- gsub(",;", ",", str)
  str <- gsub(":;", ":", str)
  return(str)
}

#' @name sanitize_date
#' @aliases sanitize_date
#' @title sanitize_date
#' @description \code{sanitize_date} sanitizes a date in geoflow
#'
#' @usage sanitize_date(date)
#'                 
#' @param date an object o class \code{character}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
sanitize_date <- function(date){
  if(is(date, "character")){
    if(date==""){
      date <- NA 
    }else{
      if(nchar(date)==10){
        date <- as.Date(date)
      }else if(nchar(date)==7){
        date <- as.Date(paste0(date,"-01"))
      }else if(nchar(date)==4){
        date <- as.Date(paste0(date,"-01-01"))
      }else{
        date <- as.POSIXct(date) 
      }
    }
  }
  return(date)
}

#' @name extract_kvp
#' @aliases extract_kvp
#' @title extract_kvp
#' @description \code{extract_kvp} parses a string into a key value pair represented by
#' a \code{geoflow_kvp} object.
#'
#' @usage extract_kvp(str)
#'                 
#' @param str a string as object of class \code{character}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
extract_kvp <- function(str){
  if(endsWith(str,":")) return(NA)
  #kvp <- unlist(strsplit(str, ":(?!//|\\d)", perl = T))
  kvp <- unlist(strsplit(str, ':(?!//)\\s*(?=([^"]*"[^"]*")*[^"]*$)', perl = TRUE))
  if(length(kvp)==1) stop("Error while splitting kvp key/value")
  if(length(kvp)>2) kvp[2] <- paste(kvp[2:length(kvp)], collapse=":", sep="")
  
  #key
  key <- kvp[1]
  key_splits <- unlist(strsplit(key, "@"))
  if(length(key_splits)>1){
    key <- key_splits[1]
    attr(key,"uri") <- key_splits[2]
  }
  hasDescription <- regexpr("\\[", key)>0 & endsWith(key, "]")
  if(hasDescription){
    attrs <- attributes(key)
    value_splits <- unlist(strsplit(key, "\\["))
    key <- value_splits[1]
    if(startsWith(key, "\"") && endsWith(key, "\"")) key <- substr(key, 2, nchar(key)-1)
    attributes(key) <- attrs
    des <- value_splits[2]
    des <- substr(des, 1, nchar(des)-1)
    if(startsWith(des, "\"") && endsWith(des, "\"")) des <- substr(des, 2, nchar(des)-1)
    attr(key, "description") <- des
  }else{
    if(startsWith(key, "\"") && endsWith(key, "\"")) key <- substr(key, 2, nchar(key)-1)
  }
  
  #values
  values <- unlist(strsplit(kvp[2], ',\\s*(?=([^"]*"[^"]*")*[^"]*$)', perl = TRUE))
  values <- lapply(values, function(value){
    value_splits <- unlist(strsplit(value, "@"))
    if(length(value_splits)>1){
      value <- value_splits[1]
      link <- value_splits[2]
      attr(value, "uri") <- link
    }
    hasDescription <- regexpr("\\[", value)>0 & endsWith(value, "]")
    if(hasDescription){
      attrs <- attributes(value)
      value_splits <- unlist(strsplit(value, "\\["))
      value <- value_splits[1]
      if(startsWith(value, "\"") && endsWith(value, "\"")) value <- substr(value, 2, nchar(value)-1)
      attributes(value) <- attrs
      des <- value_splits[2]
      des <- substr(des, 1, nchar(des)-1)
      if(startsWith(des, "\"") && endsWith(des, "\"")) des <- substr(des, 2, nchar(des)-1)
      attr(value, "description") <- des
    }else{
      if(startsWith(value, "\"") && endsWith(value, "\"")) value <- substr(value, 2, nchar(value)-1)
    }
    
    return(value)
  })
  
  #locale management
  locale = NULL
  key_attrs <- attributes(key)
  key_parts <- unlist(strsplit(key, "#"))
  if(length(key_parts)>1){
    key <- key_parts[1]
    attributes(key) <- key_attrs
    locale <- key_parts[2]
  }
  return(geoflow_kvp$new(key = key, values = values, locale = locale))
}

#' @name extract_kvps
#' @aliases extract_kvps
#' @title extract_kvps
#' @description \code{extract_kvp} parses a string into a key value pair represented by
#' a \code{geoflow_kvp} object.
#'
#' @usage extract_kvps(strs, collapse)
#'                 
#' @param strs a string as object of class \code{character}
#' @param collapse collapse by. Default is \code{NULL}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
extract_kvps <- function(strs, collapse = NULL){
  kvps <- lapply(strs, function(str){
    kvp <- extract_kvp(str)
    if(!is.null(collapse)) kvp$values <- list(paste0(kvp$values, collapse = collapse))
    if(length(kvp$values)==1) kvp$values <- kvp$values[[1]]
    return(kvp)
  })
  keys <- unique(sapply(kvps, "[[", "key"))
  kvps <- do.call("c", lapply(keys, function(key){
    kvps_for_key <- kvps[sapply(kvps, function(kvp){kvp$key == key})]
    with_locales <- any(sapply(kvps_for_key, function(x){!is.null(x$locale)}))
    if(!with_locales){
      return(kvps_for_key)
    }
    kvp_with_default_locale <- kvps_for_key[sapply(kvps_for_key, function(x){is.null(x$locale)})]
    kvp_with_locale <- kvps_for_key[sapply(kvps_for_key, function(x){!is.null(x$locale)})]
    if(length(kvp_with_default_locale)>0){
      kvp_with_default_locale <- kvp_with_default_locale[[1]]
    }else{
      #TODO support default language in geoflow
    }
    #localization
    #locale key descriptions
    for(kvp in kvp_with_locale){
      if(!is.null(attr(kvp$key, "uri"))) attr(key, paste0("uri#", kvp$locale)) <- attr(kvp$key, "uri")
      if(!is.null(attr(kvp$key, "description"))) attr(key, paste0("description#", kvp$locale)) <- attr(kvp$key, "description")
    }
    #locale key uris
    #locale values
    locale_values <- kvp_with_default_locale$values
    #if(length(locale_values)==1) locale_values <- locale_values[[1]]
    for(item in kvp_with_locale){
      attr(locale_values, paste0("locale#", toupper(item$locale))) <- item$values
    }
    
    kvp_with_locales <- list(geoflow_kvp$new(key = key, values = locale_values))
    return(kvp_with_locales)
  }))
  
  return(kvps)
}

#'@name get_locales_from
#'@aliases get_locales_from
#'@title get_locales_from
#'@description Get locales from a property values set
#'
#'@usage get_locales_from(values)
#'
#'@param values values
#'
#'@export
get_locales_from <- function(values){
  if(is.null(attributes(values))) return(NULL)
  locales <- lapply(attributes(values), function(x){
    if(is.list(x)) x <- x[[1]]
    return(x)
  })
  names(locales) <- sapply(names(attributes(values)), function(x){unlist(strsplit(x, "locale#"))[2]})
  return(locales)
}

#'@name set_locales_to
#'@aliases set_locales_to
#'@title set_locales_to
#'@description Set locales to a property values set
#'
#'@usage set_locales_to(values,locales)
#'
#'@param values values
#'@param locales locales
#'
#'@export
set_locales_to <- function(values, locales = list()){
  for(lang in names(locales)){
    attr(values, paste0("locale#", lang)) <- locales[[lang]]
  }
  return(values)
}

#'@name set_i18n
#'@aliases set_i18n
#'@title set_i18n
#'@description Set default locales to a property values set
#'
#'@usage set_i18n(term_key, default, expr, ...)
#'
#'@param term_key term key
#'@param default default
#'@param expr expr
#'@param ... named values to be passed to expr
#'
#'@export
set_i18n <- function(term_key, default = NULL, expr = "{{term}}", ...){
  
  i18n_terms = jsonlite::read_json(system.file("metadata/i18n.json", package = "geoflow"))
  if(!term_key %in% names(i18n_terms)) stop(sprintf("Term '%s' not defined in i18n.json file!"))
  locales = i18n_terms[[term_key]]
  
  if(regexpr("\\{\\{term\\}\\}", expr) == -1) stop(sprintf("Expression 'expr' should at least include the key '{{term}}'"))
  
  set_locales_to(
    values = whisker::whisker.render(expr, c(term = if(!is.null(default)) default else locales[[1]], list(...))), 
    locales = lapply(locales, function(x){
      whisker::whisker.render(expr, c(term = x, list(...)))
    })
  )
}

#' @name str_to_posix
#' @aliases str_to_posix
#' @title str_to_posix
#' @description \code{str_to_posix} parses a string into a \code{POSIX} object
#'
#' @usage str_to_posix(str)
#'                 
#' @param str a string as object of class \code{character}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
str_to_posix <- function(str){
  out <- str
  if(is(str,"character")) if(nchar(str)>7){
    str_format <- if(nchar(str)==10) "%Y-%m-%d" else "%Y-%m-%dT%H:%M:%S"
    out <- as.POSIXct(str, format = str_format, tz = ifelse(endsWith(str,"Z"), "UTC", ""))
  }
  return(out)
}

#' @name posix_to_str
#' @aliases posix_to_str
#' @title posix_to_str
#' @description \code{posix_to_str} converts a \code{POSIX} object to ISO string
#'
#' @usage posix_to_str(posix)
#'                 
#' @param posix a \code{POSIX} object
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
posix_to_str <- function(posix){
  out <- posix
  if(!class(posix)[1] %in% c("Date","POSIXct")) return(out)
  str_format <- "%Y-%m-%dT%H:%M:%S"
  if(is(posix,"Date")) str_format = "%Y-%m-%d"
  out <- format(posix, format = str_format)
  tzone <- attr(posix,"tzone")
  if(!is.null(tzone)) if(tzone %in% c("UTC","GMT")) out <- paste0(out, "Z")
  return(out)
}

#' @name filter_sf_by_cqlfilter
#' @aliases filter_sf_by_cqlfilter
#' @title filter_sf_by_cqlfilter
#' @description \code{filter_sf_by_cqlfilter} filters an object of class \code{sf} using
#' a CQL syntax. This function is minimalistic and only basic CQL filters are supported.
#' 
#' @usage filter_sf_by_cqlfilter(sfdata, cqlfilter)
#'                 
#' @param sfdata object of class \code{sf}
#' @param cqlfilter object of class \code{character} representing a CQL filter
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
filter_sf_by_cqlfilter <- function(sfdata, cqlfilter){
  out <- NULL
  rfilter <- gsub(" AND ", " & ", cqlfilter)
  rfilter <- gsub(" OR ", " | ", rfilter)
  rfilter <- gsub(" IN\\(", " %in% c(", rfilter)
  rfilter <- gsub("'","\"", rfilter)
  rfilter <- gsub("=", "==", rfilter)
  rfilter <- paste0("sfdata$", rfilter)
  sfdata.filtered <- try(eval(parse(text= sprintf("sfdata[%s,]",rfilter))))
  if(class(sfdata.filtered)[1]!="try-error") out <- sfdata.filtered
  return(out)
}

#' @name extract_cell_components
#' @aliases extract_cell_components
#' @title extract_cell_components
#' @description \code{extract_cell_components} extracts the components of a cell
#' when using tabular data content handlers (for entity and contact).
#'
#' @usage extract_cell_components(str)
#'                 
#' @param str a string as object of class \code{character}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
extract_cell_components <- function(str){
  lines <- unlist(strsplit(str, get_line_separator()))
  return(lines)
}

#' @name set_line_separator
#' @aliases set_line_separator
#' @title set_line_separator
#' @description \code{set_line_separator} set the line separator to be used by geoflow
#' when extracting cell components for tabular data content handling.
#'
#' @usage set_line_separator(x)
#'                 
#' @param x a string as object of class \code{character} representing the line separator.
#  Default is set to an underscore followed by a line break.
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
set_line_separator <- function(x = "_\n"){
  if(!is(x,"character")) stop("The line separator should be an object of class 'character'")
  .geoflow$LINE_SEPARATOR <- x
}

#' @name get_line_separator
#' @aliases get_line_separator
#' @title get_line_separator
#' @description \code{get_line_separator} get the line separator used by geoflow
#' when extracting cell components for tabular data content handling. Default is 
#' set to an underscore followed by a line break.
#'
#' @usage get_line_separator()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
get_line_separator <- function(){
  return(.geoflow$LINE_SEPARATOR)
}

#' @name enrich_text_from_entity
#' @aliases enrich_text_from_entity
#' @title enrich_text_from_entity
#' @description \code{enrich_text_from_entity} will attempt to enrich an entity text property
#' from other entity metadata, depending on text variables handled by a pattern in the form
#' %property%. 
#' 
#' - If the entity property is a text, only the name of the property name is required.
#' 
#' - If the entity property is a list, then 2 subcases can be distinguished:
#' 
#' If it is a named list (such as entity descriptions), the text variable will be compound by
#' the entity property name and the element_property name, in the form %property:element_property%
#' 
#' If it is a unnamed list (such as list of keywords, list of relations, etc), the text variable will handle
#' four elements: property (entity property name to look at), a key value pair to use for search
#' within the list, an element_property for which the value should be picked up to enrich the text.
#' The variable willbe in the form %property:keye:value:element_property%
#'
#' @usage enrich_text_from_entity(str, entity)
#' 
#' @param str a text to be enriched
#' @param entity an object of class \code{geoflow_entity}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
enrich_text_from_entity = function(str, entity){
  if(!is.character(str)) return(str)
  outstr <- str
  indexes <- gregexpr(pattern = "\\%(.*?)\\%", str)[[1]]
  if(length(indexes)==1) if(indexes == -1) return(str)
  for(index in indexes){
    newvalue <- NULL
    metavar <- unlist(strsplit(substr(str, index+1, nchar(str)),"%"))[1]
    metavars <- unlist(strsplit(metavar, ":"))
    meta_prop <- metavars[1]
    target_props <- entity[[meta_prop]]
    
    if(!is.null(target_props)){
      #character
      if(is.character(target_props)){
        newvalue <- target_props
        #lists
      }else if(is.list(target_props)){
        #named lists
        if(length(names(target_props))){
          meta_keyname <- metavars[2]
          newvalue <- target_props[[meta_keyname]]
        }else{
          if(length(metavars)!=4) next
          meta_keyname <- metavars[2]
          meta_keyvalue <- metavars[3]
          meta_value <- metavars[4]
          search <- target_props[sapply(target_props, function(x){x[[meta_keyname]]==meta_keyvalue})]
          if(length(search)==0) next
          newvalue <- search[[1]][[meta_value]]
        }
      }
    }
    if(is.null(newvalue)) next
    outstr <- sub(pattern = "\\%(.*?)\\%", newvalue, outstr)
  }
  return(outstr)
}

#' @name check_packages
#' @aliases check_packages
#' @title check_packages
#' @description \code{check_packages} checks availability of a list of R packages in R. This
#' function is essentially used internally by \pkg{geoflow} in assocation to \code{geoflow_software} and
#' \code{geoflow_action} that would need specific packages to be imported in R.
#' 
#' @usage check_packages(pkgs)
#' 
#' @param pkgs a vector of package names
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
check_packages <- function(pkgs){
  
  if(length(pkgs)==0) return(NULL)
  pkgs_df <- do.call("rbind", lapply(pkgs, function(pkg){
    pkg_loaded <- suppressWarnings(require(pkg, character.only = TRUE))
    pkg_df <- data.frame(
      package = pkg,
      installed = pkg_loaded,
      version = ifelse(pkg_loaded, as(packageVersion(pkg), "character"), NA),
      stringsAsFactors = FALSE
    )
    return(pkg_df)
  }))
  if(any(!pkgs_df$installed)){
    pkgs_not_installed <- pkgs_df[!pkgs_df$installed,]
    print(pkgs_not_installed)
    stop(sprintf("The following package(s) are required but not installed: %s",
                paste0(pkgs_not_installed$package, collapse=", ")))
  }
  return(pkgs_df)
}

#'@name add_config_utils
#'@aliases add_config_utils
#'@title add_config_utils
#'@description \code{add_config_utils} adds util functions needed (logger, log_separator) to the configuratino object
#'
#'@usage add_config_utils(config)
#'
#'@param config object of class \link{list}
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
add_config_utils <- function(config){
  id <- if(!is.null(config$profile$id)) config$profile$id else config$id
  config$logger <- function(type, text){
    txt <- text #use this to make sure sprintf calls don't conflict with next sprintf call
    cat(sprintf("[geoflow][%s][%s] %s \n", id, type, txt))
  }
  config$logger.info <- function(text){config$logger("INFO", text)}
  config$logger.warn <- function(text){config$logger("WARN", text)}
  config$logger.error <- function(text){config$logger("ERROR", text)}
  config$log_separator <- function(char){cat(paste0(paste0(rep(char,100),collapse=""),"\n"))}
  return(config)
}

#' @name load_workflow_environment
#' @aliases load_workflow_environment
#' @title load_workflow_environment
#' @description \code{load_workflow_environment} loads a workflow environment by evaluating variable expressions in the 
#' form \code{${{variable}}}. If no variable expression pattern is identified in the string, 
#' the function will return the original string.
#' 
#' @usage load_workflow_environment(config, session)
#' 
#' @param config object of class \link{list}
#' @param session a \pkg{shiny} session object (optional) to run geoflow in a \pkg{shiny} context.
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
load_workflow_environment <- function(config, session = NULL){
  config_str <- jsonlite::toJSON(config, auto_unbox = TRUE)
  
  #grab shiny session userData if any session specified
  userdata <- list()
  if(!is.null(session)){
    if(!is(session, "ShinySession")){
      stop("The 'session' argument should specify an object of class 'ShinySession'")
    }
    userdata <- as.list(session$userData)
    userdata <- userdata[!sapply(userdata, is.function)]
  }
  
  #evaluate environment variables + eventual shiny session userData
  config_str <- whisker::whisker.render(config_str, c(as.list(Sys.getenv()), userdata))

  config <- jsonlite::parse_json(config_str)
  config <- add_config_utils(config)
  return(config)
}

#from dotenv internal functions - see https://github.com/gaborcsardi/dotenv/issues/11
#dotenv_ignore_comments
dotenv_ignore_comments <- function (lines) {
  grep("^#", lines, invert = TRUE, value = TRUE)
}
#dotenv_ignore_empty_lines
dotenv_ignore_empty_lines <- function (lines) {
  grep("^\\s*$", lines, invert = TRUE, value = TRUE)
}
#dotenv_extract_match
dotenv_extract_match <- function (line, match) {
  tmp <- mapply(attr(match, "capture.start"), attr(match, "capture.length"), 
                FUN = function(start, length) {
                  tmp <- substr(line, start, start + length - 1)
                })
  names(tmp) <- attr(match, "capture.names")
  tmp
}
#dotenv_parse_dot_line
dotenv_parse_dot_line <- function (line) {
  line_regex = "^\\s*(?<export>export\\s+)?(?<key>[^=]+)=(?<q>['\"]?)(?<value>.*)\\g{q}\\s*$"
  match <- regexpr(line_regex, line, perl = TRUE)
  if (match == -1) 
    stop("Cannot parse dot-env line: ", substr(line, 1, 40), 
         call. = FALSE)
  as.list(dotenv_extract_match(line, match)[c("key", "value")])
}

#' @name unload_workflow_environment
#' @aliases unload_workflow_environment
#' @title unload_workflow_environment
#' @description \code{unload_workflow_environment} unloads a workflow environment, in the case environment 
#' was provided by means of a dotenv file, and loaded using \pkg{dotenv} by \pkg{geoflow}. The function will
#' recover the session environment variables values (useful in case an environment variable was overwriten for
#' the workflow execution).
#' 
#' @usage unload_workflow_environment(config)
#' 
#' @param config object of class \link{list}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
unload_workflow_environment <- function(config){
  env_vars_workflow <- as.list(Sys.getenv())
  envfile <- config$profile_config$environment$file
  if(!is.null(envfile)){
    tmp <- readLines(envfile)
    tmp <- dotenv_ignore_comments(tmp)
    tmp <- dotenv_ignore_empty_lines(tmp)
    if (length(tmp) > 0){
      tmp <- lapply(tmp, dotenv_parse_dot_line)
      tmp <- structure(.Data = lapply(tmp, "[[", "value"), .Names = sapply(tmp, "[[", "key"))
      
      #remove env vars based on .env file
      Sys.unsetenv(names(tmp))
      
      #reset env vars previously in session env
      env_vars_before <- config$session_env
      env_vars_to_reset <- setdiff(env_vars_before, env_vars_workflow)
      if(length(env_vars_to_reset)>0) do.call(Sys.setenv, env_vars_to_reset)
    }
  }
}

#' @name is_absolute_path
#' @aliases is_absolute_path
#' @title is_absolute_path
#' @description \code{is_absolute_path} evaluate if a \code{${{path}}} expression is an absolute path, 
#' the function will return a boolean argument.
#' 
#' @usage is_absolute_path(path)
#' 
#' @param path a path in character string
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}

#'@name get_union_bbox
#'@aliases get_union_bbox
#'@title get_union_bbox
#'@description \code{get_union_bbox} will build a unified bounding box from a list of \code{geoflow_data} objects
#'
#'@usage get_union_bbox(data_objects)
#'
#'@param data_objects list of \code{geoflow_data} objects
#'
#'@author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'@export
get_union_bbox <- function(data_objects){
  
  bbox.df <- as.data.frame(do.call("rbind", lapply(data_objects, function(data_object){
    data_object.bbox <- NULL
    if(!is.null(data_object$features)){
      data_object.bbox <- sf::st_bbox(data_object$features)
    }else if(!is.null(data_object$coverages)){
      vec = data_object$coverages@ptr$extent$vector
      data_object.bbox <- c(xmin = vec[1], ymin = vec[3], xmax = vec[2], ymax = vec[4])
      class(data_object.bbox) <- "bbox"
    }
    return(data_object.bbox)
  })))
  
  union.bbox <- c(xmin = min(bbox.df[,1]), ymin = min(bbox.df[,2]), xmax = max(bbox.df[,3]), ymax = max(bbox.df[,4]))
  class(union.bbox) <- "bbox"
  return(union.bbox)
}

#'@name get_epsg_code
#'@aliases get_epsg_code
#'@title get_epsg_code
#'@description \code{get_epsg_code} is a consolidated method to get EPSG code (srid) from a CRS
#'
#'@usage get_epsg_code(x)
#'
#'@param x an object of class 'sf'
#'
#'@author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'@export
get_epsg_code = function(x){
  epsgcode = NA
  sf.crs <- sf::st_crs(x)
  if(!is.na(sf.crs)){
    epsgcode <- sf.crs$epsg
    if(!is.null(epsgcode)) {
      if(is.na(epsgcode)){
        #try to inherit epsg code from WKT definition (thanks to rspatial/terra)
        crs_wkt <- sf.crs$wkt
        if(!is.na(crs_wkt)) if(nzchar(crs_wkt)){
          crs_def <- terra::crs(crs_wkt, describe = TRUE)
          if(!is.null(crs_def$authority)) if(!is.na(crs_def$authority)) if(crs_def$authority == "EPSG"){
            epsgcode <-crs_def$code 
          }
        }
      }
      if(!is.na(epsgcode)) epsgcode = as.integer(epsgcode)
    }
  }
  return(epsgcode)
}

#'@name get_config_resource_path
#'@aliases get_config_resource_path
#'@title get_config_resource_path
#'@usage get_config_resource_path(config, path)
#'
#'@param config a \pkg{geoflow} config
#'@param path a resource path to resolve vs. the config root dir
#'
#'@author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'@export
get_config_resource_path <- function(config, path){
  is_url <- regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", path) > 0
  if(is_url) return(path)
  if(!is_absolute_path(path)){
    path_root = config$root
    mtch = gregexpr("\\.\\./", path)[[1]]
    mtch = mtch[mtch != -1]
    if(length(mtch)>0) for(i in 1:length(mtch)){
      path_root = dirname(path_root)
    }
    path = gsub("\\.\\./", "", path)
    if(startsWith("./", path)) path = unlist(strsplit(path, "\\./"))[2]
    path = file.path(path_root, path)
  }
  return(path)
}

#'@name getDBTableComment
#'@aliases getDBTableComment
#'@title getDBTableComment
#'
#'@usage getDBTableComment(dbi, schema, table)
#'
#'@param dbi a dbi connection
#'@param schema schema
#'@param table table
#'@return the table comment

#'@author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'@export
getDBTableComment = function(dbi, schema, table){
  get_comment_sql = sprintf("select obj_description('%s.%s'::regclass, 'pg_class')",
                            paste0('"',schema,'"'), paste0('"',table,'"'))
  get_comment = DBI::dbGetQuery(dbi, get_comment_sql)
  return(get_comment$obj_description)
}

#'@name getDBTableColumnComment
#'@aliases getDBTableColumnComment
#'@title getDBTableColumnComment
#'
#'@usage getDBTableColumnComment(dbi, schema, table, column_index)
#'
#'@param dbi a dbi connection
#'@param schema schema
#'@param table table
#'@param column_index table column index
#'@return the table comment
#'@author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'@export
getDBTableColumnComment = function(dbi, schema, table, column_index){
  get_comment_sql = sprintf("select col_description('%s.%s'::regClass, %s)",
                            paste0('"',schema,'"'), paste0('"',table,'"'), column_index)
  get_comment = DBI::dbGetQuery(dbi, get_comment_sql)
  return(get_comment$col_description)
}

#'@name create_geoflow_data_from_dbi
#'@aliases create_geoflow_data_from_dbi
#'@title create_geoflow_data_from_dbi
#'
#'@usage create_geoflow_data_from_dbi(dbi, schema, table)
#'
#'@param dbi a dbi connection
#'@param schema schema
#'@param table table
#'
#'@author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'@export
create_geoflow_data_from_dbi <- function(dbi, schema, table){
  entity_data = geoflow_data$new()
  sql = sprintf("select * from %s.%s", paste0('"',schema,'"'), paste0('"',table,'"'))
  entity_data$setSourceSql(sql)
  entity_data$setSourceType("dbquery")
  entity_data$setSpatialRepresentationType("vector")
  entity_data$setUploadType("dbtable")
  entity_data$setUploadSource(table)
  #data/feature type
  fto = geoflow_featuretype$new(id = table)
  data_sample = sf::st_read(dbi, query = paste(sql, "limit 1;"))
  for(colname in colnames(data_sample)){
    col_idx = which(colnames(data_sample) == colname)
    col_comment = getDBTableColumnComment(dbi, schema, table, col_idx)
    if(is.na(col_comment)) col_comment = colname
    
    ftm = geoflow_featuremember$new(
      type = if(is(data_sample[[colname]], "character")) "attribute" else "variable",
      code = colname,
      name = col_comment,
      def = col_comment,
      defSource = NA,
      minOccurs = 0,
      maxOccurs = 1,
      uom = NA
    )
    fto$addMember(ftm)
  }
  entity_data$setFeatureType(table)
  entity_data$setFeatureTypeObj(fto)
  return(entity_data)
}

#'@name fetch_layer_styles_from_dbi
#'@aliases fetch_layer_styles_from_dbi
#'@title fetch_layer_styles_from_dbi
#'
#'@usage fetch_layer_styles_from_dbi(entity, dbi, schema, table)
#'
#'@param entity a \link{geoflow_entity} to be used and enriched
#'@param dbi a dbi connection
#'@param schema schema
#'@param table table
#'@return the entity, enriched with layer styles
#'
#'@author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'@export
fetch_layer_styles_from_dbi <- function(entity, dbi, schema, table){
  if(DBI::dbExistsTable(dbi, "layer_styles")){
    #assume this is a special table
    styles_sql = sprintf("select * from layer_styles where f_table_schema='%s' and f_table_name='%s'", 
                         schema, table)
    styles = DBI::dbGetQuery(dbi, statement = styles_sql)
    if(nrow(styles)>0){
      styles[order(styles$useasdefault,decreasing = T),] #make sure we list the default one first
      #add style names in geoflow_data
      for(i in 1:nrow(styles)){
        style = styles[i,]
        entity$data$addStyle(style$stylename)
      }
      #add style defs as entity resource to delegate copy after entity dir is created
      entity$addResource("layer_styles", styles)
    }
  }
  return(entity)
}

#'@name describeOGCRelation
#'@aliases describeOGCRelation
#'@title describeOGCRelation
#'
#'@usage describeOGCRelation(entity, data_object, service, download, format,
#'                           handle_category, handle_ogc_service_description, handle_format)
#'
#'@param entity the entity considered
#'@param data_object data object
#'@param service service acronym
#'@param download whether the relation should be a download one or not
#'@param format format
#'@param handle_category append the relation category
#'@param handle_ogc_service_description append the OGC service description
#'@param handle_format append the download format
#'
#'@author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'@export
describeOGCRelation <- function(entity, data_object, service, download = FALSE, format = NULL,
                                handle_category = TRUE, handle_ogc_service_description = TRUE, handle_format = TRUE){
  
  layername <- if(!is.null(data_object$layername)) data_object$layername else entity$identifiers$id
  layertitle = if(!is.null(data_object$layertitle)) data_object$layertitle else layername
  
  out <- switch(tolower(service),
                "wms" = {
                  out_wms_link = layertitle
                  if(handle_category) out_wms_link = set_i18n(
                    term_key = "map_access", 
                    expr = {
                      the_expr = "{{out_wms_link}} - {{term}}"
                      if(handle_ogc_service_description) the_expr = paste0(the_expr," - OGC Web Map Service (WMS)")
                      the_expr
                    },
                    out_wms_link = out_wms_link
                  )
                  out_wms_link
                },
                "wfs" = {
                  out_wfs_link = layertitle
                  if(handle_category) out_wfs_link = set_i18n(
                    term_key = if(download) "data_download" else "data_features_access",
                    expr = {
                      the_expr = "{{out_wfs_link}} - {{term}}"
                      if(handle_ogc_service_description) the_expr = paste0(the_expr, " - OGC Web Feature Service (WFS)")
                      if(handle_format && !is.null(format)) the_expr = paste0(the_expr, " - ", format)
                      the_expr
                    },
                    out_wfs_link = out_wfs_link
                  )
                  out_wfs_link
                },
                "wcs" = {
                  out_wcs_link = layertitle
                  if(handle_category) out_wcs_link = set_i18n(
                    term_key = if(download) "data_download" else "data_coverage_access",
                    expr = {
                      the_expr = "{{out_wcs_link}} - {{term}}"
                      if(handle_ogc_service_description) the_expr = paste0(the_expr, " - OGC Web Coverage Service (WCS)")
                      if(handle_format && !is.null(format)) the_expr = paste0(the_expr, " - ", format)
                      the_expr
                    },
                    out_wcs_link = out_wcs_link
                  )
                  out_wcs_link
                }
  )
  return(out)
}

#'@name create_object_identification_id
#'@aliases create_object_identification_id
#'@title create_object_identification_id
#'
#'@usage create_object_identification_id(prefix, str)
#'
#'@param prefix a character string
#'@param str a character string
#'@return a digested character string
#'@export
create_object_identification_id = function(prefix, str){
  paste(prefix, digest::digest(object = str, algo = "crc32", serialize = FALSE), sep = "_")
}


#'@description precompute_relationships
#'@aliases precompute_relationships
#'@title precompute_relationships
#'
#'@usage precompute_relationships(data, parent_key, child_key)
#'
#'@param data data
#'@param parent_key parent_key
#'@param child_key child_key
#'@param child_label child_label
#'@return a list of relationships
#'@export
precompute_relationships <- function(data, parent_key, child_key, child_label) {
  ordered_data <- data[order(data[[parent_key]], data[[child_key]]), ]
  relationships <- split(ordered_data[[child_key]], ordered_data[[parent_key]])
  rel_names = names(relationships)
  relationships <- lapply(relationships, function(x){ 
      lapply(x, function(x_el){ attr(x_el, "label") = data[data[,child_key] == x_el, child_label][1]; return(x_el) })
    })
  names(relationships) = rel_names
  return(relationships)
}


#'@name build_hierarchical_list
#'@aliases build_hierarchical_list
#'@title build_hierarchical_list
#'
#'@usage build_hierarchical_list(data, parent)
#'
#'@param parent parent
#'@param relationships relationships
#'@return a hierarchical list
#'@export
build_hierarchical_list <- function(parent, relationships) {
  children <- relationships[[parent]]
  children_names <- sapply(children, function(x){attr(x, "label")})
  children = children[order(children_names)]
  out <- list(text = if(parent == "<root>") parent else attr(parent, "label") )
  if(is.null(children)){
    out$icon = "fa-regular fa-note-sticky"
  }else{
    out$children <- lapply(children, build_hierarchical_list, relationships)
  }
  return(out)
}