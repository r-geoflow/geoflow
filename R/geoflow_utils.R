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

#' @name extract_kvp
#' @aliases extract_kvp
#' @title extract_kvp
#' @description \code{extract_kvp} parses a string into a key value pair represented by
#' a \code{geoflow_kvp} object.
#'
#' @usage extract_kvps(strs)
#'                 
#' @param str a string as object of class \code{character}
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
    tmp <- dotenv:::ignore_comments(tmp)
    tmp <- dotenv:::ignore_empty_lines(tmp)
    if (length(tmp) > 0){
      tmp <- lapply(tmp, dotenv:::parse_dot_line)
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
