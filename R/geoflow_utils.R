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
  
  return(list(key = key, values = values))
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
#' @param x a string as object of class \code{character}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
extract_cell_components <- function(x){
  lines <- unlist(strsplit(x, get_line_separator()))
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

#' @name set_temp_directory
#' @aliases set_temp_directory
#' @title set_temp_directory
#' @description \code{set_temp_directory} set the temp directory name to be used by geoflow
#' to download data and store temporary files.
#'
#' @usage set_temp_directory(x)
#'                 
#' @param x a string as object of class \code{character} representing the  temp directory.
#  Default is set to "geoflow_temp_data".
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
set_temp_directory <- function(x = "geoflow_temp_data"){
  if(!is(x,"character")) stop("The dir name should be an object of class 'character'")
  .geoflow$TEMP_DATA_DIR <- x
}

#' @name get_temp_directory
#' @aliases get_temp_directory
#' @title get_temp_directory
#' @description \code{set_temp_directory} get the temp directory name used by geoflow
#' to download data and store temporary files. Default is set to "geoflow_temp_data".
#'
#' @usage get_temp_directory()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
get_temp_directory <- function(){
  return(.geoflow$TEMP_DATA_DIR)
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

#' @name download_file
#' @aliases download_file
#' @title download_file
#' @description \code{download_file} downloads the file. Depending on the storage,
#' different download strategies will be applied, e.g. Google drive file.
#'
#' @usage download_file(url, filename)
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
download_file <- function(url, filename){
  googledrive_baseurl <- "https://drive.google.com/open?id="
  if(startsWith(url, googledrive_baseurl)){
    #managing download through google drive
    drive_id <- unlist(strsplit(url, "id="))[2]
    drive_id <- unlist(strsplit(drive_id, "&export"))[1] #control in case export param is appended
    googledrive::drive_download(file = googledrive::as_id(drive_id), path = filename)
  }else{
    #classic download
    download.file(url, destfile = filename, mode = "wb")
  }
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