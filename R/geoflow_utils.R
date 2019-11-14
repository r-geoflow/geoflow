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
  #kvp <- unlist(strsplit(str, ":(?!//|\\d)", perl = T))
  kvp <- unlist(strsplit(str, ":(?!//)", perl = T))
  if(length(kvp)==1) stop("Error while splitting kvp key/value")
  if(length(kvp)>2) kvp[2] <- paste(kvp[2:length(kvp)], collapse=":", sep="")
  
  #key
  key <- kvp[1]
  key_splits <- unlist(strsplit(key, "@"))
  if(length(key_splits)>1){
    key <- key_splits[1]
    attr(key,"uri") <- key_splits[2]
  }
  
  #values
  values <- unlist(strsplit(kvp[2], ","))
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
      attributes(value) <- attrs
      des <- value_splits[2]
      des <- substr(des, 1, nchar(des)-1)
      attr(value, "description") <- des
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
  if(nchar(str)>7){
    str_format <- if(nchar(str)==10) "%Y-%m-%d" else "%Y-%m-%dT%H:%M:%S"
    out <- as.POSIXct(str, format = str_format)
  }else{
    out <- str
  }
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