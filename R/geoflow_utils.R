#'sanitize_str
#'@export
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

#' sanitize_date
#' @export
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

#'extract_kvp
#'@export
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

#'str_to_posix
#'@export
str_to_posix <- function(str){
  if(nchar(str)>7){
    str_format <- if(nchar(str)==10) "%Y-%m-%d" else "%Y-%m-%dT%H:%M:%S"
    out <- as.POSIXct(str, format = str_format)
  }else{
    out <- str
  }
  return(out)
}

#'filter_sf_bycqlfilter
#'@export
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

#'extract_cell_components
#'@export
extract_cell_components <- function(x){
  lines <- unlist(strsplit(x, get_line_separator()))
  return(lines)
}

#'set_line_separator
#'@export
set_line_separator <- function(x = ";\n"){
  if(!is(x,"character")) stop("The line separator should be an object of class 'character'")
  .geoflow$LINE_SEPARATOR <- x
}

#'get_line_separator
#'@export
get_line_separator <- function(){
  return(.geoflow$LINE_SEPARATOR)
}