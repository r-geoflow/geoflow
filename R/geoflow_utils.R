#'sanitize_str
#'@export
sanitize_str <- function(str){
  str <- gsub("\n", ";", str)
  str <- gsub(";;", ";", str)
  return(str)
}


#'extract_kvp
#'@export
extract_kvp <- function(str){
  kvp <- unlist(strsplit(str, ":(?!//|\\d)", perl = T))
  if(length(kvp)!=2) stop("Error while splitting kvp key/value")
  
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
    hasDescription <- attr(regexpr("\\[", value), "useBytes") & endsWith(value, "]")
    if(hasDescription){
      value_splits <- unlist(strsplit(value, "\\["))
      value <- value_splits[1]
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
  str_format <- if(nchar(str)==10) "%Y-%m-%d" else "%Y-%m-%dT%H:%M:%S"
  out <- as.POSIXct(str, format = str_format)
  return(out)
}