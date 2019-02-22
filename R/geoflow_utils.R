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
  name <- kvp[1]
  name_splits <- unlist(strsplit(name, "@"))
  if(length(name_splits)>1){
    name <- name_splits[1]
    attr(name,"uri") <- name_splits[2]
  }
  
  #values
  values <- unlist(strsplit(kvp[2], ","))
  values <- lapply(values, function(value){
    value_splits <- unlist(strsplit(value, "@"))
    if(length(value_splits)>1){
      value <- value_splits[1]
      attr(value, "uri") <- value_splits[2]
    }
    return(value)
  })
  
  return(list(key = name, values = values))
}