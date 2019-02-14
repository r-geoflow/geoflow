#' handle_entities_df
#' @export
handle_entities_df <- function(config, source){
  
  if(!is(source, "data.frame")){
    errMsg <- "Error in 'handle_entities_df': source parameter should be an object of class 'data.frame'"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  entities <- list()
  rowNum <- nrow(source)
  config$logger.info(sprintf("Parsing %s entities from tabular source", rowNum))
  for(i in 1:rowNum){
    source_entity <- source[i,]
    entity <- geoflow_entity$new()
    
    #TODO parsing of metadata entity elements and encapsulating in geoflow_entity obj
    
    
    entities <- c(entities, entity)
  }
  return(entities)
}

#' handle_entities_gsheets
#' @export
handle_entities_gsheet <- function(config, source){
  
  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  
  #apply generic handler
  entities <- handle_entities_df(config, source)
  return(entities)
}

#' handle_entities_csv
#' @export
handle_entities_csv <- function(config, source){
  
  #read csv TODO -> options management: sep, encoding etc
  source <- read.csv(source)
  
  #apply generic handler
  contacts <- handle_entities_df(config, source)
  return(contacts)
}
