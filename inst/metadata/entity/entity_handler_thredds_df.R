#handle_entities_thredds_df
handle_entities_thredds_df = function(handler, source, config){
  
  entities <- handle_entities_df(handler, source, config)
  
  thredds_entities <- lapply(entities, function(entity){
    
    thredds_source <- entity$data$source[[1]]
    
    thredds_entity <- handle_entities_thredds(handle, thredds_source, config)[[1]]
    
    #identifiers (priority to df)
    if(is.null(entity$identifiers$doi)) if(!is.null(thredds_entity$identifiers$doi)) entity$identifiers$doi<-thredds_entity$identifiers$doi
    
    #titles (priority to df)
    if(is.null(entity$titles$title)) if(!is.null(thredds_entity$entity$titles$title)) entity$identifiers$doi<-thredds_entity$entity$titles$title
    
    #descriptions (priority to df)
    if(is.null(entity$descriptions$abstract)) if(!is.null(thredds_entity$descriptions$abstract)) entity$identifiers$doi<-thredds_entity$descriptions$abstract
    if(is.null(entity$descriptions$edition)) if(!is.null(thredds_entity$descriptions$edition)) entity$identifiers$doi<-thredds_entity$descriptions$edition
    if(is.null(entity$descriptions$credit)) if(!is.null(thredds_entity$descriptions$credit)) entity$identifiers$doi<-thredds_entity$descriptions$credit
    
    #subjects (cumulative)
    entity$subjects<-unique(c(entity$subjects,thredds_entity$subjects))
    
    #contacts (cumulative)
    entity$contacts<-unique(c(entity$contacts,thredds_entity$contacts))
    
    #dates (priority to thredds)
    entity$dates<-thredds_entity$dates
    
    #types (use df)
    #language (use df)
    
    #spatial coverage (priority to thredds)
    if(!is.null(thredds_entity$spatial_extent)) entity$spatial_extent<-thredds_entity$spatial_extent
    if(!is.null(thredds_entity$spatial_bbox)) entity$spatial_bbox<-thredds_entity$spatial_bbox
    
    #temporal coverage (priority to thredds)
    if(!is.null(thredds_entity$temporal_extent)) entity$temporal_extent<-thredds_entity$temporal_extent
    
    #relations (cumulative)
    entity$relations<-unique(c(entity$relations,thredds_entity$relations))
    
    #rights (cumulative)
    entity$rights<-unique(c(entity$rights,thredds_entity$rights))
    
    #formats (use df)
    #provenance (use df)
    
    #data (use thredds)
    entity$data<-thredds_entity$data
    
    return(entity)
  })
  return(thredds_entities)
}