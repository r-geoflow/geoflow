#handle_entities_dataverse
handle_entities_dataverse <- function(config, source, handle = TRUE){
  
  if(!requireNamespace("dataverse", quietly = TRUE)){
    stop("The Dataverse handler requires the 'dataverse' package")
  }
  
  DATAVERSE <- config$software$input$dataverse
  if(is.null(DATAVERSE)){
    stop("There is no dataverse input software configured to handle entities from Dataverse")
  }
  DATAVERSE_CONFIG <- config$software$input$dataverse_config
  
  results <- dataverse::dataverse_search(
    source, server = DATAVERSE$server, per_page = 1000,
    dataverse = DATAVERSE_CONFIG$properties$dataverse
  )
  results <- results[!is.na(results$global_id),]
  
  entities <- lapply(1:nrow(results), function(i){
    result <- results[i,]
    ds <- dataverse::get_dataset(dataset = result$global_id, server = DATAVERSE$server)
    ds_doi <- unlist(strsplit(ds$datasetPersistentId, ":"))[2]
    ds_meta <- ds$metadataBlocks
    config$logger.info(sprintf("Creating entity (%s out of %s) from Dataverse dataset with DOI '%s'", i, nrow(results), ds_doi))
    
    #create entity
    entity <- geoflow::geoflow_entity$new()
    #entity Identifier
    entity$setIdentifier("id", ds_doi)
    entity$setIdentifier("doi", ds_doi)
    #entity Date
    if(!is.null(ds$productionDate)) entity$addDate(dateType = "production", date = str_to_posix(ds$productionDate))
    if(!is.null(ds$createTime)) entity$addDate(dateType = "creation",   date = str_to_posix(ds$createTime))
    if(!is.null(ds$releaseTime)) entity$addDate(dateType = "released",   date = str_to_posix(ds$releaseTime))
    if(!is.null(ds$lastUpdateTime)) entity$addDate(dateType = "lastUpdate", date = str_to_posix(ds$lastUpdateTime))
    
    #entity common metadata
    citation <- ds$metadataBlocks$citation
    if(!is.null(citation)){
      fields <- ds$metadataBlocks$citation$fields
      #entity Type
      entity$setType(key = "generic", tolower(citation$fields[citation$fields$typeName == "kindOfData",]$value[[1]]))
      #entity Title
      entity$setTitle("title", citation$fields[citation$fields$typeName == "title",]$value)
      #entity Description
      entity$setDescription("abstract", citation$fields[citation$fields$typeName == "dsDescription",]$value[[1]]$dsDescriptionValue$value)
      #entity subjects
      subjects <- citation$fields[citation$fields$typeName == "subject",]$value
      keywords <- citation$fields[citation$fields$typeName == "keyword",]$value
      for(i in 1:length(subjects)){
        subject <- subjects[[i]]
        keywords <- keywords[[i]]$keywordValue$value
        
        subj <- geoflow::geoflow_subject$new()
        subj$setKey(subject)
        subj$setName(subject)
        for(keyword in keywords) subj$addKeyword(keyword)
        entity$addSubject(subj)
      }
      #entity 'topic' subject
      topics <- citation$fields[citation$fields$typeName == "topicClassification",]$value
      for(topic in topics){
        subj <- geoflow::geoflow_subject$new()
        subj$setKey("topic")
        subj$addKeyword(topic)
        entity$addSubject(subj)
      }
      #entity Contacts/roles
      #pointOfContact
      datasetContacts <- citation$fields[citation$fields$typeName == "datasetContact",]$value
      for(datasetContact in datasetContacts){
        datasetContactNames <- unlist(strsplit(datasetContact$datasetContactName$value," "))
        poc <- geoflow::geoflow_contact$new()
        poc$setIdentifier(key = "id", datasetContact$datasetContactEmail$value)
        poc$setFirstName(datasetContactNames[1])
        poc$setLastName(datasetContactNames[2])
        poc$setRole("pointOfContact")
        entity$addContact(poc)
        #metadata
        meta <- poc$clone()
        meta$setRole("metadata")
        entity$addContact(meta)
      }
      #author(s)
      authors <- citation$fields[citation$fields$typeName == "author",]$value
      for(author in authors){
        authorNames <- unlist(strsplit(author$authorName$value, ", "))
        author_c <- geoflow::geoflow_contact$new()
        author_c$setFirstName(authorNames[2])
        author_c$setLastName(authorNames[1])
        author_c$setRole("author")
        entity$addContact(author_c)
      }
      #contributor(s)
      contribs <- citation$fields[citation$fields$typeName == "contributor",]$value
      for(contrib in contribs){
        contribNames <- unlist(strsplit(contrib$contributorName$value, ", "))
        contrib_c <- geoflow::geoflow_contact$new()
        contrib_c$setFirstName(contribNames[2])
        contrib_c$setLastName(contribNames[1])
        contrib_c$setRole("contributor")
        entity$addContact(contrib_c)
      }
      #processor(s)
      producers <- citation$fields[citation$fields$typeName == "producer",]$value
      for(producer in producers){
        producerNames <- unlist(strsplit(producer$producerName$value, ", "))
        producer_c <- geoflow::geoflow_contact$new()
        producer_c$setFirstName(producerNames[2])
        producer_c$setLastName(producerNames[1])
        producer_c$setRole("processor")
        entity$addContact(producer_c)
      }
      #relations
      #TODO related material
      
      #license / terms of use
      rights <- geoflow::geoflow_right$new()
      if(!is.null(ds$termsOfUse)) if(ds$termsOfUse == "license") {
        rights$setKey("useConstraint")
        rights$setValues(ds$termsOfUse)
        if(!is.null(ds$license)) if(ds$license != "NONE") {
          rights$setKey("license")
          rights$setValues(ds$license)  
        }
      }
      
    }
    
    #entity SpatialCoverage
    geospatial <- ds$metadataBlocks$geospatial
    if(!is.null(geospatial)) if(length(geospatial$fields)>0){
      geocov <- geospatial$fields[geospatial$fields$typeName == "geographicCoverage",]
      if(nrow(geocov)>0){
        ewkt <- geocov$value[[1]]$otherGeographicCoverage$value
        if(!is.null(ewkt)){
          spatial_cov <- unlist(strsplit(ewkt, ";"))
          spatial_srid <- as.integer(unlist(strsplit(spatial_cov[1],"SRID="))[2])
          spatial_cov <- spatial_cov[2]
          entity$setSrid(spatial_srid)
          entity$setSpatialExtent(spatial_cov, crs = spatial_srid) 
        }
      }
    }
    #TODO entity TemporalCoverage
    
    #data
    if(nrow(ds$files)>0){
      data <- geoflow::geoflow_data$new()
      data$setAccess("dataverse")
      data$source <- lapply(1:nrow(ds$files), function(i){
        ds_file <- ds$files[i,]
        ds_filename <- ds_file$filename
        attr(ds_filename, "uri") <- ds_doi
        return(ds_filename)
      })
      data$upload <- TRUE
      entity$setData(data)
    }
    return(entity)
  })
  
  
  return(entities)
}
