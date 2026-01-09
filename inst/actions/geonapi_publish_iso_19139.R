function(action, entity, config){
  
  if(!requireNamespace("geonapi", quietly = TRUE)){
    stop("The 'geonapi-publish-iso-19139' action requires the 'geonapi' package")
  }
  
  #action options
  publish_thumbnails <- action$getOption("publish_thumbnails")
  create_doi_on_datacite <- action$getOption("create_doi_on_datacite")
  
  #INSPIRE metadata validation
  geometa_inspire <- action$getOption("geometa_inspire")
  INSPIRE_VALIDATOR <- NULL
  #as of 2025-05-02, there is no need anymore to have an API key to validate metadata
  #therefore the INSPIRE metadata validator software declaration is not needed
  # if(geometa_inspire){
  #   #check inspire metadata validator configuration
  #   INSPIRE_VALIDATOR <- config$software$output$inspire
  #   if(is.null(INSPIRE_VALIDATOR)){
  #     errMsg <- "This action requires a INSPIRE metadata validator software to be declared in the configuration"
  #     config$logger$ERROR(errMsg)
  #     stop(errMsg)
  #   }
  #   config$logger$INFO("INSPIRE geometa option enabled: The record will be checked against the INSPIRE reference validator prior its publication")
  # }
  
  #shortcut for gn config
  GN <- config$software$output$geonetwork
  
  if(is.null(GN)){
    errMsg <- "This action requires a Geonetwork software to be declared in the configuration"
    config$logger$ERROR(errMsg)
    stop(errMsg)
  }
  
  #to insert or update a metadata into a geonetwork.
  #An insert has to be done in 2 operations (the insert itself, and the privilege setting to "publish" it either to a restrained group or to public)
  #An update has to be done based on the internal Geonetwork id (that can be queried as well
  #function doPublish
  doPublish <- function(mdfile, inspire){
    
    privileges <- action$getOption("privileges")
    mdId <- NULL
    md <- readISO19139(metaFile)
    privs <- privileges
    if(is(md, "ISOMetadata")){
      mdId <- md$fileIdentifier
      privs <- privileges[privileges!="featured"]
    }
    if(is(md, "ISOFeatureCatalogue")){
      mdId <- md$attrs[["uuid"]]
      privs <- "view"
    }
    
    #group
    group <- action$getOption("group") #2 = sample (provided by default GN installation)
    group_match_col<-if(!grepl("\\D", group)){"id"}else{"name"}
    available_groups <- GN$getGroups()
    if(!group %in% available_groups[[group_match_col]]){
      errMsg <- sprintf("Geonetwork: no group for %s = %s - Please check below the Geonetwork available groups",group_match_col, group)
      config$logger$ERROR(errMsg)
      print(available_groups)
      stop(errMsg)
    }else{
      if(group_match_col=="name"){
        group <- available_groups[available_groups$name==group,]$id
      }
    }
    #category
    category <- action$getOption("category")
    category_match_col<-if(!grepl("\\D", category)){"id"}else{"name"}
    available_categories <- GN$getCategories()
    if(!category %in% available_categories[[category_match_col]]){
      errMsg <- sprintf("Geonetwork: no category for %s = %s - Please check below the Geonetwork available categories",category_match_col, category)
      config$logger$ERROR(errMsg)
      print(available_categories)
      stop(errMsg)
    }
    
    switch(GN$getClassName(),
           "GNOpenAPIManager" = {
             if(category_match_col=="name"){
               category <- available_categories[available_categories$name==category,]$id
             }
             GN$insertRecord(geometa = md, group = group, category = category,
                             uuidProcessing = "OVERWRITE", geometa_inspire = inspire, geometa_inspireValidator = INSPIRE_VALIDATOR)
             #config privileges
             config <- GNPrivConfiguration$new()
             config$setPrivileges(as.character(group), privs)
             if(entity$data$restricted){
               config$setPrivileges("all", c("view"))
             }
             GN$setPrivConfiguration(id = md$fileIdentifier, config = config)
           },
           "GNLegacyAPIManager" = {
             if(category_match_col=="id"){
               category <- available_categories[available_categories$id==category,]$name
             }
             metaId <- GN$get(mdId, by = "uuid", output = "id")
             if(is.null(metaId)){
               #insert metadata (once inserted only visible to the publisher)
               created = GN$insertMetadata(geometa = md, group = group, category = category,
                                           geometa_inspire = inspire, geometa_inspireValidator = INSPIRE_VALIDATOR)
               #config privileges
               config <- GNPrivConfiguration$new()
               config$setPrivileges("all", privs)
               GN$setPrivConfiguration(id = created, config = config)
             }else{
               #update a metadata
               updated = GN$updateMetadata(id = metaId, geometa = md,
                                           geometa_inspire = inspire, geometa_inspireValidator = INSPIRE_VALIDATOR)
               
               #config privileges
               gn_config <- GNPrivConfiguration$new()
               gn_config$setPrivileges("all", privs)
               GN$setPrivConfiguration(id = metaId, config = gn_config)
             }
           }
    )
    rm(md)
    return(mdId)
  }
  
  #geometa ISO 19115
  geometa_iso19115_action <- NULL
  actions <- config$actions[sapply(config$actions, function(x){x$id=="geometa-create-iso-19115"})]
  if(length(actions)>0) geometa_iso19115_action <- actions[[1]]
  if(!is.null(geometa_iso19115_action)){
    metaFile <- file.path("metadata", paste0(entity$identifiers[["id"]],"_ISO-19115.xml"))
    if(file.exists(metaFile)){
      #publication
      mdId = doPublish(metaFile, geometa_inspire)
      if(create_doi_on_datacite){
        if(is.null(entity$identifiers[["doi"]])){
          config$logger$INFO("Creating DOI on DataCite...")
          config$logger$INFO("Checking DOI registration pre-conditions...")
          checked = GN$doiCheckPreConditions(mdId)
          if(checked){
            config$logger$INFO("DOI registration pre-conditions are met, proceed with DOI registration")
            created = GN$createDOI(mdId)
            if(created){
              doi_report = attr(created, "report")
              doi = doi_report$doi
              entity$identifiers$doi = doi
              config$logger$INFO("DOI '%s' successfuly created for metadata '%s'", doi, mdId)
            }
          }else{
            config$logger$WARN("Aborting DOI creation, pre-conditions are not met!")
          }
        }else{
          config$logger$WARN("A DOI is already declared as entity identifier, skip DOI creation on DataCite")
        }
      }
      
      #upload thumbnails?
      if(GN$getClassName() == "GNOpenAPIManager") if(publish_thumbnails){
        #filter on entity thumbnails that are local
        entity_thumbnails <- entity$relations[sapply(entity$relations, function(rel){
          rel$key == "thumbnail" && regexpr("(http|https)[^([:blank:]|\\\"|<|&|#\n\r)]+", rel$link) < 0
        })]
        #manage absolute paths
        if(length(entity_thumbnails)>0) entity_thumbnails <- lapply(entity_thumbnails, function(rel){
          if(!geoflow::is_absolute_path(rel$link)) rel$link <- file.path(config$session_wd, rel$link)
          return(rel)
        })
        if(length(entity_thumbnails)>0) for(entity_thumbnail in entity_thumbnails){
          uploaded <- GN$uploadAttachment(mdId, entity_thumbnail$link)
          if(!is.null(uploaded)){
            desc <- if(!is.null(entity_thumbnail$description)) entity_thumbnail$description else ""
            published <- GN$publishThumbnail(mdId, uploaded$url, desc)
            if(published){
              config$logger$INFO("Successfully published thumbnail '%s' to metadata '%s'",
                                         entity_thumbnail$link, mdId)
            }else{
              config$logger$ERROR("Error while publishing thumbnail file '%s' to metadata '%s'", 
                                          entity_thumbnail$link, mdId)
            }
          }else{
            config$logger$ERROR("Error while attaching thumbnail file '%s' to metadata '%s'", 
                                        entity_thumbnail$link, mdId)
          }
        }
      }
      
    }else{
      config$logger$WARN("No ISO 19115 XML metadata file to publish for entity '%s, skipping action!", entity$identifiers[["id"]])
    }
  }
  #geometa ISO 19110
  geometa_iso19110_action <- NULL
  actions <- config$actions[sapply(config$actions, function(x){x$id=="geometa-create-iso-19110"})]
  if(length(actions)>0) geometa_iso19110_action <- actions[[1]]
  if(!is.null(geometa_iso19110_action)){
    geometa_inspire <- FALSE
    metaFile <- file.path("metadata", paste0(entity$identifiers[["id"]],"_ISO-19110.xml"))
    if(file.exists(metaFile)){
      mdId = doPublish(metaFile, geometa_inspire)
    }else{
      config$logger$WARN(sprintf("No ISO ISO 19110 XML metadata file to publish for entity '%s, skipping action!", entity$identifiers[["id"]]))
    }
  }
  
  return(TRUE)
}
