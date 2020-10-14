eml_create_eml <- function(entity, config, options){
  
  if(!requireNamespace("EML", quietly = TRUE)){
    stop("The EML action requires the 'EML' package")
  }
  
  #options
  taxonomySubject <- if(!is.null(options$subject_taxonomy)) options$subject_taxonomy else "taxonomy"
  
  #init dataset
  dataset <- EML::eml$dataset()
  
  #get doi
  the_doi <- entity$identifiers[["doi"]]
  if(is.null(the_doi)) the_doi <- entity$identifiers[["conceptdoi_to_save"]]
  if(is.null(the_doi)) the_doi <- entity$identifiers[["doi_to_save"]]
  
  #identifiers
  dataset$id <- entity$identifiers[["id"]]
  if(!is.null(the_doi)){
    dataset$alternateIdentifier <- list(directory = "https://orcid.org", userId = the_doi)
  }
  
  #short name
  dataset$shortName <- entity$title
  
  #title
  dataset$title <- entity$title
  
  #contact IDs already added
  contact_ids <- list()
  
  #helper functions
  #contactToEML function
  contactToEML <- function(contact){
    person <- list(id = contact$id)
    if(!is.na(contact$firstName)) person$individualName$givenName <- contact$firstName
    if(!is.na(contact$lastName)) person$individualName$surName <- contact$lastName
    if(!is.na(contact$organizationName)) person$organizationName <- contact$organizationName
    if(!is.na(contact$positionName)) person$positionName <- contact$positionName
    #address
    address <- list()
    if(!is.na(contact$postalAddress)) address$deliveryPoint <- contact$postalAddress
    if(!is.na(contact$postalCode)) address$postalCode<- contact$postalCode
    if(!is.na(contact$city)) address$city <- contact$city
    if(!is.na(contact$country)) address$country <- contact$country
    if(length(address)>0) person$address <- address
    #contact resources
    if(!is.na(contact$voice)) person$phone <- contact$voice
    if(!is.na(contact$email)) person$lectronicMailAddress <- contact$email
    if(!is.na(contact$websiteUrl)) person$onlineUrl <- contact$websiteUrl
    if(length(contact$identifiers)>0){
      for(id in contact$identifiers){
        switch(id$key,
          "orcid" = {
            person$id <- sprintf("https://orcid.org/%s", id$value)
            person$userId <- list(directory = "https://orcid.org", userId = person$id)
          }       
        )
      }
    }
    return(person)
  }
  
  #contactsToEMLRole
  contactsToEML <- function(contacts){
    persons <- lapply(contacts, contactToEML)
    out_persons <- lapply(persons, function(person){
      if(person$id %in% contact_ids){
        list(references = person$id)
      }else{
        contact_ids <<- c(contact_ids, person$id)
        person
      }
    })
    return(out_persons)
  }

  #creator
  creators <- entity$getContacts()[sapply(entity$getContacts(), function(x){x$role=="owner"})]
  dataset$creator <- contactsToEML(creators)
  
  #metadataProvider
  md_providers <- entity$getContacts()[sapply(entity$getContacts(), function(x){x$role=="metadata"})]
  if(length(md_providers)>0) dataset$metadataProvider <- contactsToEML(md_providers)
  
  #pubDate
  dataset$pubDate <- Sys.time()
  pub_dates <- entity$dates[sapply(entity$dates, function(x){x$key == "publication"})]
  if(length(pub_dates)>0) dataset$pubDate <- pub_dates[[1]]$value
  
  #language
  dataset$language <- entity$language
  
  #abstract
  if(!is.null(entity$descriptions$abstract)) dataset$abstract <- list(
    section = list(),
    para = entity$descriptions$abstract
  )
  
  #keywords
  if(length(entity$subjects)>0){
    dataset$keywordSet <- lapply(entity$subjects, function(subject){
      list(
        keywordThesaurus = subject$name,
        keyword = lapply(subject$keywords, function(keyword){keyword$name})
      )
    })
  }
  
  #additionalInfo
  if(!is.null(entity$descriptions$info)) dataset$additionalInfo <- list(para = entity$descriptions$info)
  
  #TODO intellectualRights
  rights <- entity$rights[sapply(entity$rights, function(x){x$key == "use"})]
  dataset$intellectualRights <- paste(sapply(rights, function(x){x$value}), sep = "\n")
  
  #TODO license
  #TODO distribution
  
  #coverage
  #geo
  if(!is.null(entity$spatial_bbox)){
    if(is.null(dataset$coverage)) dataset$coverage <- list()
    dataset$coverage$geographicCoverage <- list(
      boundingCoordinates = list(
        westBoundingCoordinate = entity$spatial_bbox$xmin,
        eastBoundingCoordinate = entity$spatial_bbox$xmax,
        southBoundingCoordinate = entity$spatial_bbox$ymin,
        northBoundingCoordinate = entity$spatial_bbox$ymax
      )
    )
  }
  #temporal
  if(!is.null(entity$temporal_extent)){
    if(is.null(dataset$coverage)) dataset$coverage <- list()
    dataset$coverage$temporalCoverage <- list(
      rangeOfDates = list(
        beginDate = list(calendarDate = as.character(as.Date(entity$temporal_extent$start))),
        endDate = list(calendarDate = as.character(as.Date(entity$temporal_extent$end)))
      )
    )
  }
  #taxonomic
  taxo <- entity$subjects[sapply(entity$subjects, function(x){x$name == taxonomySubject})]
  if(length(taxo)>0){
    taxo <- taxo[[1]]
    if(is.null(dataset$coverage)) dataset$coverage <- list()
    dataset$coverage$taxonomicCoverage = list(
      taxonomicClassification = lapply(taxo$keywords, function(keyword){
        kwd_name = keyword$name
        kwd_parts = unlist(strsplit(kwd_name, " "))
        taxocl <- list(
          taxonRankName = "Genus",
          taxonRankValue = kwd_parts[1]
        )
        if(length(kwd_parts)>1){
          str <- paste(kwd_parts[2:length(kwd_parts)], collapse = " ")
          taxocl$taxonomicClassification = list(
            taxonRankName = "Species",
            taxonRankValue = str
          )
        }
        return(taxocl)
      })
    )
  }
  
  #purpose
  if(!is.null(entity$descriptions$purpose)) dataset$purpose <- list(para = entity$descriptions$purpose)
  
  #contact
  pocs <- entity$getContacts()[sapply(entity$getContacts(), function(x){x$role=="pointOfContact"})]
  if(length(pocs)>0) dataset$contact <- contactsToEML(pocs)
  
  #publisher
  publishers <- entity$getContacts()[sapply(entity$getContacts(), function(x){x$role=="publisher"})]
  if(length(publishers)>0) dataset$publisher <- contactsToEML(publishers)
  
  #methods
  if(!is.null(entity$provenance)){
    if(length(entity$provenance$processes)>0){
      dataset$methodStep <- lapply(entity$provenance$processes, function(process){
        list(description = paste0(process$rationale, ": ", process$description))
      })
    }
  }
  
  #project
  project <- NULL
  if(!is.null(config$profile$project)) project <- config$profile$project
  if(!is.null(entity$descriptions$project)) project <- entity$descriptions$project
  
  #attributes
  createAttribute = function(attributeName = character(0),
                             attributeLabel = character(0),
                             attributeDefinition = character(0),
                             measurementScale = character(0),
                             domain = character(0),
                             formatString = character(0),
                             definition = character(0),
                             unit = character(0),
                             numberType = character(0),
                             missingValueCode = character(0),
                             missingValueCodeExplanation = character(0)){
    data.frame(
      attributeName = attributeName,
      attributeLabel = attributeLabel,
      attributeDefinition = attributeDefinition,
      measurementScale = measurementScale,
      domain = domain,
      formatString = formatString,
      definition = definition,
      unit = unit,
      numberType = numberType,
      missingValueCode = missingValueCode,
      missingValueCodeExplanation = missingValueCodeExplanation,
      stringsAsFactors = FALSE
    )
  }
    
  #spatialVector
  if(!is.null(entity$data)) if(!is.null(entity$data$features)){
    features = entity$data$features
    attributeList = createAttribute()
    columns <- colnames(features)
    for(featureAttrName in columns){
      config$logger.info(sprintf("EML: adding '%s' to attributeList", featureAttrName))
      fat_attr_register <- NULL
      
      #create attribute
      #default name (from data)
      memberName <- featureAttrName
      
      fat_attr <- NULL
      fat_attr_desc <- NULL
      fto <- entity$data$featureTypeObj
      if(!is.null(fto)) fat_attr <- fto$getMemberById(featureAttrName)
      if(!is.null(fat_attr)){
        fat_attr_desc <- fat_attr$name
        registerId <- fat_attr$registerId
        if(!is.null(registerId)) if(!is.na(registerId)){
          registers <- config$registers
          registers <- registers[sapply(registers, function(x){x$id == registerId})]
          if(length(registers)==0){
            warnMsg <- sprintf("Unknown register '%s'. Ignored for creating EML attributeList", registerId)
            config$logger.warn(warnMsg)
          }else{
            fat_attr_register <- registers[[1]]
          }
        }
        if(!is.null(fat_attr_desc)) memberName <- fat_attr_desc
      }
      
      
      #uom
      uom <- fat_attr$uom
      if(!is.null(uom)){
        if(!uom %in% EML::get_unitList()$units$id){
          config$logger.warn(sprintf("Unit '%s' not referenced in EML::get_unitList()$units IDs", uom))
          if(!uom %in% EML::get_unitList()$units$abbreviation){
            config$logger.warn(sprintf("Unit '%s' not referenced in EML::get_unitList()$units Abbreviations", uom))
          }else{
            config$logger.warn(sprintf("Unit '%s' found in EML::get_unitList()$units Abbreviations", uom))
            eml_unit = EML::get_unitList()$units[EML::get_unitList()$units$abbreviation == uom,]
            config$logger.warn(sprintf("Unit for '%s' attribute, set unit ID '%s' instead of abbreviation", featureAttrName, eml_unit[1L, "id"]))
            uom <- eml_unit
          }
        }
      }else{
        if(is(featureAttrValues, "sfc")){
          uom <- st_crs(features)$units
          if(is.null(uom)){
            if(entity$srid == 4326){
              uom <- "degree"
            }
          }
        }
      }
      if(is.null(uom)) uom = NA
      
      #add listed values
      featureAttrValues <- switch(class(features)[1],
                                  "sf" = features[,featureAttrName][[1]],
                                  "data.frame" = features[,featureAttrName]
      )
      #measurementScale
      measurementScale <- switch(class(featureAttrValues[1])[1],
                         "integer" = "ratio",
                         "numeric" = "ratio",
                         "character" = "nominal",
                         "logical" = "ordinal",
                         "Date" = "dateTime",
                         "POSIXct" = "dateTime",
                         "sfc_POINT" = "interval",
                         "sfc_MULTIPOINT" = "interval",
                         "sfc_LINESTRING" = "interval",
                         "sfc_MULTILINESTRING" = "interval",
                         "sfc_POLYGON" = "interval",
                         "sfc_MULTIPOLYGON" = "interval"
      )
      #domain
      domain <- switch(class(featureAttrValues[1])[1],
                       "integer" = "numericDomain",
                       "numeric" = "numericDomain",
                       "character" = "textDomain",
                       "logical" = "enumeratedDomain",
                       "Date" = "dateTime",
                       "POSIXct" = "dateTime",
                       "sfc_POINT" = "numericDomain",
                       "sfc_MULTIPOINT" = "numericDomain",
                       "sfc_LINESTRING" = "numericDomain",
                       "sfc_MULTILINESTRING" = "numericDomain",
                       "sfc_POLYGON" = "numericDomain",
                       "sfc_MULTIPOLYGON" = "numericDomain"
      )
      if(domain == "textDomain") if(!is.null(fat_attr_register)) if(is(fat_attr_register$data, "data.frame")){
        domain = "enumeratedDomain"
      }
      
      attribute = createAttribute(
        attributeName = featureAttrName,
        attributeLabel = memberName,
        attributeDefinition = ifelse(is.null(fat_attr_desc), "", fat_attr_desc),
        measurementScale = measurementScale,
        domain = domain,
        formatString = switch(class(featureAttrValues[1])[1],
          "Date" = "YYYY-MM-DD",
          "POSIXct" = "YYYY-MM-DD HH:mm:ss",
          NA
        ),
        definition = switch(domain, "textDomain" = ifelse(is.null(fat_attr_desc), NA, fat_attr_desc), NA),
        unit = switch(domain, "numericDomain" = uom, NA),
        numberType = switch(domain,
          "numericDomain" = {
            switch(class(featureAttrValues[1])[1],
              "integer" = "integer",
              "numeric" = "real",
              "real"
            )
          }, NA),
        missingValueCode = "NA",
        missingValueCodeExplanation = NA
      )
      attributeList <- rbind(attributeList, attribute)
      
    }
      
    if(is(entity$data$features, "sf")){
      #use spatialVector
      config$logger.info("EML: spatial dataset - filling attributeList as 'spatialVector'")
      dataset$spatialVector = eml$spatialVector(
        alternateIdentifier = basename(entity$getJobDataResource(config, entity$data$source[[1]])),
        entityName = entity$title,
        entityDescription = entity$descriptions[["abstract"]],
        coverage = dataset$coverage,
        attributeList = EML::set_attributes(attributes = attributeList),
        geometry = switch(class(st_geometry(entity$data$features))[1],
          "sfc_POINT" = "Point",
          "sfc_MULTIPOINT" = "MultiPoint",
          "sfc_LINESTRING" = "LineString",
          "sfc_MULTILINESTRING" = "MultiLineString",
          "sfc_POLYGON" = "Polygon",
          "sfc_MULTIPOLYGON" = "MultiPolygon"                
        ),
        geometricObjectCount = nrow(entity$data$features),
        spatialReference = list(horizCoordSysName = {
          if(!is.null(entity$srid)){
            paste0("EPSG:", entity$srid)
          }else{
            as(sf::st_crs(entity$data$features), "character")[1]
          }
        })
      )
    }else{
      #use dataTable
      config$logger.info("EML: non-spatial dataset - filling attributeList as 'dataTable'")
      dataset$dataTable = eml$dataTable(
        alternateIdentifier = basename(entity$getJobDataResource(config, entity$data$source[[1]])),
        entityName = entity$title,
        entityDescription = entity$descriptions[["abstract"]],
        coverage = dataset$coverage,
        attributeList = EML::set_attributes(attributes = attributeList),
        numberOfRecords = nrow(entity$data$features)
      )
    }
  }
  
  #wrapper
  eml_md <- EML::eml$eml(
    packageId = uuid::UUIDgenerate(),
    system = "uuid",
    dataset = dataset,
    lang = entity$language
  )
  
  #eml validation
  eml_valid <- EML::eml_validate(eml_md)
  if(eml_valid){
    config$logger.info("EML metadata produced is valid!")
  }else{
    eml_errors <- attr(eml_valid, "errors")
    config$logger.warn(sprintf("EML metadata has %s validation issues:", length(eml_errors)))
    for(eml_error in eml_errors){
      config$logger.warn(eml_error)
    }
  }
  
  #eml writing
  EML::write_eml(eml_md, file.path(getwd(), "metadata", paste0(entity$identifiers[["id"]], "_EML.xml")))
  
}