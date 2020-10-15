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
    if(!is.na(contact$email)) person$electronicMailAddress <- contact$email
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
  dataset$pubDate <- as(Sys.Date(), "character")
  pub_dates <- entity$dates[sapply(entity$dates, function(x){x$key == "publication"})]
  if(length(pub_dates)>0) dataset$pubDate <- pub_dates[[1]]$value
  
  #language
  dataset$language <- entity$language
  
  #abstract
  if(!is.null(entity$descriptions$abstract)) dataset$abstract <- eml$abstract(
    para = entity$descriptions$abstract
  )
  
  #keywords
  if(length(entity$subjects)>0){
    dataset$keywordSet <- lapply(entity$subjects, function(subject){
      eml$keywordSet(
        keywordThesaurus = subject$name,
        keyword = lapply(subject$keywords, function(keyword){keyword$name})
      )
    })
  }
  
  #additionalInfo
  if(!is.null(entity$descriptions$info)) dataset$additionalInfo <- eml$additionalInfo(para = entity$descriptions$info)
  
  #intellectualRights
  rights <- entity$rights[sapply(entity$rights, function(x){x$key %in% c("useConstraint", "accessConstraint", "otherConstraint")})]
  if(length(rights)>0){
    dataset$intellectualRights <- eml$intellectualRights(
      para = paste(sapply(rights, function(right){right$value}), collapse = "\n")
    )
  }
  
  #licensed
  licenses <- entity$rights[sapply(entity$rights, function(x){x$key == "license"})]
  if(length(license)>0){
    dataset$licensed <- lapply(licenses, function(license){
      licensed = eml$licensed(identifier = license$value, licenseName = license$value)
      return(licensed)
    })
  }
  
  #distribution
  dataset$distribution = lapply(entity$relations, function(relation){
    eml_dist = list(
      online = list(
        url = list(
          url = relation$link,
          "function" = switch(relation$key,
            "thumbnail" = "information",
            "parent" = "information",
            "http" = "information",
            "wms" = "information",
            "wfs" = "download",
            "information"
          )
        )
      )
    )
  })
  
  #coverage
  #geo
  if(!is.null(entity$spatial_bbox)){
    if(is.null(dataset$coverage)) dataset$coverage <- list()
    dataset$coverage$geographicCoverage <- eml$geographicCoverage(
      geographicDescription = "Bounding box",
      boundingCoordinates = eml$boundingCoordinates(
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
    if("instant" %in% names(entity$temporal_extent)){
      dataset$coverage$temporalCoverage <- eml$temporalCoverage(
        singleDateTime = list(calendarDate = as.character(as.Date(entity$temporal_extent$instant)))
      )
    }else{
      dataset$coverage$temporalCoverage <- eml$temporalCoverage(
        rangeOfDates = list(
          beginDate = list(calendarDate = as.character(as.Date(entity$temporal_extent$start))),
          endDate = list(calendarDate = as.character(as.Date(entity$temporal_extent$end)))
        )
      )
    }
  }
  
  #taxonomic
  taxo <- entity$subjects[sapply(entity$subjects, function(x){x$name == taxonomySubject})]
  if(length(taxo)>0){
    taxo <- taxo[[1]]
    if(is.null(dataset$coverage)) dataset$coverage <- list()
    dataset$coverage$taxonomicCoverage = eml$taxonomicCoverage(
      taxonomicClassification = lapply(taxo$keywords, function(keyword){
        kwd_name = keyword$name
        kwd_parts = unlist(strsplit(kwd_name, " "))
        taxocl <- eml$taxonomicClassification(
          taxonRankName = "Genus",
          taxonRankValue = kwd_parts[1]
        )
        if(length(kwd_parts)>1){
          str <- paste(kwd_parts[2:length(kwd_parts)], collapse = " ")
          taxocl$taxonomicClassification = eml$taxonomicClassification(
            taxonRankName = "Species",
            taxonRankValue = str
          )
        }
        return(taxocl)
      })
    )
  }
  
  #purpose
  if(!is.null(entity$descriptions$purpose)) dataset$purpose <- eml$purpose(para = entity$descriptions$purpose)
  
  #contact
  pocs <- entity$getContacts()[sapply(entity$getContacts(), function(x){x$role=="pointOfContact"})]
  if(length(pocs)>0) dataset$contact <- contactsToEML(pocs)
  
  #publisher
  publishers <- entity$getContacts()[sapply(entity$getContacts(), function(x){x$role=="publisher"})]
  if(length(publishers)>0) dataset$publisher <- contactsToEML(publishers)
  
  #methods
  if(!is.null(entity$provenance)){
    if(length(entity$provenance$processes)>0){
      dataset$methods <- eml$methods(
        methodStep = lapply(entity$provenance$processes, function(process){
          eml$methodStep(description = paste0(process$rationale, ": ", process$description))
        })
      )
    }
  }
  
  #project
  project <- NULL
  if(!is.null(config$profile$project)) project <- config$profile$project
  if(!is.null(entity$descriptions$project)) project <- entity$descriptions$project
  
  #attributes
    
  #spatialVector
  if(!is.null(entity$data)) if(!is.null(entity$data$features)){
    features = entity$data$features
    attributeList = list()
    columns <- colnames(features)
    for(featureAttrName in columns){
      config$logger.info(sprintf("EML: adding '%s' to attributeList", featureAttrName))
      fat_attr_register <- NULL
      
      #create attribute
      #default name (from data)
      memberName <- featureAttrName
      
      #description
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
      
      #featureAttrValues
      featureAttrValues <- switch(class(features)[1],
                                  "sf" = features[,featureAttrName][[1]],
                                  "data.frame" = features[,featureAttrName])
      
      #is enumerated domain?
      enumerated <- TRUE
      if(is(featureAttrValues, "sfc")){
        enumerated <- FALSE
      }else{
        featureAttrValues <- unique(featureAttrValues)
        featureAttrValues <- featureAttrValues[!is.na(featureAttrValues)]
        featureAttrValues <- featureAttrValues[order(featureAttrValues)]
        if(is.null(fat_attr)){
          enumerated <- FALSE
        }else{
          if(fat_attr$type == "variable") enumerated <- FALSE
        }
        if(is.null(fat_attr_register)){
          if(!is(fat_attr_register$data, "data.frame")) enumerated <- FALSE
        }
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
      if(is.null(uom)) uom = "dimensionless"

      #measurementScale
      measurementScale <- switch(class(featureAttrValues[1])[1],
       "integer" = eml$measurementScale(ratio = eml$ratio(
         unit = eml$unit(standardUnit = uom), 
         numericDomain = eml$numericDomain(
           numberType = "integer"
         ))),
       "numeric" = eml$measurementScale(ratio = eml$ratio(
         unit = eml$unit(standardUnit = uom), 
         numericDomain = eml$numericDomain(
           numberType = "real"
         ))),
       "character" = eml$measurementScale(nominal = eml$nominal(
         nonNumericDomain = {
           if (enumerated){
            eml$nonNumericDomain(
              enumeratedDomain = eml$enumeratedDomain(
                codeDefinition = lapply(featureAttrValues, function(featureAttrValue){
                  eml$codeDefinition(
                    code = featureAttrValue,
                    definition = {
                      def = featureAttrValue
                      reg_item <- fat_attr_register$data[fat_attr_register$data$code == featureAttrValue,]
                      if(nrow(reg_item)>0){
                        def = reg_item[1L,"label"]
                      }
                      def
                    },
                    source = {
                      src = NULL
                      reg_item <- fat_attr_register$data[fat_attr_register$data$code == featureAttrValue,]
                      if(nrow(reg_item)>0){
                        src = reg_item[1L,"uri"]
                      }
                      src
                    }
                  )
                })
              )
            )
          }else{
            eml$nonNumericDomain(
              textDomain = eml$textDomain(
                definition = "Free text",
                pattern = "\\w"
              )
            )
          }
         }
       )),
       "logical" = eml$measurementScale(ordinal = eml$ordinal(
         nonNumericDomain = eml$nonNumericDomain(
           enumeratedDomain = eml$enumeratedDomain(
             codeDefinition = list(
               eml$codeDefinition(code = "TRUE", definition = "TRUE"),
               eml$codeDefinition(code = "FALSE", definition = "FALSE")
             )
           )
         )
       )),
       "Date" = eml$measurementScale(dateTime = eml$dateTime(formatString = "YYYY-MM-DDTHH:mm:ss")),
       "POSIXct" = eml$measurementScale(dateTime = eml$dateTime(formatString = "YYYY-MM-DD")),
       "sfc_POINT" = eml$measurementScale(interval = eml$interval(unit = eml$unit(standardUnit = uom), numericDomain = eml$numericDomain(numberType = "real"))),
       "sfc_MULTIPOINT" = eml$measurementScale(interval = eml$interval(unit = eml$unit(standardUnit = uom), numericDomain = eml$numericDomain(numberType = "real"))),
       "sfc_LINESTRING" = eml$measurementScale(interval = eml$interval(unit = eml$unit(standardUnit = uom), numericDomain = eml$numericDomain(numberType = "real"))),
       "sfc_MULTILINESTRING" = eml$measurementScale(interval = eml$interval(unit = eml$unit(standardUnit = uom), numericDomain = eml$numericDomain(numberType = "real"))),
       "sfc_POLYGON" = eml$measurementScale(interval = eml$interval(unit = eml$unit(standardUnit = uom), numericDomain = eml$numericDomain(numberType = "real"))),
       "sfc_MULTIPOLYGON" = eml$measurementScale(interval = eml$interval(unit = eml$unit(standardUnit = uom), numericDomain = eml$numericDomain(numberType = "real")))
      )
      
      attribute = eml$attribute(
        attributeName = featureAttrName,
        attributeLabel = memberName,
        attributeDefinition = ifelse(is.null(fat_attr_desc), "-", fat_attr_desc),
        measurementScale = measurementScale,
        missingValueCode = eml$missingValueCode(
          code = ifelse(enumerated, "", "NA"),
          codeExplanation = "-"
        ),
        storageType = eml$storageType(
          typeSystem = "http://www.w3.org/2001/XMLSchema-datatypes"
        )
      )
      attributeList[[length(attributeList)+1]] <- attribute
      
    }
      
    if(is(entity$data$features, "sf")){
      #use spatialVector
      config$logger.info("EML: spatial dataset - filling attributeList as 'spatialVector'")
      dataset$spatialVector = eml$spatialVector(
        alternateIdentifier = basename(entity$getJobDataResource(config, entity$data$source[[1]])),
        entityName = entity$title,
        entityDescription = entity$descriptions[["abstract"]],
        coverage = dataset$coverage,
        attributeList = eml$attributeList(attribute = attributeList),
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
          spref = NULL
          if(!is.null(entity$srid)){
            spref = paste0("EPSG:", entity$srid)
            if(entity$srid==4326) spref = "GCS_WGS_1984"
          }else{
            spref = as(sf::st_crs(entity$data$features), "character")[1]
          }
          spref
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