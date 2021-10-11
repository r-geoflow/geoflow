geometa_create_iso_19110 <- function(entity, config, options){
  
  if(!requireNamespace("geometa", quietly = TRUE)){
    stop("The 'geometa-create-iso-19110' action requires the 'geometa' package")
  }
  
  ISOMetadataNamespace$GML$uri <- "http://www.opengis.net/gml/3.2"
  
  #features if any
  features = entity$data$features
  if(is.null(features)){
    warnMsg <- sprintf("No data features associated to entity '%s'. Skip feature catalogue creation", entity$identifiers[["id"]])
    config$logger.warn(warnMsg)
    return(FALSE)
  }
  
  #options
  doi <- if(!is.null(options$doi)) options$doi else FALSE
  exclude_attributes <- if(!is.null(options$exclude_attributes)) options$exclude_attributes else list()
  exclude_attributes_not_in_dictionary <- if(!is.null(options$exclude_attributes_not_in_dictionary)) options$exclude_attributes_not_in_dictionary else FALSE
  exclude_values_for_attributes <- if(!is.null(options$exclude_values_for_attributes)) options$exclude_values_for_attributes else list()
  extra_attributes <- if(!is.null(options$extra_attributes)) options$extra_attributes else list()
  default_min_occurs <- if(!is.null(options$default_min_occurs)) options$default_min_occurs else 0L
  default_max_occurs <- if(!is.null(options$default_max_occurs)) options$default_max_occurs else Inf
  
  #feature catalogue creation
  #-----------------------------------------------------------------------------------------------------
  fcIdentifier <- paste0(entity$identifiers[["id"]],"_dsd")
  #produce feature catalogue
  fc <- ISOFeatureCatalogue$new(uuid = fcIdentifier)
  fc$setName(paste0(entity$title, " - Feature Catalogue"))
  fc$addFieldOfApplication("Open Science")
  fc$addFieldOfApplication("FAIR")
  versionDate <- as.POSIXct(Sys.time())
  versionNumber <- format(versionDate, "%Y%m%dT%H%M%S")
  fc$setVersionNumber(versionNumber)
  fc$setVersionDate(versionDate)
  fc$setFunctionalLanguage(entity$language)
  
  #add scopes
  #--------------------------------------------------------------------------
  #-> geoflow scope
  fc$addScope(paste0("geoflow:", entity$data$uploadType))
  #-> ofv (openfairviewer) scope
  ofv_scope <- "ogc_filters"
  if(entity$data$uploadType == "dbquery" & length(entity$data$parameters)>0){
    ofv_scope <- "ogc_viewparams"
  }
  if(length(entity$data$ogc_dimensions)>0){
    ofv_scope <- "ogc_dimensions"
  }
  fc$addScope(paste0("openfairviewer:", ofv_scope))
  
  #producer
  #--------------------------------------------------------------------------
  main_entity <- NULL
  owners <- entity$contacts[sapply(entity$contacts, function(x){x$role == "owner"})]
  if(length(owners)==0) main_entity <- entity$contacts[[1]] else main_entity <- owners[[1]]
  producer <- ISOResponsibleParty$new()
  if(!is.na(main_entity$firstName) && !is.na(main_entity$lastName)) producer$setIndividualName(paste(main_entity$firstName, main_entity$lastName))
  producer$setOrganisationName(main_entity$organizationName)
  producer$setPositionName(main_entity$positionName)
  producer$setRole(main_entity$role)
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice(main_entity$voice)
  phone$setFacsimile(main_entity$facsimile)
  contact$setPhone(phone)
  address <- ISOAddress$new()
  address$setDeliveryPoint(main_entity$postalAddress)
  address$setCity(main_entity$city)
  address$setPostalCode(main_entity$postalCode)
  address$setCountry(main_entity$country)
  address$setEmail(main_entity$email)
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage(main_entity$websiteUrl)
  res$setName(main_entity$websiteName)
  contact$setOnlineResource(res)
  producer$setContactInfo(contact) 
  
  orcid = main_entity$identifiers[["orcid"]]
  if(!is.null(orcid)){
    producer$parentAttrs[["xlink:href"]] <- paste0("https://orcid.org/", orcid)
  }
  
  fc$setProducer(producer)
  
  #citation
  #--------------------------------------------------------------------------
  ct <- ISOCitation$new()
  ct$setTitle(entity$titles[["title"]])
  if("alternative" %in% names(entity$titles)){
    ct$setAlternateTitle(entity$titles[["alternative"]])
  }
  d <- ISODate$new()
  d$setDate(Sys.Date())
  d$setDateType("publication")
  ct$addDate(d)
  ct$setEdition(versionNumber)
  editionDate <- if(!is.null(entity$date)) entity$date else Sys.Date()
  ct$setEditionDate(editionDate)
  
  #set metadata identifier
  ct$addIdentifier(ISOMetaIdentifier$new(code = entity$identifiers[["id"]]))
  #methodology to set DOI inspired by NOAA wiki
  #https://geo-ide.noaa.gov/wiki/index.php?title=DOI_Minting_Procedure#Third.2C_Include_the_DOI_and_citation_text_in_the_ISO_Metadata_Record
  the_doi <- entity$identifiers[["doi"]]
  if(is.null(the_doi)) the_doi <- entity$identifiers[["conceptdoi_to_save"]]
  if(is.null(the_doi)) the_doi <- entity$identifiers[["doi_to_save"]]
  if(!is.null(the_doi) & doi){
    mdIdentifier <- ISOAnchor$new(
      name = paste0("doi:", the_doi), 
      href = paste0("http://dx.doi.org/", the_doi)
    )
    mdIdentifier$setAttr("xlink:title", "DOI")
    mdIdentifier$setAttr("xlink:actuate", "onRequest")
    ct$addIdentifier(ISOMetaIdentifier$new(code = mdIdentifier))
  }
  ct$addPresentationForm("mapDigital")
  fc$addDefinitionSource(ct)
  
  
  #add feature type / attributes
  #--------------------------------------------------------------------------
  layername <- if(!is.null(entity$data$layername)) entity$data$layername else entity$identifiers$id
  #create featureType
  ft <- ISOFeatureType$new()
  ft$setTypeName(layername)
  ft$setDefinition(entity$title)
  ft$setCode(layername)
  ft$setIsAbstract(FALSE)
  
  columns <- c(colnames(features), unlist(extra_attributes))
  for(featureAttrName in columns){

    if(featureAttrName %in% exclude_attributes){
      config$logger.warn(sprintf("Feature Attribute '%s' is listed in 'exclude_attributes'. Discarding it...", featureAttrName)) 
      next
    }
      
    fat_attr_register <- NULL
    
    #create attribute
    fat <- ISOFeatureAttribute$new()
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
        if(length(registers)>0) registers <- registers[sapply(registers, function(x){x$id == registerId})]
        if(length(registers)==0){
          warnMsg <- sprintf("Unknown register '%s'. Ignored for creating feature catalogue", registerId)
          config$logger.warn(warnMsg)
        }else{
          fat_attr_register <- registers[[1]]
        }
      }
      if(!is.null(fat_attr_desc)) memberName <- fat_attr_desc
    }else{
      if(exclude_attributes_not_in_dictionary){
        config$logger.warn(sprintf("Feature Attribute '%s' not referenced in dictionary and 'exclude_attributes_not_in_dictionary' option is enabled. Discarding it...", featureAttrName)) 
        next
      }
    }
    fat$setMemberName(memberName)
    fat$setDefinition(fat_attr$def)
    #cardinality
    minOccurs <- default_min_occurs; if(!is.null(fat_attr)) minOccurs <- fat_attr$minOccurs
    maxOccurs <- default_max_occurs; if(!is.null(fat_attr)) maxOccurs <- fat_attr$maxOccurs
    if(is.null(minOccurs)) minOccurs <- default_min_occurs
    if(is.null(maxOccurs)) maxOccurs <- default_max_occurs
    if(maxOccurs == "Inf") maxOccurs <- Inf
    fat$setCardinality(lower = minOccurs, upper = maxOccurs)
    #code
    fat$setCode(featureAttrName)
    #uom
    uom <- fat_attr$uom
    if(!is.null(uom)){
      gmlUom <- GMLUnitDefinition$buildFrom(uom)
      if(is.null(gmlUom)){
        gmlUom <- GMLUnitDefinition$new()
        uomId <- uom
        uomUri <- attr(uom, "uri")
        if(is.null(uomUri)) uomUri <- ""
        gmlUom$setIdentifier(uom, uomUri)
        uomName <- attr(uom, "description")
        if(!is.null(uomName)) gmlUom$addName(uomName)
      }
      fat$setValueMeasurementUnit(gmlUom)
    }
    
    #add listed values
    if(featureAttrName %in% colnames(features)){
      featureAttrValues <- switch(class(features)[1],
          "sf" = features[,featureAttrName][[1]],
          "data.frame" = features[,featureAttrName]
      )
    }else{
      featureAttrValues <- fat_attr_register$data$code
    }
    
    addValues <- TRUE
    if(is(featureAttrValues, "sfc")){
      addValues <- FALSE
    }else if(featureAttrName %in% exclude_values_for_attributes){
      addValues <- FALSE
    }else{
      if(is.null(fat_attr)){
        addValues <- FALSE
      }else{
        if(fat_attr$type == "variable") addValues <- FALSE
      }
    }
    if(addValues){
      featureAttrValues <- unique(featureAttrValues)
      featureAttrValues <- featureAttrValues[order(featureAttrValues)]
      for(featureAttrValue in featureAttrValues){
        if(!is.na(featureAttrValue)){
          val <- ISOListedValue$new()
          if(!is(featureAttrValue, "character")) featureAttrValue <- as(featureAttrValue, "character")
          val$setCode(featureAttrValue)
          if(!is.null(fat_attr_register)){
            reg_item <- fat_attr_register$data[fat_attr_register$data$code == featureAttrValue,]
            if(nrow(reg_item)>0){
              if(!is.na(reg_item[1L, "uri"])){
                val$setCode(ISOAnchor$new(name = featureAttrValue, href = reg_item[1L, "uri"]))
              }
              val$setLabel(reg_item[1L,"label"])
              val$setDefinition(reg_item[1L, "definition"])
            }else{
              val$setLabel(NA)
            }
          }else{
            val$setLabel(NA)
          }
          fat$listedValue <- c(fat$listedValue, val)
        }
      }
    }
      
    #add primitive type + data type (attribute or variable) as valueType
    fat_type <- switch(class(featureAttrValues[1])[1],
      "integer" = "xsd:int",
      "numeric" = "xsd:decimal",
      "character" = "xsd:string",
      "logical" = "xsd:boolean",
      "Date" = "xsd:date",
      "POSIXct" = "xsd:datetime",
      "sfc_POINT" = "gml:PointPropertyType",
      "sfc_MULTIPOINT" = "gml:MultiPointPropertyType",
      "sfc_LINESTRING" = "gml:LineStringPropertyType",
      "sfc_MULTILINESTRING" = "gml:MultiLineStringPropertyType",
      "sfc_POLYGON" = "gml:PolygonPropertyType",
      "sfc_MULTIPOLYGON" = "gml:MultiPolygonPropertyType"
    )
    fat_generic_type <- switch(class(featureAttrValues[1])[1],
      "integer" = "variable",
      "numeric" = "variable",
      "attribute"
    )
    if(!is.null(fat_attr)) fat_generic_type <- fat_attr$type
    fat_type_anchor <- ISOAnchor$new(name = fat_type, href = fat_generic_type)
    fat$setValueType(fat_type_anchor)
    
    #add feature attribute as carrierOfCharacteristic
    ft$carrierOfCharacteristics <- c(ft$carrierOfCharacteristics, fat)
  }
  #add featureType to catalogue
  fc$addFeatureType(ft)
  
  #we save the metadata
  #saveRDS(fc, file.path(getwd(), "metadata", paste0(fcIdentifier, ".rds")))
  fc$save(file.path(getwd(), "metadata", paste0(entity$getEntityJobDirname(), "_ISO-19110.xml")))
  rm(fc)
}