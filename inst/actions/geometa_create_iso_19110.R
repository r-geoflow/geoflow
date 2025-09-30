function(action, entity, config){
  
  if(!requireNamespace("geometa", quietly = TRUE)){
    stop("The 'geometa-create-iso-19110' action requires the 'geometa' package")
  }
  
  ISOMetadataNamespace$GML$uri <- "http://www.opengis.net/gml/3.2"
  
  skipEnrichWithData = if(!is.null(config$profile$options[["skipEnrichWithData"]])) config$profile$options[["skipEnrichWithData"]] else FALSE
  
  #manage multiple sources (supposes a common data structure to expose as ISO 19110)
  data_objects <- list()
  if(is.null(entity$data$dir)){
    data_objects <- list(entity$data)
  }else{
    data_objects <- entity$data$getData()
  }
  
  #features if any
  build_catalog_from_features = TRUE
  features = do.call("rbind", lapply(data_objects, function(data_object){data_object$features}))
  if(is.null(features)){
    if(!skipEnrichWithData){
      warnMsg <- sprintf("No data features associated to entity '%s' and global option 'skipEnrichWithData' is false. Skip feature catalogue creation", entity$identifiers[["id"]])
      config$logger.warn(warnMsg)
      return(FALSE)
    }else{
      fto <- entity$data$featureTypeObj
      if(!is.null(fto)){
        infoMsg <- "Global option 'skipEnrichWithData' is true. Feature catalogue will be created based on the dictionary only"
        config$logger.info(infoMsg)
        build_catalog_from_features = FALSE
      }else{
        warnMsg <- "Global option 'skipEnrichWithData' is true, but no dictionary available. Skip feature catalogue creation"
        config$logger.warn(warnMsg)
        return(FALSE)
      }
    }
  }
  
  #options
  doi <- action$getOption("doi")
  exclude_attributes <- action$getOption("exclude_attributes")
  exclude_attributes_not_in_dictionary <- action$getOption("exclude_attributes_not_in_dictionary")
  exclude_values_for_attributes <- action$getOption("exclude_values_for_attributes")
  extra_attributes <- action$getOption("extra_attributes")
  default_min_occurs <- action$getOption("default_min_occurs")
  default_max_occurs <- action$getOption("default_max_occurs")
  
  #feature catalogue creation
  #-----------------------------------------------------------------------------------------------------
  fcIdentifier <- paste0(entity$identifiers[["id"]],"_dsd")
  #produce feature catalogue
  fc <- ISOFeatureCatalogue$new(uuid = fcIdentifier)
  fc_title_locales <- geoflow::get_locales_from(entity$titles[["title"]])
  if(!is.null(fc_title_locales)){
    fc_title_locale_names <- names(fc_title_locales)
    fc_title_locales <- lapply(fc_title_locales, function(x){paste0(x, " - Feature Catalogue")})
    names(fc_title_locales) <- fc_title_locale_names
  }
  fc$setName(paste0(entity$titles[["title"]], " - Feature Catalogue"), locales = fc_title_locales)
  fc$addFieldOfApplication("FAIR")
  versionDate <- as.POSIXct(Sys.time())
  versionNumber <- format(versionDate, "%Y%m%dT%H%M%S")
  fc$setVersionNumber(versionNumber)
  fc$setVersionDate(versionDate)
  fc$setFunctionalLanguage(entity$language)
  
  #locales (i18n/i10n support)
  if(length(entity$locales)>0){
    ref_locales = utils::read.csv(system.file("extdata/codelists", "ISO-639-2_utf-8.txt", package = "geometa"),sep="|", stringsAsFactors = FALSE)
    for(locale in entity$locales){
      a_locale <- ISOLocale$new()
      a_locale$setId(locale)
      language = ref_locales[ref_locales$alpha2 == tolower(locale),]$alpha3[1]
      a_locale$setLanguage(language)
      a_locale$setCharacterSet("utf8")
      fc$addLocale(a_locale)
    }
  }
  
  #add scopes
  #--------------------------------------------------------------------------
  #-> geoflow scope
  fc$addScope(paste0("geoflow:", data_objects[[1]]$uploadType))
  #-> ofv (openfairviewer) scope
  ofv_scope <- "ogc_filters"
  if(length(entity$data$parameters)>0){
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
  ror = main_entity$identifiers[["ror"]]
  if(!is.null(ror)){
    producer$parentAttrs[["xlink:href"]] <- paste0("https://ror.org/", ror)
  }
  
  fc$setProducer(producer)
  
  #citation
  #--------------------------------------------------------------------------
  ct <- ISOCitation$new()
  ct$setTitle(entity$titles[["title"]])
  if("alternative" %in% names(entity$titles)){
    ct$addAlternateTitle(entity$titles[["alternative"]])
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
  #create featureType
  ft <- ISOFeatureType$new()
  ft$setTypeName(entity$identifiers$id)
  ft$setDefinition(entity$title)
  ft$setCode(entity$identifiers$id)
  ft$setIsAbstract(FALSE)
  
  columns <- if(build_catalog_from_features){
    #from data features
    c(colnames(features), unlist(extra_attributes))
  }else{
    #from dictionary
    fto <- entity$data$featureTypeObj
    sapply(fto$getMembers(), function(x){x$id})
  }
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
    featureAttrValues <- fat_attr_register$data$code
    if(build_catalog_from_features) if(featureAttrName %in% colnames(features)){
      featureAttrValues <- switch(class(features)[1],
                                  "sf" = features[,featureAttrName][[1]],
                                  "data.frame" = features[,featureAttrName]
      )
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
    if(!is.null(featureAttrValues) & addValues){
      config$logger.info(sprintf("Listing values for feature Attribute '%s'...", featureAttrName)) 
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
    }else{
      config$logger.warn(sprintf("Skip listing values for feature Attribute '%s'...", featureAttrName))
    }
    
    #add primitive type + data type (attribute or variable) as valueType
    fat_type <- if(build_catalog_from_features & !is.null(featureAttrValues[1])){
      switch(class(featureAttrValues[1])[1],
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
    }else{
      type = if(!is.null(fto)) fto$getMemberById(featureAttrName)$type else "attribute"
      switch(type,
        "attribute" = "xsd:string",
        "variable" = "xsd:decimal",
        type
      )
    }
    config$logger.info(sprintf("Set primitive type '%s' for feature Attribute '%s'...", fat_type, featureAttrName))
    fat_generic_type <- if(build_catalog_from_features){
      switch(class(featureAttrValues[1])[1],
       "integer" = "variable",
       "numeric" = "variable",
       "attribute"
      )
    }else{
      if(!is.null(fto)) fto$getMemberById(featureAttrName)$type else "attribute"
    }
    config$logger.info(sprintf("Feature member generic type for '%s': %s", featureAttrName, fat_generic_type))
    if(!is.null(fat_attr)) fat_generic_type <- fat_attr$type
    fat_type_anchor <- ISOAnchor$new(name = fat_type, href = fat_generic_type)
    fat$setValueType(fat_type_anchor)
    
    #add feature attribute as carrierOfCharacteristic
    config$logger.info(sprintf("Add carrier of characteristics for feature Attribute '%s'...", featureAttrName))
    ft$carrierOfCharacteristics <- c(ft$carrierOfCharacteristics, fat)
  }
  #add featureType to catalogue
  fc$addFeatureType(ft)
  
  #we save the metadata
  #saveRDS(fc, file.path(getwd(), "metadata", paste0(fcIdentifier, ".rds")))
  fc$save(file.path(getwd(), "metadata", paste0(entity$getEntityJobDirname(), "_ISO-19110.xml")))
  rm(fc)
}
