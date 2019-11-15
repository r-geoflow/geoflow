geometa_create_iso_19110 <- function(entity, config, options){
  
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
  if(length(entity$data$dimensions)>0){
    ofv_scope <- "ogc_dimensions"
  }
  fc$addScope(paste0("openfairviewer:", ofv_scope))
  
  #producer
  #--------------------------------------------------------------------------
  main_entity <- NULL
  owners <- entity$contacts[sapply(entity$contacts, function(x){x$role == "owner"})]
  if(length(owners)==0) main_entity <- entity$contacts[[1]] else main_entity <- owners[[1]]
  producer <- ISOResponsibleParty$new()
  producer$setIndividualName(paste(main_entity$firstName, main_entity$lastName))
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
  fc$setProducer(producer)
  
  #citation
  #--------------------------------------------------------------------------
  ct <- ISOCitation$new()
  ct$setTitle(entity$title)
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
  
  add_all_attrs <- TRUE
  if(length(entity$data$attributes)>0 | length(entity$data$variables)>0) add_all_attrs <- FALSE
  
  for(featureAttrName in colnames(features)){
    
    fat_attr_register <- NULL
    
    #create attribute
    fat <- ISOFeatureAttribute$new()
    #name
    memberName <- featureAttrName
    fat_attrs <- entity$data$attributes[unlist(entity$data$attributes) == featureAttrName]
    if(length(fat_attrs)>0){
      fat_attr_desc <- attr(fat_attrs[[1]], "description")
      fat_attr_uri <- attr(fat_attrs[[1]], "uri")
      if(!is.null(fat_attr_uri)){
        registers <- config$registers
        registers <- registers[sapply(registers, function(x){x$id == fat_attr_uri})]
        if(length(registers)==0){
          warnMsg <- sprintf("Unknown register '%s'. Ignored for creating feature catalogue", fat_attr_uri)
          config$logger.warn(warnMsg)
        }else{
          fat_attr_register <- registers[[1]]
        }
      }
      if(!is.null(fat_attr_desc)) memberName <- fat_attr_desc
    }
    fat_vars <- entity$data$variables[unlist(entity$data$variables) == featureAttrName]
    if(length(fat_vars)>0){
      fat_var_desc <- attr(fat_vars[[1]], "description")
      if(!is.null(fat_var_desc)) memberName <- fat_var_desc
    }
    fat$setMemberName(memberName)
    #type (definition) -> attribute or variable
    fat_def <- "attribute" #default definition
    if(featureAttrName %in% entity$data$variables) fat_def <- "variable"
    fat$setDefinition(fat_def)
    fat$setCardinality(lower=1,upper=1)
    #code
    fat$setCode(featureAttrName)
    
    #add listed values
    featureAttrValues <- features[,featureAttrName][[1]]
    addValues <- TRUE
    if(is(featureAttrValues, "sfc")){
      addValues <- FALSE
    }else{
      if(!add_all_attrs & !(featureAttrName %in% entity$data$attributes)) addValues <- FALSE
      if(featureAttrName %in% entity$data$variables) addValues <- FALSE
    }
    if(addValues){
      featureAttrValues <- unique(featureAttrValues)
      for(featureAttrValue in featureAttrValues){
        if(!is.na(featureAttrValue)){
          val <- ISOListedValue$new()
          if(!is(featureAttrValue, "character")) featureAttrValue <- as(featureAttrValue, "character")
          val$setCode(featureAttrValue)
          if(!is.null(fat_attr_register)){
            reg_item <- fat_attr_register$data[fat_attr_register$data$code == featureAttrValue,]
            if(nrow(reg_item)>0){
              val$setLabel(reg_item[1L,"label"])
              val$setDefinition(reg_item[1L, "definition"])
            }else{
              val$setLabel(featureAttrValue)
              val$setDefinition(featureAttrValue)
            }
          }else{
            val$setLabel(featureAttrValue)
            val$setDefinition(featureAttrValue) 
          }
          fat$listedValue <- c(fat$listedValue, val)
        }
      }
    }
      
    #add type
    fat_type <- switch(class(featureAttrValues)[1],
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
    fat$setValueType(fat_type)
    
    #add feature attribute as carrierOfCharacteristic
    ft$carrierOfCharacteristics <- c(ft$carrierOfCharacteristics, fat)
  }
  #add featureType to catalogue
  fc$addFeatureType(ft)
  
  #we save the metadata
  saveRDS(fc, file.path(getwd(), "metadata", paste0(fcIdentifier, ".rds")))
  fc$save(file.path(getwd(), "metadata", paste0(entity$identifiers[["id"]], "_ISO-19110.xml")))
  
  return(fc)
}