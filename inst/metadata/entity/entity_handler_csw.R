#handle_entities_csw
handle_entities_csw <- function(handler, source, config, handle = TRUE){
  
  if(!requireNamespace("ows4R", quietly = TRUE)){
    stop("The OGC CSW handler requires the 'ows4R' package")
  }
  if(!requireNamespace("sf", quietly = TRUE)){
    stop("The OGC CSW handler requires the 'sf' package")
  }
  if(!requireNamespace("geometa", quietly = TRUE)){
    stop("The OGC CSW handler requires the 'geometa' package")
  }
  
  CSW = config$software$input$csw
  CSW_CONFIG = config$software$input$csw_config
  if(is.null(CSW)){
    stop("There is no 'csw' input software configured to handle entities from an CSW service endpoint")
  }
  
  recs = if(is.null(source)){
    CSW$getRecords()
  }else{
    cons <- CSWConstraint$new(cqlText = source)
    query <- CSWQuery$new(constraint = cons)
    CSW$getRecords(query = query, outputSchema = "http://www.isotc211.org/2005/gmd")
  }
  
  createContactFromResponsibleParty = function(rp){
    contact = geoflow_contact$new()
    if(!is.null(rp$contactInfo$address$electronicMailAddress)) if(!is.na(rp$contactInfo$address$electronicMailAddress)) contact$identifiers[["id"]] = rp$contactInfo$address$electronicMailAddress
    if(!is.null(rp$organisationName)) if(!is.na(rp$organisationName)) contact$setOrganizationName(rp$organisationName)
    if(!is.null(rp$positionName)) if(!is.na(rp$positionName)) contact$setPositionName(rp$positionName)
    ind = rp$individualName
    if(!is.null(ind)) if(!is.na(ind)){
      ind_parts = unlist(strsplit(ind, " "))
      contact$setFirstName(ind_parts[1])
      contact$setLastName(ind_parts[2])
    }
    if(!is.null(rp$contactInfo$address$deliveryPoint)) if(!is.na(rp$contactInfo$address$deliveryPoint)) contact$setPostalAddress(rp$contactInfo$address$deliveryPoint)
    if(!is.null(rp$contactInfo$address$postalCode)) if(!is.na(rp$contactInfo$address$postalCode)) contact$setPostalCode(rp$contactInfo$address$postalCode)
    if(!is.null(rp$contactInfo$address$city)) if(!is.na(rp$contactInfo$address$city)) contact$setCity(rp$contactInfo$address$city)
    if(!is.null(rp$contactInfo$address$country)) if(!is.na(rp$contactInfo$address$country)) contact$setCountry(rp$contactInfo$address$country)
    if(!is.null(rp$contactInfo$address$electronicMailAddress)) if(!is.na(rp$contactInfo$address$electronicMailAddress)) contact$setEmail(rp$contactInfo$address$electronicMailAddress)
    if(!is.null(rp$contactInfo$phone$voice)) if(!is.na(rp$contactInfo$phone$voice)) contact$setVoice(rp$contactInfo$phone$voice)
    if(!is.null(rp$contactInfo$phone$facsimile)) if(!is.na(rp$contactInfo$phone$facsimile)) contact$setFacsimile(rp$contactInfo$phone$facsimile)
    if(!is.null(rp$contactInfo$onlineResource$name)) if(!is.na(rp$contactInfo$onlineResource$name)) contact$setWebsiteName(rp$contactInfo$onlineResource$name)
    if(is(rp$contactInfo$onlineResource$linkage, "ISOURL")){
      contact$setWebsiteUrl(rp$contactInfo$onlineResource$linkage$value)
    }
    contact$setRole(rp$role$attrs$codeListValue)
    return(contact$clone(deep = TRUE))
  }
  
  entities = lapply(recs, function(rec){
    entity = geoflow_entity$new()
    entity$setIdentifier("id", rec$fileIdentifier)
    print(sprintf("Processing metadata '%s' from CSW", rec$fileIdentifier))
    #type
    if(length(rec$hierarchyLevel)>0) entity$setType(key = "generic", type = rec$hierarchyLevel[[1]]$attrs$codeListValue)
    #language
    entity$setLanguage(if(is.list(rec$language)) rec$language$attrs$codeListValue else rec$language)
    #srid
    if(length(rec$referenceSystemInfo)>0){
      code = rec$referenceSystemInfo[[1]]$referenceSystemIdentifier$code
      code_parts = unlist(strsplit(code, "/"))
      code = code_parts[length(code_parts)]
      code_parts = unlist(strsplit(code, ":"))
      code = code_parts[length(code_parts)]
      if(code == "WGS 84") code = 4326
      entity$setSrid(suppressWarnings(as.integer(code)))
    }
    
    #parent identifier
    if(!is.null(rec$parentIdentifier)){
      parent_rel = geoflow_relation$new()
      parent_rel$setKey("parent")
      if(is(rec$parentIdentifier, "ISOAnchor")){
        parent_rel$setName(rec$parentIdentifier$value)
        parent_rel$setLink(rec$parentIdentifier$attrs[["xlink:href"]])
      }else{
        parent_rel$setName(rec$parentIdentifier)
      }
      entity$addRelation(parent_rel)
    }
    
    #add origin metadata URL (CSW GetRecordById)
    csw_record_url = paste0(
      CSW_CONFIG$parameters$url,
      "?service=CSW&request=GetRecordById&Version=",
      CSW_CONFIG$parameters$serviceVersion,
      "&elementSetName=full&outputSchema=http%3A//www.isotc211.org/2005/gmd&id=",
      rec$fileIdentifier
    )
    csw_record_rel = geoflow_relation$new()
    csw_record_rel$setKey("http")
    csw_record_rel$setName("Source ISO 19115 metadata (CSW GetRecordById)")
    csw_record_rel$setLink(csw_record_url)
    entity$addRelation(csw_record_rel)
    
    #creator
    #metadata contacts
    for(poc in rec$contact) if(length(poc)>1){
      entity$addContact(createContactFromResponsibleParty(poc))
    }
    
    #metadata date
    entity$addDate("metadata", rec$dateStamp)
    
    #identificationInfo metadata fields
    if(length(rec$identificationInfo)>0){
      
      #graphic overviews
      gos = rec$identificationInfo[[1]]$graphicOverview
      for(go in gos){
        thumbnail_rel = geoflow_relation$new()
        thumbnail_rel$setKey("thumbnail")
        thumbnail_rel$setName(go$fileDescription)
        thumbnail_rel$setLink(go$fileName)
        entity$addRelation(thumbnail_rel)
      }
      
      #cited responsible party
      rps = rec$identificationInfo[[1]]$citation$citedResponsibleParty
      for(rp in rps){
        entity$addContact(createContactFromResponsibleParty(rp))
      }
      
      #dates
      dates = rec$identificationInfo[[1]]$citation$date
      for(date in dates){
        if(date$dateType$attrs$codeListValue != "edition"){
          entity$addDate(date$dateType$attrs$codeListValue, date$date)
        }
      }
      editionDates = rec$identificationInfo[[1]]$citation$editionDate
      if(length(editionDates)>0) for(editionDate in editionDates){
        if(is(editionDate,"numeric")) editionDate = as.Date(editionDate, origin = "1970-01-01")
        entity$addDate("edition", editionDate)
      }
      
      #doi (in case available)
      hasDOI = sapply(rec$identificationInfo[[1]]$citation$identifier, function(identifier){
        has = !is.character(identifier$code) & !is.na(identifier$code)
        if(has) has = regexpr(pattern = "dx.doi.org", identifier$code$attrs[["xlink:href"]]) > 0
        return(has)
      })
      if(any(hasDOI)){
        doi_meta_id = rec$identificationInfo[[1]]$citation$identifier[hasDOI][[1]]
        doi = unlist(strsplit(doi_meta_id$code$attrs[["xlink:href"]], "dx.doi.org/"))[2]
        entity$setIdentifier("doi", doi)
      }
      #title
      entity$setTitle("title", rec$identificationInfo[[1]]$citation$title)
      altitles = rec$identificationInfo[[1]]$citation$alternateTitle
      if(length(altitles)>0) for(altitle in altitles) entity$setTitle("alternative", altitle)
      #description
      entity$setDescription("abstract", rec$identificationInfo[[1]]$abstract)
      entity$setDescription("purpose", rec$identificationInfo[[1]]$purpose)
      credits = rec$identificationInfo[[1]]$credit
      if(length(credits)>0) entity$setDescription("credit", credits[[1]])
      entity$setDescription("info", rec$identificationInfo[[1]]$supplementalInformation)
      entity$setDescription("edition", rec$identificationInfo[[1]]$citation$edition)
      status = rec$identificationInfo[[1]]$status
      if(length(status)>0) entity$setDescription("status", status[[1]]$attrs$codeListValue)
      #subject
      entity$subjects = lapply(rec$identificationInfo[[1]]$descriptiveKeywords, function(dk){
        subject = geoflow_subject$new()
        subject$setKey(dk$type$attrs$codeListValue)
        title = dk$thesaurusName$title
        if(!is.null(title)){
          if(is(title, "ISOAnchor")){
            subject$setName(title$value)
            subject$setUri(title$attrs[["xlink:href"]])
          }else{
            subject$setName(title)
          }
        }
        kwds = dk$keyword
        for(kwd in kwds){
          gkwd = kwd
          if(is(kwd, "ISOAnchor")){
            gkwd = kwd$value
            attr(gkwd, "uri") = kwd$attrs[["xlink:href"]]
          }
          subject$addKeyword(gkwd)
        }
        return(subject)
      })
      #extents
      if(length(rec$identificationInfo[[1]]$extent)>0){
        extent = rec$identificationInfo[[1]]$extent[[1]]
        #spatial coverage
        if(length(extent$geographicElement)>0) {
          geo_extent = extent$geographicElement[[1]]
          if(is(geo_extent, "ISOGeographicBoundingBox")){
            entity$geo_bbox = sf::st_bbox(c(
                xmin = geo_extent$westBoundLongitude, 
                xmax = geo_extent$eastBoundLongitude, 
                ymin = geo_extent$southBoundLatitude, 
                ymax = geo_extent$northBoundLatitude
              ), 
              crs = sf::st_crs(4326)
            )
          }
        }
        #temporal coverage
        if(length(extent$temporalElement)>0){
          time_extent = extent$temporalElement[[1]]
          if(is(time_extent, "ISOTemporalExtent")){
            if(is(time_extent$extent, "GMLTimeInstant")){
              entity$temporal_extent = list(instant = time_extent$extent$timePosition$value)
            }else if(is(time_extent$extent, "GMLTimePeriod")){
              entity$temporal_extent = list(
                start = time_extent$extent$beginPosition$value,
                end = time_extent$extent$endPosition$value
              )
            }
          }
        }
      }
      #rights
      constraints = rec$identificationInfo[[1]]$resourceConstraints
      if(length(constraints)>0){
        for(constraint in constraints){
          if(is(constraint, "ISOLegalConstraints")){
            #use constraints
            use_values = lapply(constraint$useConstraints, function(x){x$attrs$codeListValue})
            if(length(use_values)>0){
              for(use_value in use_values){
                use_right = geoflow_right$new()
                use_right$setKey("useConstraint")
                use_right$setValues(use_value)
                entity$addRight(use_right) 
              }
            }
            #access constraints
            access_values = lapply(constraint$accessConstraints, function(x){x$attrs$codeListValue})
            if(length(access_values)>0){
              for(access_value in access_values){
                access_right = geoflow_right$new()
                access_right$setKey("accessConstraint")
                access_right$setValues(access_value)
                entity$addRight(access_right)  
              }
            }
            #other constraints
            if(length(constraint$otherConstraints)>0){
              for(otherConstraint in constraint$otherConstraints){
                other_right = geoflow_right$new()
                other_right$setKey("otherConstraint")
                other_right$setValues(otherConstraint)
                entity$addRight(other_right) 
              }
            }
            #use limitations
            if(length(constraint$useLimitation)>0){
              for(useLimitation in constraint$useLimitation){
                use_right = geoflow_right$new()
                use_right$setKey("useLimitation")
                use_right$setValues(useLimitation)
                entity$addRight(use_right)
              }
            }
          }
        }
      }
      #resource formats
      resource_formats = rec$identificationInfo[[1]]$resourceFormat
      if(length(resource_formats)>0){
        for(resource_format in resource_formats){
          format = geoflow_format$new()
          format$setKey("resource")
          if(is(resource_format, "ISOFormat")){
            name = resource_format$name
            if(is(name, "ISOAnchor")){
              format$setUri(name$attrs[["xlink:href"]])
              name = name$value
            }
            format$setName(name)
            entity$addFormat(format)
          }
        }
      }
    }
    
    #distributionInfo metadata fields
    if(length(rec$distributionInfo)>0){
      #distributors
      distributors = rec$distributionInfo$distributor
      for(distributor in distributors){
        entity$addContact(createContactFromResponsibleParty(distributor$distributorContact))
      }
      #distribution formats
      distrib_formats = rec$distributionInfo$distributionFormat
      if(length(distrib_formats)>0){
        for(distrib_format in distrib_formats){
          format = geoflow_format$new()
          format$setKey("distribution")
          if(is(distrib_format, "ISOFormat")){
            name = distrib_format$name
            if(is(name, "ISOAnchor")){
              format$setUri(name$attrs[["xlink:href"]])
              name = name$value
            }
            format$setName(name)
            entity$addFormat(format)
          }
        }
      }
      
      #relations/online resources
      if(length(rec$distributionInfo$transferOptions)>0){
        online_resources = rec$distributionInfo$transferOptions[[1]]$onLine
        if(length(online_resources)>0) for(online_resource in online_resources){
          rel = geoflow_relation$new()
          key <- switch(online_resource$protocol,
                       "WWW:LINK-1.0-http--link" = "http",
                       "WWW:DOWNLOAD-1.0-http--download" = "download",
                       "OGC:WMS" = "wms", #defaut
                       "OGC:WMS-1.1.0-http-get-map" = "wms110",
                       "OGC:WMS-1.1.1-http-get-map" = "wms111",
                       "OGC:WMS-1.3.0-http-get-map" = "wms130",
                       "OGC:WFS" = "wfs",
                       "OGC:WFS-1.0.0-http-get-feature" = "wfs100",
                       "OGC:WFS-1.1.0-http-get-feature" = "wfs110",
                       "OGC:WFS-2.0.0-http-get-feature" = "wfs200",
                       "OGC:WCS" = "wcs",
                       "OGC:WCS-1.0.0-http-get-coverage" = "wcs100",
                       "OGC:WCS-1.1-http-get-coverage" = "wcs11",
                       "OGC:WCS-1.1.0-http-get-coverage" = "wcs110",
                       "OGC:WCS-1.1.1-http-get-coverage" = "wcs111",
                       "OGC:WCS-2.0.1-http-get-coverage" = "wcs201",
                       "OGC:WCS-2.1.0-http-get-coverage" = "wcs210", 
                       "http"
          )
          rel$setKey(key)
          if(is(online_resource$name, "ISOMimeFileType")){
            rel$setName(online_resource$name$value)
          }else{
            rel$setName(online_resource$name)
          }
          rel$setDescription(online_resource$description)
          rel$setLink(online_resource$linkage$value)
          entity$addRelation(rel)
        }
      }
    }
    
    #provenance
    if(length(rec$dataQualityInfo)>0){
      dq = rec$dataQualityInfo[[1]]
      if(is(dq$lineage, "ISOLineage")){
        prov = geoflow_provenance$new()
        prov$setStatement(dq$lineage$statement)
        steps = dq$lineage$processStep
        if(length(steps)>0) for(i in 1:length(steps)){
          step = steps[[i]]
          proc = geoflow_process$new()
          proc$setRationale(step$rationale)
          proc$setDescription(step$description)
          prov$addProcess(proc)
          #add processor to contacts
          for(processor in step$processor){
            processor_entity = createContactFromResponsibleParty(processor)
            processor_entity$role = paste0(processor_entity$role, i)
            entity$addContact(processor_entity)
          }
        }
        entity$setProvenance(prov)
      }
    }
    
    return(entity)
  })
  
  return(entities)
}