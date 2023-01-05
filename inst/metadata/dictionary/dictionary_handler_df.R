#handle_dictionary_df
handle_dictionary_df <- function(config, source){
  
  source [source == ""] <- NA
  
  if(!is(source, "data.frame")){
    errMsg <- "Error in 'handle_dictionary_df': source parameter should be an object of class 'data.frame'"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  dict <- geoflow::geoflow_dictionary$new()
  dict$setSource(source)
  config$logger.info(sprintf("Parsing %s dictionary elements from tabular source", nrow(source)))
  if(!"FeatureType" %in% colnames(source)){
    errMsg <- "Error in 'handle_dictionary_df': missing 'featuretype' column"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #feature types
  fts <- unique(source$FeatureType)
  config$logger.info(sprintf("Loading %s feature types from data dictionnary...",length(fts)))
  for(ft in fts){
    ft_df <- source[source$FeatureType == ft, ]
    featuretype <- geoflow::geoflow_featuretype$new(id = ft)
    rowNb <- nrow(ft_df)
    for(i in 1:rowNb){
      ftm <- ft_df[i,]
      defSource <- ftm$DefinitionSource
      if(!is.na(defSource)){
        defSource <- geoflow::extract_kvp(paste0("str:",defSource))$values[[1]]
      }
      member <- geoflow::geoflow_featuremember$new(
        type = ftm$MemberType,
        code = ftm$MemberCode,
        name = ftm$MemberName,
        def = ftm$Definition,
        defSource = defSource,
        registerId = ftm$RegisterId
      )
      if(!is.null(ftm$MinOccurs)) member$minOccurs <- ftm$MinOccurs
      if(!is.null(ftm$MaxOccurs)) member$maxOccurs <- ftm$MaxOccurs
      uom <- ftm$MeasurementUnit
      if(!is.na(uom)){
        member$uom <- geoflow::extract_kvp(paste0("str:", uom))$values[[1]]
      }
      featuretype$addMember(member)
    }
    dict$addFeatureType(featuretype)
  }
  
  #registers
  config$logger.info("Loading register scripts from data dictionnary...")
  scripts <- unique(source$RegisterScript)
  scripts <- scripts[!is.na(scripts)]
  
  invisible(lapply(as.character(scripts), function(script){
    isSourceUrl <- regexpr('(http|https)[^([:blank:]|\\\'|<|&|#\n\r)]+', script) > 0
    if(!isSourceUrl){
      if(!is_absolute_path(script)){
        script<-file.path(config$session_wd,script)
      }
    }
    source(script)
  }))
  
  config$logger.info("Fetching registers from data dictionnary...")
  handlers <- unique(source$RegisterId)
  handlers <- handlers[!is.na(handlers)]
  for(handler in handlers){
    
    fun <- eval(parse(text = handler))
    if(is(fun,"try-error")){
      errMsg <- sprintf("Error while trying to evaluate function '%s", handler)
      config$logger.error(errMsg)
      stop(errMsg)
    }
    if(!is(fun,"function")){
      errMsg <- sprintf("'%s' is not a function!", handler)
      config$logger.error(errMsg)
      stop(errMsg)
    }
    register_to_fetch <- geoflow::geoflow_register$new(
      id = handler, 
      def = "", 
      fun = fun
    )
    config$logger.info(sprintf("Fetching data for register '%s'...", handler))
    register_to_fetch$fetch(config)
    dict$addRegister(register_to_fetch)
  }
  
  return(dict)
}