#handle_dictionary_df
handle_dictionary_df <- function(handler, source, config){
  
  source [source == ""] <- NA
  
  if(!is(source, "data.frame")){
    errMsg <- "Error in 'handle_dictionary_df': source parameter should be an object of class 'data.frame'"
    config$logger$ERROR(errMsg)
    stop(errMsg)
  }
  
  dict <- geoflow::geoflow_dictionary$new()
  dict$setSource(source)
  config$logger$INFO("Parsing %s dictionary elements from tabular source", nrow(source))
  if(!"FeatureType" %in% colnames(source)){
    errMsg <- "Error in 'handle_dictionary_df': missing 'featuretype' column"
    config$logger$ERROR(errMsg)
    stop(errMsg)
  }
  
  #feature types
  fts <- unique(source$FeatureType)
  config$logger$INFO("Loading %s feature types from data dictionnary...",length(fts))
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
      
      #memberName
      src_membername <- geoflow::sanitize_str(ftm$MemberName)
      memberName <- src_membername
      if(!is.na(src_membername)){
        if(!startsWith(src_membername, "name:")) src_membername <- paste0("name:", src_membername)
      }
      membernames <- if(!is.na(src_membername)) geoflow::extract_cell_components(src_membername) else list()
      if(length(membernames)>0){
        kvps <- geoflow::extract_kvps(membernames, collapse=",")
        memberName <- kvps[[1]]$values
      }
      
      #definition
      src_memberdef <- geoflow::sanitize_str(ftm$Definition)
      memberDef <- src_memberdef
      if(!is.na(src_memberdef)){
        if(!startsWith(src_memberdef, "def:")) src_memberdef <- paste0("def:", src_memberdef)
      }
      memberdefs <- if(!is.na(src_memberdef)) geoflow::extract_cell_components(src_memberdef) else list()
      if(length(memberdefs)>0){
        kvps <- geoflow::extract_kvps(memberdefs, collapse=",")
        memberDef <- kvps[[1]]$values
      }
      
      member <- geoflow::geoflow_featuremember$new(
        type = ftm$MemberType,
        code = ftm$MemberCode,
        name = memberName, #i18n support
        def = memberDef, #i18n support
        defSource = defSource,
        registerId = ftm$RegisterId,
        registerScript = ftm$RegisterScript
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
  config$logger$INFO("Loading register scripts from data dictionnary...")
  scripts <- unique(source$RegisterScript)
  scripts <- scripts[!is.na(scripts)]
  
  invisible(lapply(as.character(scripts), function(script){
    isSourceUrl <- regexpr('(http|https)[^([:blank:]|\\\'|<|&|#\n\r)]+', script) > 0
    if(!isSourceUrl){
      if(!geoflow::is_absolute_path(script)){
        script<-file.path(config$session_wd,script)
      }
    }
    source(script)
  }))
  
  config$logger$INFO("Fetching registers from data dictionnary...")
  handlers <- unique(source$RegisterId)
  handlers <- handlers[!is.na(handlers)]
  for(handler in handlers){
    
    fun <- eval(parse(text = handler))
    if(is(fun,"try-error")){
      errMsg <- sprintf("Error while trying to evaluate function '%s", handler)
      config$logger$ERROR(errMsg)
      stop(errMsg)
    }
    if(!is(fun,"function")){
      errMsg <- sprintf("'%s' is not a function!", handler)
      config$logger$ERROR(errMsg)
      stop(errMsg)
    }
    register_to_fetch <- geoflow::geoflow_register$new(
      id = handler, 
      def = "", 
      fun = fun
    )
    config$logger$INFO("Fetching data for register '%s'...", handler)
    register_to_fetch$fetch(config)
    dict$addRegister(register_to_fetch)
  }
  
  return(dict)
}
