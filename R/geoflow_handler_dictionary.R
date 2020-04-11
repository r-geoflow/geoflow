#handle_dictionary_df
handle_dictionary_df <- function(config, source){
  
  if(!is(source, "data.frame")){
    errMsg <- "Error in 'handle_dictionary_df': source parameter should be an object of class 'data.frame'"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  dict <- geoflow_dictionary$new()
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
    featuretype <- geoflow_featuretype$new(id = ft)
    rowNb <- nrow(ft_df)
    for(i in 1:rowNb){
      ftm <- ft_df[i,]
      defSource <- ftm$DefinitionSource
      if(!is.na(defSource)){
        defSource <- extract_kvp(paste0("str:",defSource))$values[[1]]
      }
      member <- geoflow_featuremember$new(
        type = ftm$MemberType,
        code = ftm$MemberCode,
        name = ftm$MemberName,
        def = ftm$Definition,
        defSource = defSource,
        registerId = ftm$RegisterId
      )
      if(!is.null(ftm$MinOccurs)) member$minOccurs <- ftm$MinOccurs
      if(!is.null(ftm$MaxOccurs)) member$maxOccurs <- ftm$MaxOccurs
      featuretype$addMember(member)
    }
    dict$addFeatureType(featuretype)
  }
  
  #registers
  config$logger.info("Loading register scripts from data dictionnary...")
  scripts <- unique(source$RegisterScript)
  scripts <- scripts[!is.na(scripts)]
  invisible(lapply(scripts, source))
  config$logger.info("Fetching registers from data dictionnary...")
  handlers <- unique(source$RegisterId)
  handlers <- handlers[!is.na(handlers)]
  for(handler in handlers){
    
    fun <- eval(parse(text = handler))
    if(class(fun)=="try-error"){
      errMsg <- sprintf("Error while trying to evaluate function '%s", handler)
      config$logger.error(errMsg)
      stop(errMsg)
    }
    if(class(fun)!="function"){
      errMsg <- sprintf("'%s' is not a function!", handler)
      config$logger.error(errMsg)
      stop(errMsg)
    }
    register_to_fetch <- geoflow_register$new(
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

#handle_dictionary_gsheet
handle_dictionary_gsheet <- function(config, source){
  
  #read gsheet URL
  source <- as.data.frame(gsheet::gsheet2tbl(source))
  
  #apply generic handler
  dictionary <- handle_dictionary_df(config, source)
  return(dictionary)
}

#handle_dictionary_csv
handle_dictionary_csv <- function(config, source){
  
  #read csv TODO -> options management: sep, encoding etc
  source <- read.csv(source)
  
  #apply generic handler
  dictionary <- handle_dictionary_df(config, source)
  return(dictionary)
}

#handle_dictionary_excel
handle_dictionary_excel <- function(config, source){
  
  #read excel TODO -> options management: sep, encoding etc
  source <- as.data.frame(readxl::read_excel(source))
  
  #apply generic handler
  dictionary <- handle_dictionary_df(config, source)
  return(dictionary)
}

#handle_dictionary_dbi
handle_dictionary_dbi <- function(config, source){
  dbi <- config$software$input$dbi
  if(is.null(dbi)){
    stop("There is no database input software configured to handle dictionary from DB")
  }
  
  #db source
  is_query <- startsWith(tolower(source), "select ")
  if(is_query){
    source <- try(DBI::dbGetQuery(dbi, source))
    if(class(source)=="try-error"){
      errMsg <- sprintf("Error while trying to execute DB query '%s'.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }else{
    source <- try(DBI::dbGetQuery(dbi, sprintf("select * from %s", source)))
    if(class(source)=="try-error"){
      errMsg <- sprintf("Error while trying to read DB table/view '%s'. Check if it exists in DB.", source)
      config$logger.error(errMsg)
      stop(errMsg)
    }
  }
  
  #apply generic handler
  dictionary <- handle_dictionarys_df(config, source)
  return(dictionary)
}
