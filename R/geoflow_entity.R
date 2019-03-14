#'geoflow_entity
#'@export
geoflow_entity <- R6Class("geoflow_entity",
  public = list(
    identifiers = list(),
    date = NULL,
    language = "eng",
    title = NULL,
    descriptions = list(),
    subjects = list(),
    contacts = list(),
    relations = list(),
    rights = list(),
    spatial_extent = NULL,
    srid = NULL,
    temporal_extent = NULL,
    initialize = function(){},
    
    #setIdentifier
    setIdentifier = function(key, id){
      self$identifiers[[key]] <- id
    },
    
    #setDate
    setDate = function(date){
      self$date <- date
    },
    
    #setLanguage
    setLanguage = function(language){
      self$language <- language
    },
    
    #setTitle
    setTitle = function(title){
      self$title <- title
    },
    
    #setDescription
    setDescription = function(key, id){
      self$descriptions[[key]] <- id
    },
    
    #addSubject
    addSubject = function(subject){
      if(!is(subject, "geoflow_subject")){
        stop("The argument should be an object of class 'geoflow_subject'")
      }
      self$subjects <- c(self$subjects, subject)
    },
    
    #addContact
    addContact = function(contact){
      if(!is(contact, "geoflow_contact")){
        stop("The argument should be an object of class 'geoflow_contact'")
      }
      self$contacts <- c(self$contacts, contact)
    },
    
    #addRelation
    addRelation = function(relation){
      if(!is(relation, "geoflow_relation")){
        stop("The argument should be an object of class 'geoflow_relation'")
      }
      self$relations <- c(self$relations, relation)
    },
    
    #addRight
    addRight = function(right){
      if(!is(right, "geoflow_right")){
        stop("The argument should be an object of class 'geoflow_right'")
      }
      self$rights <- c(self$rights, right)
    },
    
    #setSpatialExtent
    setSpatialExtent = function(wkt, crs = NA){
      spatial_extent <- sf::st_as_sfc(wkt, crs = crs)
      if(class(spatial_extent)[1]=="try-error"){
        stop("The spatial extent is invalid!")
      }
      self$spatial_extent <- spatial_extent
    },
    
    #setSrid
    setSrid = function(srid){
      self$srid <- srid
    },
    
    #setTemporalExtent
    setTemporalExtent = function(str){
      isInstant <- FALSE
      strs <- unlist(strsplit(str,"/"))
      if(length(strs)==1) isInstant <- TRUE
      if(isInstant){
        self$temporal_extent <- list(instant = str_to_posix(strs))
      }else{
        self$temporal_extent <- list(
          start = str_to_posix(strs[1]),
          end = str_to_posix(strs[2])
        )
      }
    }
    
  )
)

