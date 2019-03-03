#'geoflow_entity
#'@export
geoflow_entity <- R6Class("geoflow_entity",
  public = list(
    id = NULL,
    language = "eng",
    title = NULL,
    abstract = NULL,
    subjects = list(),
    contacts = list(),
    relations = list(),
    initialize = function(){},
    
    #setId
    setId = function(id){
      self$id <- id
    },
    
    #setLanguage
    setLanguage = function(language){
      self$language <- language
    },
    
    #setTitle
    setTitle = function(title){
      self$title <- title
    },
    
    #setAbstract
    setAbstract = function(abstract){
      self$abstract <- abstract
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
    }
    
  )
)

