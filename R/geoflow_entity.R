#'geoflow_entity
#'@export
geoflow_entity <- R6Class("geoflow_entity",
  public = list(
    id = NULL,
    title = NULL,
    abstract = NULL,
    subjects = list(),
    contacts = list(),
    initialize = function(){},
    
    #setId
    setId = function(id){
      self$id <- id
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
    }
    
    
  )
)

