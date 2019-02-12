#'geoflow_entity
#'@export
geoflow_entity <- R6Class("geoflow_entity",
  public = list(
    title = NULL,
    abstract = NULL,
    contacts = list(),
    initialize = function(){},
    
    #setTitle
    setTitle = function(title){
      self$title <- title
    },
    
    #setAbstract
    setAbstract = function(abstract){
      self$abstract <- abstract
    },
    
    #addContact
    addContact = function(contact){
      if(!is(contact, "geoflow_entity_contact")){
        stop("The argument should be an object of class 'geoflow_entity_contact'")
      }
      self$contacts <- c(self$contacts, contact)
    }
  )
)

