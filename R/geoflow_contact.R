#'geoflow_contact
#'@export
geoflow_contact <- R6Class("geoflow_contact",
  public = list(
    id = NULL,
    individualName = NULL,
    organizationName = NULL,
    positionName = NULL,
    role = NULL,
    voice = NULL,
    facsimile = NULL,
    email = NULL,
    website = NULL,
    postalAddress = NULL,
    postalCode = NULL,
    city = NULL,
    country = NULL,
    initialize = function(str = NULL){
      if(!is.null(str)){
        
      }
    },
    
    #setId
    setId = function(id){
      self$id <- id
    },
    
    #setIndividualName
    setIndividualName = function(individualName){
      self$individualName <- individualName
    },
    
    #setOrganizationName
    setOrganizationName = function(organizationName){
      self$organizationName <- organizationName
    },
    
    #setPositionName
    setPositionName = function(positionName){
      self$positionName <- positionName
    },
    
    #setRole
    setRole = function(role){
      self$role <- role
    },
    
    #setVoice
    setVoice = function(voice){
      self$voice <- voice
    },
    
    #setFacsimile
    setFacsimile = function(facsimile){
      self$facsimile <- facsimile
    },
    
    #setEmail
    setEmail = function(email){
      self$email <- email
    },
    
    #setWebsite
    setWebsite = function(website){
      self$website <- website
    },
    
    #setPostalAddress
    setPostalAddress = function(postalAddress){
      self$postalAddress <- postalAddress
    },
    
    #setPostalCode
    setPostalCode = function(postalCode){
      self$postalCode <- postalCode
    },
    
    #setCity
    setCity = function(city){
      self$city <- city
    },
    
    #setCountry
    setCountry = function(country){
      self$country <- country
    }
   
  )                                  
)