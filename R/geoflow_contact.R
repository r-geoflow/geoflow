#'geoflow_contact
#'@export
geoflow_contact <- R6Class("geoflow_contact",
  public = list(
    id = NULL,
    firstName = NULL,
    lastName = NULL,
    organizationName = NULL,
    positionName = NULL,
    role = NULL,
    voice = NULL,
    facsimile = NULL,
    email = NULL,
    websiteUrl = NULL,
    websiteName = NULL,
    postalAddress = NULL,
    postalCode = NULL,
    city = NULL,
    country = NULL,
    identifiers = list(),
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
      stop("The function 'setIndividualname' is deprecated, please use setFirstName/setLastName methods instead!")
    },
    
    #setFirstName
    setFirstName = function(firstName){
      self$firstName <- firstName
    },
    
    #setLastName
    setLastName = function(lastName){
      self$lastName <- lastName
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
    
    #setWebsiteUrl
    setWebsiteUrl = function(websiteUrl){
      self$websiteUrl <- websiteUrl
    },
    
    #setWebsiteName
    setWebsiteName = function(websiteName){
      self$websiteName <- websiteName
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
    },
    
    #addIdentifier
    addIdentifier = function(identifier){
      if(!is(identifier, "geoflow_kvp")){
        stop("The argument should be an object of class 'geoflow_kvp'")
      }
      self$identifiers = c(self$identifiers, identifier)
    }
   
  )                                  
)