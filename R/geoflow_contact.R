#' geoflow_contact
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' 
#' @name geoflow_contact
#' @title Geoflow contact class
#' @description This class models a contact to be executed by geoflow
#' @keywords contact
#' @return Object of \code{\link{R6Class}} for modelling a contact
#' @format \code{\link{R6Class}} object.
#' 
#' @field id contact identifier
#' @field firstName contact first name
#' @field lastName contact lastname
#' @field organizationName contact organization
#' @field positionName contact position
#' @field role contact role
#' @field voice contact phone number
#' @field facsimile contact facsimile
#' @field email contact email
#' @field websiteUrl contact website URL
#' @field websiteName contact website name
#' @field postalAddress contact postal address
#' @field postalCode contact postal code
#' @field city contact city
#' @field country contact country
#' @field identifiers contact identifiers
#' 
#' @examples
#' \dontrun{
#'   contact <- geoflow_contact$new()
#'   contact$setId("john.doe@@nowhere.org")
#'   contact$setFirstName("John")
#'   contact$setLastName("Doe")
#'   contact$setOrganizationName("Nowhere")
#'   contact$setPositionName("Wizard")
#'   contact$setRole("Manager")
#'   contact$setVoice("+9999000000000")
#'   contact$setFacsimile("+9999000000001")
#'   contact$setEmail("john.doe@@nowhere.org")
#'   contact$setWebsiteUrl("www.nowhere.org")
#'   contact$setWebsiteName("Nowhere Inc.")
#'   contact$setPostalAddress("Nowhere street")
#'   contact$setPostalCode("Nowhere code")
#'   contact$setCity("Nowhere city")
#'   contact$setCountry("Nowhere country")
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate a geoflow_contact object
#'  }
#'  \item{\code{setId(id)}}{
#'    Set a contact id, as object of class \code{character}
#'  }
#'  \item{\code{setFirstName(firstName)}}{
#'    Set the contact first name
#'  }
#'  \item{\code{setLastName(lastName)}}{
#'    Set the contact last name
#'  }
#'  \item{\code{setOrganizationName(organizationName)}}{
#'    Set the organization name
#'  }
#'  \item{\code{setPositionName(positionName)}}{
#'    Set the position name
#'  }
#'  \item{\code{setRole(role)}}{
#'    Set the contact role
#'  }
#'  \item{\code{setVoice(voice)}}{
#'    Set voice (phone number), as object of class \code{character}
#'  }
#'  \item{\code{setFacsimile}}{
#'    Set facsimile, as object of class \code{character}
#'  }
#'  \item{\code{setEmail(email)}}{
#'    Set email
#'  }
#'  \item{\code{setWebsiteUrl(websiteUrl)}}{
#'    Set website URL
#'  }
#'  \item{\code{setWebsiteName(websiteName)}}{
#'    Set website name
#'  }
#'  \item{\code{setPostalAddress(postalAddress)}}{
#'    Set postal address
#'  }
#'  \item{\code{setPostalCode(postalCode)}}{
#'    Set postal code
#'  }
#'  \item{\code{setCity(city)}}{
#'    Set city
#'  }
#'  \item{\code{setCountry(country)}}{
#'    Set country
#'  }
#'  \item{\code{addIdentifier(identifier)}}{
#'    Adds an identifier as object of class \code{geoflow_kvp}
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
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
    initialize = function(){},
    
    #setIdentifier
    setIdentifier = function(key = "id", id){
      self$identifiers[[key]] <- id
    },
    
    #setId
    setId = function(id){
      self$setIdentifier(key = "id", id)
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
    }
   
  )                                  
)