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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
geoflow_contact <- R6Class("geoflow_contact",
  private = list(
     #TODO manage these allowed key values in class definitions (eg. geoflow_format)
     allowedKeyValuesFor = list(
       identifiers = c("id", "orcid")
     ) 
  ),
  public = list(
    #' @field id contact identifier
    id = NULL,
    #' @field firstName contact first name
    firstName = NULL,
    #' @field lastName contact lastname
    lastName = NULL,
    #' @field organizationName contact organization
    organizationName = NULL,
    #' @field positionName contact position
    positionName = NULL,
    #' @field role contact role
    role = NULL,
    #' @field voice contact phone number
    voice = NULL,
    #' @field facsimile contact facsimile
    facsimile = NULL,
    #' @field email contact email
    email = NULL,
    #' @field websiteUrl contact website URL
    websiteUrl = NULL,
    #' @field websiteName contact website name
    websiteName = NULL,
    #' @field postalAddress contact postal address
    postalAddress = NULL,
    #' @field postalCode contact postal code
    postalCode = NULL,
    #' @field city contact city
    city = NULL,
    #' @field country contact country
    country = NULL,
    #' @field identifiers contact identifiers
    identifiers = list(),
 
    #'@description Initializes a \link{geoflow_contact} object
    initialize = function(){},
    
    #'@description Retrieves keys allowed for a given tabular field name. eg. "Identifier"
    #'@param field field name
    #'@return the list of valid keys for the field considered
    getAllowedKeyValuesFor = function(field){
      clazz <- eval(parse(text = paste0("geoflow_validator_contact_",field)))
      clazz_obj <- clazz$new(0,0,"")
      return(clazz_obj$getValidKeys())
    },
    
    #'@description Sets an identifier by means of key
    #'@param key an identifier key. Default is "id"
    #'@param id the identifier
    setIdentifier = function(key = "id", id){
      self$identifiers[[key]] <- id
    },
    
    #'@description Sets an "id" identifier
    #'@param id the identifier
    setId = function(id){
      self$setIdentifier(key = "id", id)
    },
    
    #'@description Sets contact first name
    #'@param firstName contact first name
    setFirstName = function(firstName){
      self$firstName <- firstName
    },
    
    #'@description Sets contact last name
    #'@param lastName contact last name
    setLastName = function(lastName){
      self$lastName <- lastName
    },
    
    #'@description Sets contact organization name
    #'@param organizationName contact organization name
    setOrganizationName = function(organizationName){
      self$organizationName <- organizationName
    },
    
    #'@description Sets contact position name
    #'@param positionName contact position name
    setPositionName = function(positionName){
      self$positionName <- positionName
    },
    
    #'@description Sets contact role
    #'@param role the contact role
    setRole = function(role){
      self$role <- role
    },
    
    ##'@description Sets contact voice (phone number)
    ##'@param voice contact voice (phone number)
    setVoice = function(voice){
      self$voice <- voice
    },
    
    #'@description Sets contact facsimile
    #'@param facsimile contact facsimile
    setFacsimile = function(facsimile){
      self$facsimile <- facsimile
    },
    
    #'@description Sets contact email
    #'@param email contact email
    setEmail = function(email){
      self$email <- email
    },
    
    #'@description Sets contact website URL
    #'@param websiteUrl contact website URL
    setWebsiteUrl = function(websiteUrl){
      self$websiteUrl <- websiteUrl
    },
    
    #'@description Sets contact website name
    #'@param websiteName contact website name
    setWebsiteName = function(websiteName){
      self$websiteName <- websiteName
    },
    
    ##'@description Sets the contact postal address
    ##'@param postalAddress contact postal address
    setPostalAddress = function(postalAddress){
      self$postalAddress <- postalAddress
    },
    
    #'@description Sets the contact postal code
    #'@param postalCode contact postalCode
    setPostalCode = function(postalCode){
      self$postalCode <- postalCode
    },
    
    #'@description Sets the contact city
    #'@param city contact city
    setCity = function(city){
      self$city <- city
    },
    
    #'@description Sets the contact country
    #'@param country contact country
    setCountry = function(country){
      self$country <- country
    },
    
    
    #'@description Methods to export the \link{geoflow_contact} as \code{data.frame} using key-based syntax.
    #'@param line_separator a line separator. By default, the default line separator will be used.
    #'@return an object of class \code{data.frame} giving the entities using key-based syntax
    asDataFrame = function(line_separator = NULL){
      if(is.null(line_separator)) line_separator <- get_line_separator()
      out <- data.frame(
        #Identifier
        Identifier = paste0(sapply(names(self$identifiers),function(name){
          outid <- paste(name, self$identifiers[[name]],sep=":")
          return(outid)
        }),collapse=line_separator),
        #Email
        Email = if(!is.null(self$email)) self$email else "",
        #OrganizationName
        OrganizationName = if(!is.null(self$organizationName)) self$organizationName else "",
        #PositionName
        PositionName = if(!is.null(self$positionName)) self$positionName else "",
        #LastName
        LastName = if(!is.null(self$lastName)) self$lastName else "",
        #FirstName
        FirstName = if(!is.null(self$firstName)) self$firstName else "",
        #PostalAddress
        PostalAddress = if(!is.null(self$postalAddress)) self$postalAddress else "",
        #PostalCode
        PostalCode = if(!is.null(self$postalCode)) self$postalCode else "",
        #City
        City = if(!is.null(self$city)) self$city else "",
        #Country
        Country = if(!is.null(self$country)) self$country else "",
        #Voice
        Voice = if(!is.null(self$voice)) self$voice else "",
        #Facsimile
        Facsimile = if(!is.null(self$facsimile)) self$facsimile else "",
        #WebsiteUrl
        WebsiteUrl = if(!is.null(self$websiteUrl)) self$websiteUrl else "",
        #WebsiteName
        WebsiteName = if(!is.null(self$websiteName)) self$websiteName else "",
        stringsAsFactors = FALSE 
      )
      return(out)
    }
   
  )                                  
)