#'geoflow_handler
#'@export
geoflow_handler <- R6Class("geoflow_handler",
  public = list(
    id = NA,
    def = NA,
    fun = NA,
    script = NA,
    initialize = function(id, def = "", fun = NULL, script = NULL){
      self$id <- id
      self$def <- def
      self$fun <- fun
      self$script <- script
    }
  )
)

#'register_contact_handlers
#'@export
register_contact_handlers <- function(){
  handlers <- list(
    geoflow_handler$new(
      id = "csv",
      def = "Handle metadata contacts from a CSV file",
      fun = handle_contacts_csv
    ),
    geoflow_handler$new(
      id = "excel",
      def = "Handle metadata contacts from a Microsoft Excel (xls,xlsx) file",
      fun = handle_contacts_excel
    ),
    geoflow_handler$new(
      id = "gsheet",
      def = "Handle metadata contacts from a Google spreadsheet",
      fun = handle_contacts_gsheet
    ),
    geoflow_handler$new(
      id = "dbi",
      def = "Handle metadata contacts from a DB source",
      fun = handle_contacts_dbi
    )
  )
  .geoflow$contact_handlers <- handlers
}

#'list_contact_handlers
#'@export
list_contact_handlers <- function(raw = FALSE){
  handlers <- .geoflow$contact_handlers 
  if(raw){
    return(handlers)
  }else{
    handlers <- do.call("rbind", lapply(handlers, function(handler){
      return(data.frame(
        id = handler$id,
        definition = handler$def,
        stringsAsFactors = FALSE
      ))
    }))
  }
  return(handlers)
}


#'register_entity_handlers
#'@export
register_entity_handlers <- function(){
  handlers <- list(
    geoflow_handler$new(
      id = "csv",
      def = "Handle metadata entities from a CSV file",
      fun = handle_entities_csv
    ),
    geoflow_handler$new(
      id = "excel",
      def = "Handle metadata entities from a Microsoft Excel (xls,xlsx) file",
      fun = handle_entities_excel
    ),
    geoflow_handler$new(
      id = "gsheet",
      def = "Handle metadata entities from a Google spreadsheet",
      fun = handle_entities_gsheet
    ),
    geoflow_handler$new(
      id = "dbi",
      def = "Handle metadata entities from a DB source",
      fun = handle_entities_dbi
    )
  )
  .geoflow$entity_handlers <- handlers
}

#'list_entity_handlers
#'@export
list_entity_handlers <- function(raw = FALSE){
  handlers <- .geoflow$entity_handlers 
  if(raw){
    return(handlers)
  }else{
    handlers <- do.call("rbind", lapply(handlers, function(handler){
      return(data.frame(
        id = handler$id,
        definition = handler$def,
        stringsAsFactors = FALSE
      ))
    }))
  }
  return(handlers)
}

