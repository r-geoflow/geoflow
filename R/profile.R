.onLoad <- function (libname, pkgname) { # nocov start
  
  assign(".geoflow", new.env(), envir= asNamespace(pkgname))
  
  #default line separator
  set_line_separator()
  
  #embedded handlers
  register_contact_handlers()
  register_entity_handlers()
  register_dictionary_handlers()
  
  #software
  register_software()
  
  #access endpoints
  register_data_accessors()
  
  #registers
  register_registers()
  
  #embedded actions
  register_actions()
  
  #vocabularies
  register_vocabularies()
  
} # nocov end
