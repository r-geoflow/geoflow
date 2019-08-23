.onLoad <- function (libname, pkgname) { # nocov start
  
  assign(".geoflow", new.env(), envir= asNamespace(pkgname))
  
  #default line separator
  set_line_separator()
  
  #embedded handlers
  register_contact_handlers()
  register_entity_handlers()
  
  #software
  register_software()
  
  #embedded actions
  register_actions()
  
} # nocov end