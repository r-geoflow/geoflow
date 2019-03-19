.onLoad <- function (libname, pkgname) { # nocov start
  
  assign(".geoflow", new.env(), envir= asNamespace(pkgname))
  
  #embedded handlers
  register_contact_handlers()
  register_entity_handlers()
  
  #embedded actions
  register_actions()
  
} # nocov end