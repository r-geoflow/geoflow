.onLoad <- function (libname, pkgname) { # nocov start
  
  assign(".geoflow", new.env(), envir= asNamespace(pkgname))
  
  #embedded actions
  register_geoflow_actions()
  
} # nocov end