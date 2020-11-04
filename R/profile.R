.onLoad <- function (libname, pkgname) { # nocov start
  
  message("Setting package .geoflow hidden property object")
  assign(".geoflow", new.env(), envir= asNamespace(pkgname))
  
  #default line separator
  message("Setting line separator...")
  set_line_separator()
  
  #defaut temp directory
  message("Setting temp directory...")
  set_temp_directory()
  
  #embedded handlers
  message("Loading 'contact' handlers...")
  register_contact_handlers()
  message("Loading 'entity' handlers...")
  register_entity_handlers()
  message("Loading 'dictionary' handlers...")
  register_dictionary_handlers()
  
  #software
  message("Loading software...")
  register_software()
  
  #registers
  message("Loading registers...")
  register_registers()
  
  #embedded actions
  message("Loading actions...")
  register_actions()
  
} # nocov end
