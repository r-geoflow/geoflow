.onLoad <- function (libname, pkgname) { # nocov start
  
  message("Setting .geoflow object at line 3...")
  print("Setting .geoflow object at line 4...")
  #out <- try(assign(".geoflow", new.env(), envir= asNamespace(pkgname)))
  #packageStartupMessage(out)
  
  #default line separator
  packageStartupMessage("Setting line separator...")
  set_line_separator()
  
  #defaut temp directory
  packageStartupMessage("Setting temp directory...")
  set_temp_directory()
  
  #embedded handlers
  packageStartupMessage("Loading 'contact' handlers...")
  register_contact_handlers()
  packageStartupMessage("Loading 'entity' handlers...")
  register_entity_handlers()
  packageStartupMessage("Loading 'dictionary' handlers...")
  register_dictionary_handlers()
  
  #software
  packageStartupMessage("Loading software...")
  register_software()
  
  #registers
  packageStartupMessage("Loading registers...")
  register_registers()
  
  #embedded actions
  packageStartupMessage("Loading actions...")
  register_actions()
  
} # nocov end
