create_metadata_Rmd <- function(entity, config, options){
  
  if(!require("rmarkdown")){
    stop("This action requires the 'rmarkdown' package")
  }
  
  config$logger.info('Generate Rmd')
  #options
  template <- if(!is.null(options$template)) options$template else "generic"

  infoMsg <- sprintf("Rmd template use :'%s'", template)
  config$logger.info(infoMsg)
  
  switch(template,
    "generic" = template_file<-system.file("extdata/markdown", "generic.Rmd", package="geoflow")
  )
  infoMsg <- sprintf("Rmd Localisation of template :'%s'", template_file)
  file<-file.path(getwd(), "markdown", paste(entity$getEntityJobDirname(),template,"metadata.html",sep="_"))
  rmarkdown::render(template_file,output_file = file, params = list(config=config,entity=entity))
}