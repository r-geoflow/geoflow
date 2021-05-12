create_metadata_Rmd <- function(entity, config, options){
  
  if(!require("rmarkdown")){
    stop("This action requires the 'rmarkdown' package")
  }
  
  config$logger.info('Generate Rmd')
  #options
  template <- if(!is.null(options$template)) options$template else "generic"
  output_format <- if(!is.null(options$output_format)) options$output_format else "html"

  infoMsg <- sprintf("Rmd template use :'%s'", template)
  config$logger.info(infoMsg)
  
  file_ext <- unlist(strsplit(template, "\\.(?=[^\\.]+$)", perl=TRUE))[2]
  if(!is.na(file_ext) && file_ext=='Rmd'){
    template_name<-basename(unlist(strsplit(template, "\\.(?=[^\\.]+$)", perl=TRUE))[1])
    template_file<-template
  }else{
    switch(template,
      "generic" = {template_file<-system.file("extdata/markdown", "generic.Rmd", package="geoflow")
                   template_name<-"generic"}
                  
    )
  }
  infoMsg <- sprintf("Rmd Localisation of template :'%s'", template_file)
  file<-file.path(getwd(), "markdown", paste(entity$getEntityJobDirname(),paste0(template_name,".",output_format),sep="_"))
  rmarkdown::render(template_file,output_file = file, params = list(config=config,entity=entity))
}