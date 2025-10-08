function(action, entity, config){
  
  if(!requireNamespace("rmarkdown")){
    stop("The action 'create-metadata-rmd' requires the 'rmarkdown' package")
  }
  
  config$logger$INFO('Generate Rmd')
  #options
  template <- action$getOption("template")
  output_format <- action$getOption("html")
  
  infoMsg <- sprintf("Rmd template use :'%s'", template)
  config$logger$INFO(infoMsg)
  
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
