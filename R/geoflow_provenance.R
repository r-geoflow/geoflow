#'geoflow_provenance
#'@export
geoflow_provenance <- R6Class("geoflow_provenance",
   list(
     statement = NULL,
     processes = list(),
     initialize = function(str = NULL){
       if(!is.null(str)){
         data_props <-  unlist(strsplit(sanitize_str(str), ";"))
         data_props <- lapply(data_props, function(data_prop){
           return(extract_kvp(data_prop))
         })
         #statement
         names(data_props) <- sapply(data_props, function(x){x$key})
         if(!any(sapply(data_props, function(x){x$key=="statement"}))){
           stop("The data 'statement' is mandatory")
         }
         self$setStatement(paste(data_props$statement$values,collapse=","))
         #processes
         processes <- data_props[sapply(data_props, function(x){x$key=="process"})]
         processors <- data_props[sapply(data_props, function(x){x$key=="processor"})]
         if(length(processors)!=length(processes)) stop("Number of processors doesn't match the number of process steps")
         if(length(processes)>0 & length(processors)>0 & length(processes)==length(processors)){
           processes <- processes[[1]]$values
           processors <- sapply(processors[[1]]$values, function(val){paste0(val,"@",attr(val,"uri"))})
           for(i in 1:length(processes)){
             process <- processes[[i]]
             process_obj <- geoflow_process$new()
             process_des <- attr(process, "description")
             process_obj$setDescription(process_des)
             attr(process, "description") <- NULL
             process_obj$setRationale(process)
             processor_obj <- geoflow_contact$new()
             processor_obj$setId(processors[i])
             processor_obj$setRole("processor")
             process_obj$setProcessor(processor_obj)
             self$addProcess(process_obj)
           }
         }
         
       }
     },
     
     #setStatement
     setStatement = function(statement){
       self$statement <- statement
     },
     
     #addProcess
     addProcess = function(process){
       if(!is(process, "geoflow_process")){
         stop("The argument should be an object of class 'geoflow_process'")
       }
       self$processes <- c(self$processes, process)
     }
     
   )                                  
)