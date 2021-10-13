# test_config_metadata_gsheets_zenodo.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Integration tests for config_metadata_gsheets_zenodo.json workflow
#=======================
require(geoflow, quietly = TRUE)
require(testthat)

cfg_file = system.file("extdata/workflows/config_metadata_gsheets_zenodo.json", package = "geoflow")

#init
test_that("init",{
  CFG <- geoflow::initWorkflow(cfg_file)
  expect_is(CFG$metadata$content, "list")
  expect_equal(length(CFG$metadata$content), 2L)
  expect_equal(names(CFG$metadata$content), c("contacts", "entities"))
  expect_equal(length(CFG$metadata$content$contacts), 3L)
  expect_equal(length(CFG$getContacts()), 3L)
  expect_equal(length(CFG$metadata$content$entities), 2L)
  expect_equal(length(CFG$getEntities()), 2L)
  expect_equal(length(CFG$actions), 1L)
  expect_equal(length(CFG$software), 2L)
  expect_equal(names(CFG$software), c("input", "output"))
  expect_equal(length(CFG$software$input), 0L)
  expect_equal(length(CFG$software$output), 2L)
  expect_equal(names(CFG$software$output), c("zenodo", "zenodo_config"))
})

#debug
test_that("debug",{
  DEBUG <- geoflow::debugWorkflow(cfg_file, entityIndex = 1, dir = ".")
  expect_equal(names(DEBUG), c("config", "entity"))
  expect_is(DEBUG$config, "list")
  expect_is(DEBUG$entity, "geoflow_entity")
})

#execute
test_that("execute",{
  #deposit
  EXEC <- geoflow::executeWorkflow(cfg_file, dir = ".")
  expect_true(dir.exists(EXEC))
  expect_true(file.exists(file.path(EXEC, "job.json")))
  expect_true(file.exists(file.path(EXEC, "job-logs.txt")))
  expect_true(file.exists(file.path(EXEC, "config_copyof_contacts_1.csv")))
  expect_true(file.exists(file.path(EXEC, "config_copyof_entities_1.csv")))
  expect_true(dir.exists(file.path(EXEC, "entities")))
  expect_true(dir.exists(file.path(EXEC, "zenodo")))
  zenodo_files <- list.files(file.path(EXEC, "zenodo"))
  expect_equal(zenodo_files, c("zenodo_entities_1_with_pids_for_publication.csv",
                               "zenodo_geoflow_config_for_publication.json",
                               "zenodo_pids.csv"))
  
  config <- geoflow::initWorkflow(cfg_file, dir = ".")
  zenodo = config$software$output$zenodo
  
  zenodo_records <- as.data.frame(readr::read_csv(file.path(EXEC, "zenodo", "zenodo_pids.csv")))
  expect_equal(zenodo_records$Identifier, c("my-geoflow-zenodo-deposit1", "my-geoflow-zenodo-deposit2"))
  expect_equal(zenodo_records$Status, c("draft", "draft"))
  for(id in zenodo_records$Identifier){
    conceptdoi <- zenodo_records[zenodo_records$Identifier == id, "DOI_for_allversions"]
    rec <- zenodo$getDepositionByConceptDOI(conceptdoi)
    expect_is(rec, "ZenodoRecord")
  }
})

#execute - with publication & versioning
test_that("execute - with publication & versioning",{
  
  cfg_file_full = system.file("extdata/workflows/config_metadata_gsheets_zenodo_full.json", package = "geoflow")
  
  #deposit
  EXEC <- geoflow::executeWorkflow(cfg_file_full, dir = ".")
  expect_true(dir.exists(EXEC))
  expect_true(file.exists(file.path(EXEC, "job.json")))
  expect_true(file.exists(file.path(EXEC, "job-logs.txt")))
  expect_true(file.exists(file.path(EXEC, "config_copyof_contacts_1.csv")))
  expect_true(file.exists(file.path(EXEC, "config_copyof_entities_1.csv")))
  expect_true(dir.exists(file.path(EXEC, "entities")))
  expect_true(dir.exists(file.path(EXEC, "zenodo")))
  zenodo_files <- list.files(file.path(EXEC, "zenodo"))
  expect_equal(zenodo_files, c("zenodo_entities_1_with_pids_for_publication.csv",
                               "zenodo_geoflow_config_for_publication.json",
                               "zenodo_pids.csv"))
  
  config <- geoflow::initWorkflow(cfg_file_full, dir = ".")
  zenodo = config$software$output$zenodo
  
  zenodo_records <- as.data.frame(readr::read_csv(file.path(EXEC, "zenodo", "zenodo_pids.csv")))
  expect_equal(zenodo_records$Identifier, c("my-geoflow-zenodo-record1", "my-geoflow-zenodo-record2"))
  expect_equal(zenodo_records$Status, c("draft", "draft"))
  for(id in zenodo_records$Identifier){
    conceptdoi <- zenodo_records[zenodo_records$Identifier == id, "DOI_for_allversions"]
    rec <- zenodo$getDepositionByConceptDOI(conceptdoi)
    expect_is(rec, "ZenodoRecord")
  }
  
  #publication
  wd <- getwd()
  setwd(file.path(EXEC, "zenodo"))
   
  #modify files attached (publication of a new version requires new files each time)
  entities_filename <- "zenodo_entities_1_with_pids_for_publication.csv"
  src_entities <- as.data.frame(readr::read_csv(entities_filename))
  src_entities$Data <- sapply(1:nrow(src_entities), function(i){
    src_entity <- src_entities[i,]
    filename <- paste0(unlist(strsplit(geoflow::extract_cell_components(src_entity$Identifier)[1],"id:"))[2], "_", 
                       format(Sys.time(), "%Y%m%d%H%M%S"), ".csv")
    write.csv(src_entity, filename)
    outdata <- paste0("source:", filename, "@", tools::file_path_as_absolute(filename))
    return(outdata)
  })
  readr::write_csv(src_entities, entities_filename)
  
  #publication
  cfg_file_publish <- "zenodo_geoflow_config_for_publication.json"
  EXEC2 <- geoflow::executeWorkflow(cfg_file_publish, dir = ".")
  expect_true(dir.exists(EXEC2))
  setwd(wd)
  
  zenodo_published_records <- as.data.frame(readr::read_csv(file.path(EXEC2, "zenodo", "zenodo_pids.csv")))
  expect_equal(zenodo_published_records$Identifier, c("my-geoflow-zenodo-record1", "my-geoflow-zenodo-record2"))
  expect_equal(zenodo_published_records$Status, c("published", "published"))
  for(id in zenodo_published_records$Identifier){
    conceptdoi <- zenodo_published_records[zenodo_published_records$Identifier == id, "DOI_for_allversions"]
    rec <- zenodo$getRecordByConceptDOI(conceptdoi)
    expect_is(rec, "ZenodoRecord")
  }
})
