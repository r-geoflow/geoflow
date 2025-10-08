# test_config_metadata_gsheets_zenodo.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Integration tests for config_metadata_gsheets_zenodo.json workflow
#=======================
require(geoflow, quietly = TRUE)
require(testthat)

#init
test_that("init",{
  testthat::skip_on_cran()
  cfg_file = system.file("extdata/workflows/config_metadata_gsheets_zenodo.json", package = "geoflow")
  CFG <- geoflow::initWorkflow(cfg_file, dir = ".")
  expect_is(CFG$metadata$content, "list")
  expect_equal(length(CFG$metadata$content), 2L)
  expect_equal(names(CFG$metadata$content), c("contacts", "entities"))
  expect_equal(length(CFG$metadata$content$contacts), 4L)
  expect_equal(length(CFG$getContacts()), 4L)
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
  testthat::skip_on_cran()
  cfg_file = system.file("extdata/workflows/config_metadata_gsheets_zenodo.json", package = "geoflow")
  DEBUG <- geoflow::debugWorkflow(cfg_file, entityIndex = 1, dir = ".")
  expect_equal(names(DEBUG), c("config", "entity", "dir"))
  expect_is(DEBUG$config, "list")
  expect_is(DEBUG$entity, "geoflow_entity")
  closeWorkflow(DEBUG$config)
})

#execute
test_that("execute",{
  testthat::skip_on_cran()
  cfg_file = system.file("extdata/workflows/config_metadata_gsheets_zenodo.json", package = "geoflow")
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
  print(zenodo_records)
  expect_equal(zenodo_records$Identifier, c("my-geoflow-zenodo-deposit1", "my-geoflow-zenodo-deposit2"))
  expect_equal(zenodo_records$Status, c("draft", "draft"))
  for(id in zenodo_records$Identifier){
    conceptdoi <- zenodo_records[zenodo_records$Identifier == id, "DOI_for_allversions"]
    rec <- zenodo$getDepositionByConceptDOI(conceptdoi)
    expect_is(rec, "ZenodoRecord")
  }
})
