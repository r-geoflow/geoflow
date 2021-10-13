# test_config_metadata_gsheets_iso19115_inspire_geonetwork.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Integration tests for config_metadata_gsheets_iso19115_inspire_geonetwork.json workflow
#=======================
require(geoflow, quietly = TRUE)
require(testthat)

cfg_file = system.file("extdata/workflows/config_metadata_gsheets_iso19115_inspire_geonetwork.json", package = "geoflow")

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
  expect_equal(length(CFG$actions), 2L)
  expect_equal(length(CFG$software), 2L)
  expect_equal(names(CFG$software), c("input", "output"))
  expect_equal(length(CFG$software$input), 0L)
  expect_equal(length(CFG$software$output), 2L)
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
  EXEC <- geoflow::executeWorkflow(cfg_file, dir = ".")
  expect_true(dir.exists(EXEC))
  expect_true(file.exists(file.path(EXEC, "job.json")))
  expect_true(file.exists(file.path(EXEC, "job-logs.txt")))
  expect_true(file.exists(file.path(EXEC, "config_copyof_contacts_1.csv")))
  expect_true(file.exists(file.path(EXEC, "config_copyof_entities_1.csv")))
  expect_true(dir.exists(file.path(EXEC, "entities")))
  
  config <- geoflow::initWorkflow(cfg_file)
  geonetwork <- config$software$output$geonetwork
  
  entity_dirs <- list.dirs(path = file.path(EXEC, "entities"), full.names = F,recursive = F)
  expect_equal(entity_dirs, c("my-geoflow-record1", "my-geoflow-record2"))
  for(entity_dir in entity_dirs){
    entity_md <- geonetwork$getMetadataByUUID(entity_dir)
    expect_is(entity_md, "ISOMetadata")
  }
})
