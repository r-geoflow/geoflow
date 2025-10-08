# test_config_metadata_gsheets_iso19115_inspire_geonetwork.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Integration tests for config_metadata_gsheets_iso19115_inspire_geonetwork.json workflow
#=======================
require(geoflow, quietly = TRUE)
require(testthat)

cfg_file = system.file("extdata/workflows/config_metadata_gsheets_iso19115_inspire_geonetwork_with_uuid.json", package = "geoflow")

if(T){
#init
test_that("init",{
  testthat::skip_on_cran()
  CFG <- geoflow::initWorkflow(cfg_file, dir = ".")
  expect_is(CFG$metadata$content, "list")
  expect_equal(length(CFG$metadata$content), 2L)
  expect_equal(names(CFG$metadata$content), c("contacts", "entities"))
  expect_equal(length(CFG$metadata$content$contacts), 4L)
  expect_equal(length(CFG$getContacts()), 4L)
  expect_equal(length(CFG$metadata$content$entities), 2L)
  expect_equal(length(CFG$getEntities()), 2L)
  expect_equal(length(CFG$actions), 2L)
  expect_equal(length(CFG$software), 2L)
  expect_equal(names(CFG$software), c("input", "output"))
  expect_equal(length(CFG$software$input), 0L)
  expect_equal(length(CFG$software$output), 4L)
})

#debug
test_that("debug",{
  testthat::skip_on_cran()
  DEBUG <- geoflow::debugWorkflow(cfg_file, entityIndex = 1, dir = ".")
  expect_equal(names(DEBUG), c("config", "entity", "dir"))
  expect_is(DEBUG$config, "list")
  expect_is(DEBUG$entity, "geoflow_entity")
})

#execute
test_that("execute",{
  testthat::skip_on_cran()
  EXEC <- geoflow::executeWorkflow(cfg_file, dir = ".")
  expect_true(dir.exists(EXEC))
  expect_true(file.exists(file.path(EXEC, "job.json")))
  expect_true(file.exists(file.path(EXEC, "job-logs.txt")))
  expect_true(file.exists(file.path(EXEC, "config_copyof_contacts_1.csv")))
  expect_true(file.exists(file.path(EXEC, "config_copyof_entities_1.csv")))
  expect_true(dir.exists(file.path(EXEC, "entities")))
  
  config <- geoflow::initWorkflow(cfg_file, dir = ".")
  geonetwork <- config$software$output$geonetwork
  
  entity_dirs <- list.dirs(path = file.path(EXEC, "entities"), full.names = F,recursive = F)
  expect_equal(entity_dirs, c("my-geoflow-record", "my-geoflow-record-i18n"))
  for(entity_dir in entity_dirs){
    entity_resource_dirs <- list.dirs(path = file.path(EXEC, "entities", entity_dir), full.names = F,recursive = F)
    expect_equal(entity_resource_dirs, c("data", "metadata"))
    expect_true(file.exists(file.path(EXEC, "entities", entity_dir, "metadata", paste0(entity_dir,"_ISO-19115.xml"))))
  }
})
}
