# test_config_metadata_gsheets_geoserver_shp.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Integration tests for config_metadata_gsheets_geoserver_shp.json workflow
#=======================
require(geoflow, quietly = TRUE)
require(testthat)

cfg_file = system.file("extdata/workflows/config_metadata_gsheets_sdi_geoserver_shp.json", package = "geoflow")

#init
test_that("init",{
  testthat::skip_on_cran()
  CFG <- geoflow::initWorkflow(cfg_file)
  expect_is(CFG$metadata$content, "list")
  expect_equal(length(CFG$metadata$content), 2L)
  expect_equal(names(CFG$metadata$content), c("contacts", "entities"))
  expect_equal(length(CFG$metadata$content$contacts), 4L)
  expect_equal(length(CFG$getContacts()), 4L)
  expect_equal(length(CFG$metadata$content$entities), 1L)
  expect_equal(length(CFG$getEntities()), 1L)
  expect_equal(length(CFG$actions), 1L)
  expect_equal(length(CFG$software), 2L)
  expect_equal(names(CFG$software), c("input", "output"))
  expect_equal(length(CFG$software$input), 0L)
  expect_equal(length(CFG$software$output), 2L)
})

#debug
test_that("debug",{
  testthat::skip_on_cran()
  DEBUG <- geoflow::debugWorkflow(cfg_file, entityIndex = 1, dir = ".")
  expect_equal(names(DEBUG), c("config", "entity"))
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
  entity_dirs <- list.dirs(path = file.path(EXEC, "entities"), full.names = F,recursive = F)
  expect_true(all(entity_dirs %in% c("my-geoflow-shapefile-record1")))
  config <- geoflow::initWorkflow(cfg_file)
  GS <- config$software$output$geoserver
  expect_is(GS$getWorkspace("myshp"), "GSWorkspace")
  expect_is(GS$getDataStore("myshp", "myshpstore"), "GSShapefileDataStore")
  expect_is(GS$getFeatureType("myshp", "myshpstore", "shapefile1"), "GSFeatureType")
  expect_is(GS$getLayer("shapefile1"), "GSLayer")
})
