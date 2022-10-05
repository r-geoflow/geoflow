# test_config_metadata_dataverse.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Integration tests for config_metadata_dataverse.json workflow
#=======================
require(geoflow, quietly = TRUE)
require(testthat)

#init
test_that("init",{
  testthat::skip_on_cran()
  cfg_file = system.file("extdata/workflows/config_metadata_dataverse.json", package = "geoflow")
  CFG <- geoflow::initWorkflow(cfg_file)
  expect_is(CFG$metadata$content, "list")
  expect_equal(length(CFG$metadata$content), 1L)
  expect_equal(names(CFG$metadata$content), "entities")
  expect_equal(length(CFG$metadata$content$entities), 11L)
  expect_equal(length(CFG$getEntities()), 11L)
  expect_equal(length(CFG$actions), 0L)
  expect_equal(length(CFG$software), 2L)
  expect_equal(names(CFG$software), c("input", "output"))
  expect_equal(length(CFG$software$input), 2L)
  expect_equal(length(CFG$software$output), 0L)
  expect_equal(names(CFG$software$input), c("dataverse", "dataverse_config"))
})

#debug
test_that("debug",{
  testthat::skip_on_cran()
  cfg_file = system.file("extdata/workflows/config_metadata_dataverse.json", package = "geoflow")
  DEBUG <- geoflow::debugWorkflow(cfg_file, entityIndex = 1, dir = ".")
  expect_equal(names(DEBUG), c("config", "entity"))
  expect_is(DEBUG$config, "list")
  expect_is(DEBUG$entity, "geoflow_entity")
})

#execute
test_that("execute",{
  testthat::skip_on_cran()
  cfg_file = system.file("extdata/workflows/config_metadata_dataverse.json", package = "geoflow")
  #execution
  EXEC <- geoflow::executeWorkflow(cfg_file, dir = ".")
  expect_true(dir.exists(EXEC))
  expect_true(file.exists(file.path(EXEC, "job.json")))
  expect_true(file.exists(file.path(EXEC, "job-logs.txt")))
})
