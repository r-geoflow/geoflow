# test_config_metadata_gsheets.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for config_metadata_gsheets.json workflow
#=======================
require(geoflow, quietly = TRUE)
require(testthat)

cfg_file = system.file("extdata/workflows/config_metadata_gsheets.json", package = "geoflow")

#init
test_that("init",{
  CFG <- geoflow::initWorkflow(cfg_file)
  expect_is(CFG$metadata$content, "list")
  expect_equal(length(CFG$metadata$content), 2L)
  expect_equal(names(CFG$metadata$content), c("contacts", "entities"))
  expect_equal(length(CFG$metadata$content$contacts), 3L)
  expect_equal(length(CFG$metadata$content$entities), 2L)
  expect_equal(length(CFG$actions), 0L)
  expect_equal(length(CFG$software), 0L)
})

#debug
test_that("debug",{
  DEBUG <- geoflow::debugWorkflow(cfg_file)
  expect_equal(names(DEBUG), c("config", "entity"))
  expect_is(DEBUG$config, "list")
  expect_is(DEBUG$entity, "geoflow_entity")
})

#execute
test_that("execute",{
  EXEC <- geoflow::executeWorkflow(cfg_file)
  expect_true(dir.exists(EXEC))
})
