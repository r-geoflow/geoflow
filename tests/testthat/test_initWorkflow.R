# test_initWorkflow.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for initWorkflow.R
#=======================

test_that("geoflow loads properly CRAN packages",{
  #TODO
})

test_that("geoflow loads properly GitHub packages",{
  #TODO
})

test_that("gsheet handler is working properly",{
  #try to init workflow
  cfg_file = system.file("extdata/config_example_gsheets.json", package = "geoflow")
  CFG <- initWorkflow(cfg_file)
  #check
  expect_is(CFG$metadata$content, "list")
  expect_equal(length(CFG$metadata$content), 2L)
  expect_equal(names(CFG$metadat$content), c("contacts", "entities"))
  expect_equal(length(CFG$metadata$content$contacts), 3L)
  expect_equal(length(CFG$metadata$content$entities), 1L)
})

test_that("ISO 19115 metadata produced out of gsheets",{
  #try to init workflow
  cfg_file = system.file("extdata/config_example_gsheets_action1.json", package = "geoflow")
  CFG <- initWorkflow(cfg_file)
  #check
  expect_is(CFG$metadata$content, "list")
  expect_equal(length(CFG$metadata$content), 2L)
  expect_equal(names(CFG$metadat$content), c("contacts", "entities"))
  expect_equal(length(CFG$metadata$content$contacts), 3L)
  expect_equal(length(CFG$metadata$content$entities), 1L)
})