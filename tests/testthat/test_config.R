# test_config.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Integration tests for json/yml configs
#=======================
require(geoflow, quietly = TRUE)
require(testthat)

#init
test_that("compare initWorkflow outputs - json vs. yml",{
  testthat::skip_on_cran()
  
  cfg_file_json = system.file("extdata/workflows/config_metadata_gsheets_iso19115.json", package = "geoflow")
  CFG1 <- geoflow::initWorkflow(cfg_file_json, dir = tempdir())
  
  cfg_file_yml = system.file("extdata/workflows/config_metadata_gsheets_iso19115.yml", package = "geoflow")
  CFG2 <- geoflow::initWorkflow(cfg_file_yml, dir = tempdir())
  
  testthat::expect_equal(length(waldo::compare(CFG1$profile, CFG2$profile)), 0L)
  testthat::expect_equal(length(waldo::compare(CFG1$metadata, CFG2$metadata)), 0L)
  testthat::expect_equal(length(waldo::compare(CFG1$software, CFG2$software)), 0L)
  testthat::expect_equal(length(waldo::compare(CFG1$actions, CFG2$actions)), 0L)
})
