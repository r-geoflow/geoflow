# test_config_metadata_csw.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Integration tests for config_metadata_csw.json workflow
#=======================
require(geoflow, quietly = TRUE)
require(testthat)

cfg_file = system.file("extdata/workflows/config_metadata_csw.json", package = "geoflow")

if(T){
  #init
  test_that("init",{
    testthat::skip_on_cran()
    CFG <- geoflow::initWorkflow(cfg_file, dir = tempdir())
    expect_is(CFG$metadata$content, "list")
    expect_equal(length(CFG$metadata$content), 1L)
    expect_equal(names(CFG$metadata$content), c("entities"))
    expect_equal(length(CFG$metadata$content$entities), 10L)
    expect_equal(length(CFG$getEntities()), 10L)
    expect_equal(length(CFG$actions), 0L)
    expect_equal(length(CFG$software), 2L)
    expect_equal(names(CFG$software), c("input", "output"))
    expect_equal(length(CFG$software$input), 2L)
    expect_equal(length(CFG$software$output), 0L)
  })
  
  #debug
  test_that("debug",{
    testthat::skip_on_cran()
    DEBUG <- geoflow::debugWorkflow(cfg_file, entityIndex = 1, dir = tempdir())
    expect_equal(names(DEBUG), c("config", "entity", "dir"))
    expect_is(DEBUG$config, "list")
    expect_is(DEBUG$entity, "geoflow_entity")
    expect_equal(DEBUG$entity$identifiers$id, "cwp-grid-map-30deg_x_30deg")
  })
  
  #execute
  test_that("execute",{
    testthat::skip_on_cran()
    EXEC <- geoflow::executeWorkflow(cfg_file, dir = tempdir())
    expect_true(dir.exists(EXEC))
    expect_true(file.exists(file.path(EXEC, "job.json")))
    expect_true(file.exists(file.path(EXEC, "job-logs.txt")))
    expect_true(dir.exists(file.path(EXEC, "entities")))
  })
}
