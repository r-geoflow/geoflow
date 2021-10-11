# test_software_clients.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for software clients
#=======================
require(geoflow, quietly = TRUE)
require(testthat)

test_that("GeoServer is properly configured",{
  GS <- geosapi::GSManager$new(
    url = "http://localhost:8080/geoserver",
    user = "admin", pwd = "geoserver",
    logger = "DEBUG"
  )
  expect_is(GS, "GSManager")
})
