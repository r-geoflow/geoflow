# test_software_clients.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for software clients
#=======================
require(geosapi, quietly = TRUE)
require(testthat)

test_that("GeoServer is properly configured",{
  GS <- geosapi::GSManager$new(
    url = "http://localhost:8080/geoserver",
    user = "admin", pwd = "geoserver",
    logger = "DEBUG"
  )
  expect_is(GS, "GSManager")
})

test_that("GeoNetwork is properly configured",{
  GN <- geonapi::GNManager$new(
    url = "http://localhost:8282/geonetwork", version = "3.6.0",
    user = "admin", pwd = "admin",
    logger = "DEBUG"
  )
  expect_is(GS, "GNManager")
})
