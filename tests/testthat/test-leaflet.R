test_that("make_leaflet() works", {
  expect_s3_class(make_leaflet(orcas::cwr_tidy[1:5, ]), "leaflet")
})
