test_that("make_leaflet() works", {
  expect_s3_class(make_dt(orcas::cwr_tidy[1:5, ]), "datatables")
})
