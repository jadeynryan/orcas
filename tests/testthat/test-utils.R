test_that("href() works", {
  # Expect character string
  expect_type(href("a"), "character")
  # Test example
  expect_equal(href("google.com"),
               "<a href='google.com' target='_blank'>google.com</a>")
})
