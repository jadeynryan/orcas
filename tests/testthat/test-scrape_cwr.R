test_that("get_encounter_links() works", {
  # Error if max_urls is not numeric
  expect_error(
    get_encounter_links("https://www.whaleresearch.com/2022encounters",
                        max_urls = "a")
  )
  # Max urls = 3 has vector with length 3
  expect_length(
    get_encounter_links("https://www.whaleresearch.com/2022encounters",
                        max_urls = 3),
    3
  )
  # Returns character vector
  expect_type(
    get_encounter_links("https://www.whaleresearch.com/2022encounters"),
    "character"
  )
})
