test_that("get_encounter_links() works", {
  # Returns character vector
  expect_type(
    get_encounter_links("https://www.whaleresearch.com/2022encounters"),
    "character"
  )

  # Default max_urls (Inf) captures all links and archive website_type works
  expect_length(
    get_encounter_links(
      "https://whaleresearch.wixsite.com/2021encounters",
      website_type = "archive"
    ),
    89
  )

  # max_urls = 3 has vector with length 3
  expect_length(
    get_encounter_links("https://www.whaleresearch.com/2022encounters",
                        max_urls = 3),
    3
  )

  # Error if url is not character
  expect_error(get_encounter_links(2))

  # Error if max_urls is not numeric
  expect_error(
    get_encounter_links("https://www.whaleresearch.com/2022encounters",
                        max_urls = "a")
  )
})
