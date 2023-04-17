test_that("get_encounter_links() works", {
  # Returns character vector
  expect_type(get_encounter_links(),
              "character")

  # max_urls = 3 has vector with length 3
  expect_length(get_encounter_links(max_urls = 3), 3)

  # Error if year is not numeric
  expect_error(get_encounter_links(year = "a"))

  # Error if year is not between 2017 and the current year
  expect_error(get_encounter_links(year = 2000))
  expect_error(get_encounter_links(year = 2030))

  # Error if user enters more than 1 year
  expect_error(get_encounter_links(year = 2017:2023))

  # Error if max_urls is not numeric
  expect_error(get_encounter_links(max_urls = "a"))
})

test_that("get_encounter_data() works", {
  # Returns character vector
  expect_type(get_encounter_data("https://www.whaleresearch.com/2023-4"),
              "character")

  # Error if user enters more than 1 url
  expect_error(get_encounter_data(
    c(
      "https://www.whaleresearch.com/2023-4",
      "https://www.whaleresearch.com/2023-09"
    )
  ))
})

test_that("parse_encounter() works", {
  encounter <- get_encounter_data("https://www.whaleresearch.com/2023-13")

  # Returns dataframe
  expect_s3_class(parse_encounter(encounter), "data.frame")

  # Error if encounter_data is not character vector
  expect_error(parse_encounter(1))
})

test_that("make_encounter_df() works", {

  df <- make_encounter_df(years = 2022:2023, max_urls = 1)

  # Returns dataframe
  expect_s3_class(df, "data.frame")

  # Number of rows = number of total urls = number of encounters
  expect_equal(nrow(df), 2)
})
