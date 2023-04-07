#' Extract Link for Each Encounter
#' @description Extract encounter links from
#'   \href{https://www.whaleresearch.com/}{Center for Whale
#'   Research (CWR)} encounter landing pages.
#'
#'   2022 and 2023 encounters are on their current website while 2017 - 2021
#'   encounters are on their
#'   \href{https://whaleresearch.wixsite.com/archives}{archival website}.
#'
#' @param landing_url Character url to CWR encounter landing page.
#' @param website_type Either "current" or "archive" depending on whether the
#'   encounter landing page is on the CWR current website or archive website.
#'   Defaults to "current".
#' @param max_urls Number of urls to extract. Defaults to `Inf`.
#'
#' @returns Character vector of urls
#' @export
#'
#' @examples
#' library(rvest)
#' get_encounter_links("https://www.whaleresearch.com/2022encounters",
#'                     max_urls = 10)
get_encounter_links <-
  function(landing_url = "https://www.whaleresearch.com/2022encounters",
           website_type = "current",
           max_urls = Inf) {
    stopifnot("landing_url must be a character string" = is.character(landing_url))
    stopifnot("max_urls must be numeric." = is.numeric(max_urls))

    website_type <- match.arg(website_type, c("current", "archive"))

    selector <- switch(website_type,
                       current = ".font_6+ .font_7 , .comp-l3369ith .font_7+ .font_7 a",
                       archive = "#comp-kjk4daye2 span a")

    rvest::read_html(landing_url) |>
      rvest::html_elements(selector) |>
      rvest::html_attr("href") |>
      utils::head(max_urls)
  }

# # Extract data from encounter
# url <- "https://www.whaleresearch.com/2023-13"
#
# encounter <- read_html(url) |>
#   html_elements("#comp-lfqwu3ih2 span") |>
#   html_text2()
#
# get_encounter_data <- function(encounter_url) {
#   read_html(encounter_url) |>
#     html_elements("#comp-lfqwu3ih2 span") |>
#     html_text2()
# }
#
# encounter <- as.data.frame(encounter) |>
#   # Separate column by first colon
#   separate(
#     encounter,
#     into = c("variable", "data"),
#     sep = ":",
#     extra = "merge"
#   ) |>
#   # Move encounter summary text (row 15) to data column
#   mutate(data = case_when(variable == "EncSummary" ~ variable[15],
#                           TRUE ~ data)) |>
#   # Select the 14 rows with data (removes extra EncSummary row)
#   head(14) |>
#   # Pivot wider to tidy dataframe
#   pivot_wider(names_from = "variable",
#               values_from = "data")
