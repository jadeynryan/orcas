#' Extract Link for Each CWR Encounter
#' @description Extract encounter links from
#'   \href{https://www.whaleresearch.com/}{Center for Whale
#'   Research (CWR)} encounter landing pages.
#'
#'   2022 and 2023 encounters are on their current website while 2017 - 2021
#'   encounters are on their
#'   \href{https://whaleresearch.wixsite.com/archives}{archival website}.
#'
#' @param year Numeric year 2017 or later. Defaults to 2023.
#' @param max_urls Number of urls to extract (from newest to oldest). Defaults
#'   to `Inf`.
#'
#' @returns Character vector of urls.
#' @export
#'
#' @examples
#' # Get 10 most recent encounter links for one year
#' get_encounter_links(year = 2022, max_urls = 10)
#'
#' # Get all encounter links from 2017 through 2020
#' library(purrr)
#' purrr::map(2017:2020, get_encounter_links) |>
#'   purrr::flatten()
get_encounter_links <- function(year = 2023,
                                max_urls = Inf) {
  stopifnot("year must be numeric." = is.numeric(year))
  if (year < 2017 | year > format(Sys.Date(), "%Y")) {
    stop("year must be between 2017 and the current year.")
  }
  stopifnot("year must be a single value." = length(year) == 1)
  stopifnot("max_urls must be numeric." = is.numeric(max_urls))

  url <- ifelse(
    year >= 2022,
    glue::glue("https://www.whaleresearch.com/2022encounters"),
    glue::glue("https://whaleresearch.wixsite.com/{year}encounters")
  )

  website_type <- ifelse(year >= 2022,
                         "current",
                         "archive")

  selector <- switch(website_type,
                     current = "//*[starts-with(@id, 'comp-l3369ith')]//a",
                     archive = "//*[starts-with(@id, 'comp')]//a")

  links <- tryCatch(
    rvest::read_html(url) |>
      rvest::html_elements(xpath = selector) |>
      rvest::html_attr("href")
  )

  links <- purrr::keep(links, function(link) {
    grepl(year, link)
  }) |>
    utils::head(max_urls)

  return(links)
}

#' Extract CWR Data from Encounter Webpage
#'
#' @param encounter_url Character url for CWR encounter.
#'
#' @returns Character vector to be input to `parse_encounter()`.
#' @export
#'
#' @examples
#' get_encounter_data("https://www.whaleresearch.com/2023-13")
get_encounter_data <- function(encounter_url) {
  stopifnot("only one url allowed" = length(encounter_url) == 1)
  # Be polite and wait 2 seconds to avoid overloading server
  Sys.sleep(2)

  # Scrape webpage
  results <- tryCatch(
    rvest::read_html(encounter_url) |>
      rvest::html_elements(xpath = "//*[starts-with(@id,'comp-')]//p") |>
      rvest::html_text2()
  )

  # Print message for which url is being read
  message("Scraped URL: ", encounter_url)

  return(results)
}

#' Parse CWR Encounter Data
#'
#' @param encounter_data Character vector from `get_encounter_data()`.
#'
#' @returns Dataframe with one row of encounter data parsed into columns.
#' @export
#'
#' @examples
#' get_encounter_data("https://www.whaleresearch.com/2023-13") |>
#'   parse_encounter()
parse_encounter <- function(encounter_data) {
  stopifnot("encounter_data must be character vector" =
              is.character(encounter_data))

  # Text to remove from dataframe
  remove <- paste(c("Photos taken",
                    "Get the latest",
                    "Join our email list",
                    "©",
                    "501c3 nonprofit",
                    "All rights reserved"),
                  collapse = "|")

  results <- as.data.frame(encounter_data) |>
    # Filter rows that are empty or irrelevant
    dplyr::filter(!grepl(remove, encounter_data) &
                    !encounter_data == " ") |>
    # Move NMFS permit to top row to make it easier to extract the
    # encounter summary
    dplyr::arrange(!grepl("NMFS", encounter_data))

  # Get row index of Encounter Summary
  summary <- paste(c("EncSummary",
                     "Enc Summary",
                     "Encounter Summary",
                     "EncounterSummary"),
                   collapse = "|")
  summary_row_index <- which(grepl(summary, encounter_data))

  # Make one row dataframe of encounter summary (useful since some
  # encounter summaries span multiple list elements)
  encounter_summary <- results |>
    dplyr::slice(summary_row_index:length(encounter_data)) |>
    dplyr::summarize(encounter_data = paste(encounter_data,
                                            collapse = ""))

  results <- results |>
    # Remove multi-row encounter summary
    dplyr::slice(1:summary_row_index - 1) |>
    # Add single row encounter summary
    dplyr::bind_rows(encounter_summary) |>
    # Separate column by first colon
    tidyr::separate(
      encounter_data,
      into = c("variable", "value"),
      sep = ":",
      extra = "merge"
    ) |>
    # Trim whitespace
    dplyr::mutate(value = trimws(.data$value)) |>
    # Pivot wider to tidy dataframe
    tidyr::pivot_wider(names_from = "variable",
                       values_from = "value")

  # Clean column names
  names(results) <- janitor::make_clean_names(names(results))

  return(results)
}

#' Make a Dataframe of Multiple CWR Encounters
#'
#' @param encounter_urls Character vector of CWR urls. Pipe
#'   `get_encounter_links` to this function.
#'
#' @returns Dataframe with one row per encounter.
#' @export
#'
#' @examples
#' # Piping the `get_encounter_links()` function
#' get_encounter_links(year = 2023, max_urls = 2) |>
#'   make_encounter_df()
make_encounter_df <- function(encounter_urls) {
  # stopifnot("encounter_urls must be a character string or vector." =
  #             is.character(encounter_urls))

  df <- encounter_urls |>
    purrr::map(.x = _, tryCatch(get_encounter_data))
  #   purrr::map_df(.x = _, tryCatch(parse_encounter))
  #
  # df <- df |>
  #   # tidyr::unite(enc_number, tidyr::any_of(c("enc_number", "enc"))) |>
  #   tidyr::unite(enc_date, tidyr::any_of(c("enc_date", "date"))) |>
  #   # Convert column types
  #   dplyr::mutate(enc_date = as.Date(.data$enc_date,
  #                                    tryFormats = c("%d/%m/%y",
  #                                                   "%d-%m-%y",
  #                                                   "%d/%B/%y",
  #                                                   "%d-%B-%y",
  #                                                   "%d/%b/%y",
  #                                                   "%d-%b-%y"
  #                                                   )))

  # return(df)
}

get_all_encounters <- function(years = 2023) {
  links <- purrr::map(years, get_encounter_links) |>
    purrr::flatten()
  make_encounter_df(links)
}
