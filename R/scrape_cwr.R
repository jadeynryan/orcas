#' Extract Link for Each CWR Encounter
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
#' @returns Character vector of urls.
#' @export
#'
#' @examples
#' get_encounter_links("https://www.whaleresearch.com/2022encounters",
#'                     max_urls = 10)
get_encounter_links <-
  function(landing_url = "https://www.whaleresearch.com/2022encounters",
           website_type = "current",
           max_urls = Inf) {
    stopifnot("landing_url must be a character string" =
                is.character(landing_url))
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
  # Be polite and wait 3 seconds to avoid overloading server
  Sys.sleep(3)

  # Scrape webpage
  results <- rvest::read_html(encounter_url) |>
    rvest::html_elements(xpath = "//*[starts-with(@id,'comp-l')]//p") |>
    rvest::html_text2()

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
#' parse_encounter()
parse_encounter <- function(encounter_data) {
  # Text to remove from dataframe
  remove <- paste(c("Photos taken",
                    "Get the latest",
                    "Join our email list"),
                  collapse = "|")

  results <- as.data.frame(encounter_data) |>
    # Filter rows that are empty or irrelevant
    dplyr::filter(!grepl(remove, encounter_data) &
                    !encounter_data == " ") |>
    # Move NMFS permit to top row
    dplyr::arrange(!grepl("NMFS", encounter_data))

  # Get row index of Encounter Summary
  summary_row_index <- which(grepl("EncSummary", encounter_data))

  # Make one row dataframe of encounter summary (useful since some
  # encounter summaries span multiple list elements)
  encounter_summary <- results |>
    dplyr::slice(summary_row_index:dplyr::n()) |>
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
      into = c("variable", "data"),
      sep = ":",
      extra = "merge"
    ) |>
    # Trim whitespace
    dplyr::mutate(data = trimws(.data$data)) |>
    # Pivot wider to tidy dataframe
    tidyr::pivot_wider(names_from = "variable",
                       values_from = "data")

  # Clean column names
  names(results) <- janitor::make_clean_names(names(results))

  # Convert column types
  results <- results |>
    dplyr::mutate(
      enc_date = as.Date(.data$enc_date,
                         tryFormats = c("%d/%m/%y", "%d-%m-%y")),
      enc_seq = as.integer(.data$enc_seq),
      enc_number = as.integer(.data$enc_number)
    )

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
#' get_encounter_links(max_urls = 3) |>
#'   make_encounter_df()
#'
#' # Providing a character vector of urls
#' make_encounter_df(c("https://www.whaleresearch.com/2023-4",
#'                     "https://www.whaleresearch.com/2023-09"))
make_encounter_df <- function(encounter_urls) {
  as.list(encounter_urls) |>
    purrr::map(.x = _, get_encounter_data) |>
    purrr::map(.x = _, parse_encounter) |>
    dplyr::bind_rows()
}
