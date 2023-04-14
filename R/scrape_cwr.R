#' Extract Link for Each CWR Encounter
#' @description Extract encounter links from
#'   \href{https://www.whaleresearch.com/}{Center for Whale
#'   Research (CWR)} encounter landing pages.
#'
#'   2022 and 2023 encounters are on their current website while 2017
#'   - 2021 encounters are on their
#'   \href{https://whaleresearch.wixsite.com/archives}{archival
#'   website}.
#'
#' @param year Numeric year 2017 or later. Defaults to 2023.
#' @param max_urls Number of urls to extract (from newest to oldest).
#'   Defaults to `Inf`.
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
#' map(2017:2020, get_encounter_links) |>
#'   flatten()
get_encounter_links <- function(year = 2023,
                                max_urls = Inf) {
  if (!is.numeric(year)) {
    stop("year must be numeric.")
  }

  if (!length(year) == 1) {
    stop(
      paste(
        "year must be a single value.",
        "See examples for using purrr::map()",
        "to get links for multiple years at once."
      )
    )
  }

  if (year < 2017 | year > format(Sys.Date(), "%Y")) {
    stop("year must be between 2017 and the current year.")
  }

  if (!is.numeric(max_urls)) {
    stop("max_urls must be numeric.")
  }

  url <- ifelse(
    year >= 2022,
    glue::glue("https://www.whaleresearch.com/2022encounters"),
    glue::glue("https://whaleresearch.wixsite.com/{year}encounters")
  )

  website_type <- ifelse(year >= 2022,
    "current",
    "archive"
  )

  selector <- switch(website_type,
    current = "//*[starts-with(@id, 'comp-l3369ith')]//a",
    archive = "//*[starts-with(@id, 'comp')]//a"
  )

  links <- tryCatch(
    rvest::read_html(url) |>
      rvest::html_elements(xpath = selector) |>
      rvest::html_attr("href")
  )

  links <- purrr::keep(links, function(link) {
    grepl(year, link)
  })

  links <- purrr::keep(links, function(link) {
    grepl(".*whaleresearch.*\\d$", link)
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
  if (!length(encounter_url) == 1) {
    stop("only one url allowed")
  }

  # Be polite and wait 2 seconds to avoid overloading server
  Sys.sleep(2)

  # Scrape webpage
  text <- tryCatch(
    rvest::read_html(encounter_url) |>
      rvest::html_elements(
        xpath = "//*[starts-with(@id,'comp-')]//p"
      ) |>
      rvest::html_text2()
  )

  # Print message for which url is being read
  message("Scraped: ", encounter_url)

  # Keep elements of interest (case insensitive)
  elements_to_keep <-
    paste(
      c(
        "enc",
        "date",
        "nmfs",
        "permit",
        "vessel",
        "staff",
        "observer",
        "pod",
        "ecotype",
        "start",
        "begin",
        "end",
        "time",
        "location",
        "lat",
        "latitude",
        "long",
        "longitude",
        "summary"
      ),
      collapse = "|"
    )

  results <-
    text[purrr::map_lgl(
      text,
      ~ any(grepl(
        elements_to_keep,
        tolower(.x)
      ))
    )]

  # Remove irrelevant case insensitive elements
  elements_to_remove <-
    paste(
      c(
        "folder",
        "\u00A9",
        "copyright",
        "nonprofit",
        "rights reserved",
        "email list",
        "whale news",
        "photos taken under federal",
        "please support"
      ),
      collapse = "|"
    )

  results <-
    results[purrr::map_lgl(
      results,
      ~ any(!grepl(
        elements_to_remove,
        tolower(.x)
      ))
    )]

  # Remove "ENCOUNTERS" text, if present
  results <-
    results[purrr::map_lgl(
      results,
      ~ any(!grepl(
        "ENCOUNTERS",
        .x
      ))
    )]

  return(results)
}

#' Parse CWR Encounter Data
#'
#' @param encounter_data Character vector from `get_encounter_data()`.
#'
#' @returns Dataframe with one row of encounter data parsed into
#'   columns.
#' @export
#'
#' @examples
#' get_encounter_data("https://www.whaleresearch.com/2022-13") |>
#'   parse_encounter()
parse_encounter <- function(encounter_data) {
  if (!is.character(encounter_data)) {
    stop("encounter_data must be character vector")
  }

  # Make dataframe
  results <- as.data.frame(encounter_data) |>
    # Move NMFS permit to top row to make it easier to extract the
    # encounter summary
    dplyr::arrange(!grepl("NMFS Permit",
      encounter_data,
      ignore.case = TRUE
    ))

  # Get row index of Encounter Summary
  summary <- paste(
    c(
      ".*ummary.*",
      "EncSummary.*",
      "Enc Summary.*",
      "Encounter Summary.*",
      "Encounter Summary Sequence.*",
      "EncounterSummary.*"
    ),
    collapse = "|"
  )

  summary_row_index <- which(grepl(summary, results$encounter_data))

  # Make one row dataframe of encounter summary (useful since some
  # encounter summaries span multiple list elements)
  encounter_summary <- results |>
    dplyr::slice(summary_row_index:length(results$encounter_data)) |>
    dplyr::summarize(encounter_data = paste(
      encounter_data,
      collapse = ""
    ))

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
    # Pivot wider to tidy dataframe
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "value",
      values_fn = ~ paste(.x, collapse = ", ")
    )

  # Clean column names
  names(results) <- janitor::make_clean_names(names(results))

  return(results)
}

#' Make a Dataframe of Multiple CWR Encounters From One Year
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
  if (!is.character(encounter_urls)) {
    stop("encounter_urls must be a character string or vector.")
  }

  # Map urls to get_encounter_data() and parse_encounter() functions
  encounter_urls |>
    purrr::map(\(x) get_encounter_data(x),
               .progress = "Getting data") |>
    purrr::map_df(\(x) parse_encounter(x),
                  .progress = "Parsing data")
}

#' Make a Dataframe of Multiple CWR Encounters From One Year
#'
#' Getting all encounters from 2017 to 2023 makes about 30 minutes.
#' The resulting dataframe will need to be wrangled and cleaned. See
#' `tidy_encounters().`
#'
#' @param years Numeric years from 2017 to present day.
#'
#' @returns Dataframe with one row per encounter.
#' @export
#'
get_all_encounters <- function(years) {
  links <- purrr::map(years, get_encounter_links) |>
    purrr::list_c()
  df <- make_encounter_df(links)
  return(df)
}

#' Save encounter dataframe to .Rdata
#'
#' @param df Dataframe to save to data folder.
#'
save_encounter_df <- function(df) {
  name <- deparse(substitute(df))

  if (!fs::dir_exists("data")) {
    fs::dir_create("data")
  }

  save(df, file = glue::glue("./data/{name}.Rdata"))
}
