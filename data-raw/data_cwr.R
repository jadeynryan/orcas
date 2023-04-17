## Code to prepare `data_cwr` dataset

# Initial scrape to extract and tidy all encounter date
cwr_untidy <- orcas::make_encounter_df(years = 2017:2023)
usethis::use_data(cwr_untidy, overwrite = TRUE)

# Tidy dataframe by uniting columns with the same data but slightly
# differing names. This script only works for the entire 2017:2023
# dataset because column names are hardcoded from my manual
# identification of typos and inconsistent variable naming.

cwr_tidy <- cwr_untidy |>
  tidyr::separate(
    col = "begin_lat_long",
    sep = "/",
    into = c("b_latitude", "b_longitude")
  ) |>
  tidyr::separate(
    col = "end_lat_long",
    sep = "/",
    into = c("e_latitude", "e_longitude")
  ) |>
  tidyr::unite(date,
    tidyr::any_of(c("enc_date", "date")),
    sep = "", na.rm = TRUE
  ) |>
  tidyr::unite(begin_time,
    tidyr::any_of(c(
      "enc_start_time",
      "observ_begin",
      "encounter_state_time"
    )),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(end_time,
    tidyr::any_of(c(
      "enc_end_time",
      "observ_end",
      "encounter_end_time"
    )),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(encounter_number,
    tidyr::any_of(c(
      "enc_number", "encounter_number", "enc",
      "encounter_numbe"
    )),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(encounter_sequence,
    tidyr::any_of(c("sequence", "enc_seq", "enc_s_eq")),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(observers,
    tidyr::any_of(c("observers", "staff")),
    sep = "",
    na.rm = TRUE
  ) |>
  # Combine other observers with observers and staff, with comma sep
  tidyr::unite(observers,
    tidyr::any_of(c("observers", "other_observers")),
    sep = ",",
    na.rm = TRUE
  ) |>
  tidyr::unite(pod_ecotype,
    tidyr::any_of(c("pods", "pods_or_ecotype", "ecotype")),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(location,
    tidyr::any_of(c(
      "location_descr", "locationn_descr", "location"
    )),
    sep = "", na.rm = TRUE
  ) |>
  tidyr::unite(begin_latitude,
    tidyr::any_of(c(
      "begin_latitude",
      "start_latitude",
      "b_latitude"
    )),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(begin_longitude,
    tidyr::any_of(c(
      "begin_longitude",
      "start_longitude",
      "b_longitude"
    )),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(end_latitude,
    tidyr::any_of(c(
      "end_latitude",
      "e_latitude"
    )),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(end_longitude,
    tidyr::any_of(c(
      "end_longitude",
      "e_longitude"
    )),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(vessel,
    tidyr::any_of(c("vessel", "vessels", "other_vessel")),
    sep = "", na.rm = TRUE
  ) |>
  tidyr::unite(encounter_summary,
    tidyr::any_of(
      c(
        "enc_summary",
        "encounter_summary",
        "summary",
        "enc_ummary",
        "enc_summary_sequence_number_1",
        "encounter_summary_sequence_1",
        "encounter_summary_sequence_2_3"
      )
    ),
    sep = "",
    na.rm = TRUE
  )

# Trim whitespace, fix column types, correct typos ---------------
cwr_tidy <- cwr_tidy |>
  dplyr::mutate(
    dplyr::across(everything(), ~ dplyr::na_if(trimws(.), "")),
    date = lubridate::parse_date_time(date, c("dmy")),
    date = dplyr::case_when(
      date == lubridate::date("0218-09-30") ~
        lubridate::date("2018-09-30"),
      date == lubridate::date("2014-08-08") ~
        lubridate::date("2018-07-24"),
      TRUE ~ lubridate::date(date),
    ),
    year = lubridate::year(date),
    encounter_number = ifelse(date == lubridate::date("2022-08-31"),
      53, as.numeric(encounter_number)
    )
  )

# Calculate duration of encounter -------------------------------

cwr_tidy <- cwr_tidy |>
  dplyr::mutate(
    dplyr::across(
      tidyr::contains("time"),
      \(x) lubridate::parse_date_time(x, orders = "HM")
    ),
    duration = lubridate::as.duration(
      lubridate::interval(begin_time, end_time)
    ),
    .after = "end_time",
    dplyr::across(tidyr::contains("time"), \(x) substr(x, 12, 16))
  )

# Clean up coordinates --------------------------------------

# Parse coordinates
cwr_tidy <- cwr_tidy |>
  dplyr::mutate(
    dplyr::across(dplyr::ends_with("latitude"), parzer::parse_lat),
    dplyr::across(dplyr::ends_with("longitude"), parzer::parse_lon)
  ) |>
  # Make longitude negative
  dplyr::mutate(dplyr::across(
    dplyr::ends_with("longitude"),
    \(x) abs(x) * -1
  )) |>
  # Change coordinates to NaN if they are out of bounds
  dplyr::mutate(
    begin_latitude = dplyr::case_when(
      begin_latitude < 45 ~ NaN,
      TRUE ~ begin_latitude
    ),
    end_latitude = dplyr::case_when(
      end_latitude < 45 ~ NaN,
      TRUE ~ end_latitude
    ),
    begin_longitude = dplyr::case_when(
      begin_longitude > -120 ~ NaN,
      TRUE ~ begin_longitude
    ),
    end_longitude = dplyr::case_when(
      end_longitude > -120 ~ NaN,
      TRUE ~ end_longitude
    )
  )

# Final tidying -----------------------------------------

# Remove unneeded columns
cwr_tidy <- cwr_tidy |>
  dplyr::select(
    -c(
      "encounter_summary_to_follow",
      "location_code",
      "other_location_code",
      "end_latitude_48_19_5",
      "enc_number_53"
    )
  ) |>
  # Rename columns
  dplyr::rename("ids_encountered" = "i_ds_encountered") |>
  # Relocate columns
  dplyr::relocate("year", .before = "date") |>
  dplyr::relocate("ids_encountered", .after = "pod_ecotype") |>
  # Remove empty rows and columns
  janitor::remove_empty(which = c("rows", "cols")) |>
  # Remove duplicate rows
  dplyr::distinct() |>
  # Reorder columns
  dplyr::select(year, encounter_sequence, encounter_number,
                date, begin_time, end_time, duration, vessel,
                observers, pod_ecotype, ids_encountered, location,
                begin_latitude, begin_longitude, end_latitude,
                end_longitude, encounter_summary, nmfs_permit, link) |>
  # Arrange by date
  dplyr::arrange(desc(date))

write.csv(cwr_tidy, "data-raw/cwr_tidy.csv")
usethis::use_data(cwr_tidy, overwrite = TRUE)
