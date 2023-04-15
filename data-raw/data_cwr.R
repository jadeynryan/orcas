## Code to prepare `data_cwr` dataset goes here

# Initial scrape to extract and tidy all encounter date
data_cwr_untidy <- get_all_encounters(years = 2017:2023)
usethis::use_data(data_cwr_untidy, overwrite = TRUE)

# Tidy dataframe by uniting columns with the same data but slightly
# differing names
data_cwr_tidy <- data_cwr_untidy |>
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
  tidyr::unite(enc_date,
    tidyr::any_of(c("enc_date", "date")),
    sep = "", na.rm = TRUE
  ) |>
  tidyr::unite(enc_start_time,
    tidyr::any_of(c(
      "enc_start_time",
      "observ_begin",
      "encounter_state_time"
    )),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(enc_end_time,
    tidyr::any_of(c(
      "enc_end_time",
      "observ_end",
      "encounter_end_time"
    )),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(enc_number,
    tidyr::any_of(c(
      "enc_number", "encounter_number", "enc",
      "encounter_numbe"
    )),
    sep = "",
    na.rm = TRUE
  ) |>
  tidyr::unite(enc_seq,
    tidyr::any_of(c("sequence", "enc_seq", "enc_s_eq")),
    sep = "", na.rm = TRUE
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
  tidyr::unite(enc_summary,
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
  ) |>
  # Trim whitespace, format date column, correct typos
  dplyr::mutate(
    dplyr::across(everything(), ~ dplyr::na_if(trimws(.), "")),
    enc_date = lubridate::parse_date_time(enc_date, c("dmy")),
    enc_date = dplyr::case_when(
      enc_date == lubridate::date("0218-09-30") ~ lubridate::date("2018-09-30"),
      enc_date == lubridate::date("2014-08-08") ~ lubridate::date("2018-07-24"),
      TRUE ~ lubridate::date(enc_date),
    ),
    enc_year = lubridate::year(enc_date),
    enc_number = ifelse(enc_date == lubridate::date("2022-08-31"),
      53,
      enc_number
    )
  ) |>
  # Parse coordinates
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
  ) |>
  # Remove unneeded columns
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
  dplyr::relocate("enc_year", .before = "enc_date") |>
  dplyr::relocate("ids_encountered", .after = "pod_ecotype") |>
  # Remove empty rows and columns
  janitor::remove_empty(which = c("rows", "cols")) |>
  # Remove duplicate rows
  dplyr::distinct()

usethis::use_data(data_cwr_tidy, overwrite = TRUE)
