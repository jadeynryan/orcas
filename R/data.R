#' CWR untidy
#'
#' Untidy Center for Whale Research data from 2017 to 2023.
#'
#' @format A data frame with 603 rows and 47 variables that need to be
#'   wrangled to reduce the redundancy in column names.
#' @source <https://www.whaleresearch.com/>
"data_cwr_untidy"

#' CWR tidy
#'
#' Tidy Center for Whale Research data from 2017 to 2023.
#'
#' @format A data frame with 593 rows and 16 variables:
#' \describe{
#'    \item{nmfs_permit}{Permits for photos}
#'    \item{enc_year}{Year of encounter}
#'    \item{enc_date}{Date of encounter}
#'    \item{enc_seq}{Encounter sequence}
#'    \item{enc_number}{Encounter number}
#'    \item{enc_start_time}{Encounter start time}
#'    \item{enc_end_time}{Encounter end time}
#'    \item{vessel}{Name of vessel(s) used for encounter observation}
#'    \item{observers}{Names of observers}
#'    \item{pod_ecotype}{Pod(s) and/or ecotype observed}
#'    \item{location}{Description of location}
#'    \item{begin_latitude}{Latitude encounter began}
#'    \item{end_latitude}{Latitude encounter ended}
#'    \item{begin_longitude}{Longitude encounter began}
#'    \item{end_longitude}{Longitude encounter ended}
#'    \item{enc_summary}{Summary of encounter}
#'    \item{ids_encountered}{IDs of whales encountered}
#'    ...
#' }
#' @source <https://www.whaleresearch.com/>
"data_cwr_tidy"
