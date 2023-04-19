#' CWR untidy
#'
#' Untidy Center for Whale Research data from 2017 to 2023.
#'
#' @format A data frame with 603 rows and 47 variables that need to be
#'   wrangled to reduce the redundancy in column names.
#' @source <https://www.whaleresearch.com/>
"cwr_untidy"

#' CWR tidy
#'
#' Tidy Center for Whale Research data from 2017 to 2023.
#'
#' @format A data frame with 593 rows and 19 variables:
#' \describe{
#'    \item{year}{Year of encounter}
#'    \item{encounter_sequence}{Encounter sequence}
#'    \item{encounter_number}{Encounter number}
#'    \item{date}{Date of encounter}
#'    \item{begin_time}{Encounter start time}
#'    \item{end_time}{Encounter end time}
#'    \item{duration}{Duration of encounter}
#'    \item{vessel}{Name of vessel(s) used for encounter observation}
#'    \item{observers}{Names of observers}
#'    \item{pods_or_ecotype}{Pod(s) and/or ecotype observed}
#'    \item{ids_encountered}{IDs of whales encountered}
#'    \item{location}{Description of location}
#'    \item{begin_latitude}{Latitude encounter began}
#'    \item{end_latitude}{Latitude encounter ended}
#'    \item{begin_longitude}{Longitude encounter began}
#'    \item{end_longitude}{Longitude encounter ended}
#'    \item{encounter_summary}{Summary of encounter}
#'    \item{nmfs_permit}{Permits for photos}
#'    \item{link}{URL to encounter webpage}
#'    ...
#' }
#' @source <https://www.whaleresearch.com/>
"cwr_tidy"
