#' Make a DT
#'
#' Creates an interactive DataTable using the R interface to the
#' JavaScript library DataTables
#'
#' @param data CWR tidy dataframe.
#' @importFrom rlang .data
#'
#' @returns A datatable object.
#' @export
#'
#' @examples
#'   df <- data.frame(
#'     encounter_number = c(14, 13),
#'     date = c("2023-04-08", "2023-03-25"),
#'     duration = c("4980s (~1.38 hours)", "23700s (~6.58 hours)"),
#'     pods_or_ecotype = c("J, L", "J"),
#'     location = c("East Sooke", "Haro Strait"),
#'     begin_latitude = c(48.3108329772949, 48.5881652832031),
#'     begin_longitude = c(-123.664001464844, -123.203163146973),
#'     end_latitude = c(48.300666809082, 48.7113342285156),
#'     end_longitude = c(-123.747329711914, -123.272163391113),
#'     encounter_summary = c(
#'       "While out scanning the western...[trunc]",
#'       "After receiving reports of J pod ...[trunc]"
#'     ),
#'     link = c(
#'       "https://www.whaleresearch.com/2023-14",
#'       "https://www.whaleresearch.com/2023-13"
#'     )
#'   )
#' make_dt(df)
make_dt <- function(data) {
  data <- data |>
    dplyr::mutate(link = href(.data$link)) |>
    janitor::clean_names(case = "sentence") |>
    dplyr::select(!tidyr::matches("latitude|longitude|permit"))

  table <- data |> DT::datatable(
    class = "table-compact row-border",
    rownames = FALSE,
    escape = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      dom = "Bfrtip",
      buttons = list(
        list(
          extend = "excel",
          text = "Download",
          filename = "CWR_Encounters",
          title = "Center for Whale Research Encounters (https://www.whaleresearch.com/)"
        ),
        list(
          extend = "colvis"
        ),
        list(
          extend = "pageLength"
        )
      )
    )
  )

  return(table)
}
