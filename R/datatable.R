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
#' make_dt(orcas::cwr_tidy[1:5, ])
make_dt <- function(data) {
  data <- data |>
    dplyr::mutate(
      link = href(.data$link),
      duration = as.numeric(.data$duration) / 60
    ) |>
    dplyr::select(!tidyr::matches("year|sequence|latitude|longitude|permit|ids")) |>
    janitor::clean_names(case = "sentence") |>
    dplyr::rename("Duration (min)" = "Duration")

  table <- data |> DT::datatable(
    fillContainer = TRUE,
    class = "table-compact row-border",
    rownames = FALSE,
    escape = FALSE,
    extensions = c("Buttons", "Scroller"),
    options = list(
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#3C584A', 'color': '#fff'});",
        "}"
      ),
      dom = "Bfrtip",
      autowidth = TRUE,
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20),
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
      ),
      columnDefs = list(
        list(
          targets = 9,
          render = DT::JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 10 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
            "}"
          )
        )
      )
    )
  )

  return(table)
}
