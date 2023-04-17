#' Make leaflet map of CWR encounters with Stamen.Terrain tiles and
#' whale icons
#'
#' @param data Tidy dataframe with latitude and longitude columns
#'
#' @returns Leaflet map
#' @export
#'
#' @examples {
#' df <- data.frame(
#'   encounter_number = c(14, 13),
#'   date = c("2023-04-08", "2023-03-25"),
#'   duration = c("4980s (~1.38 hours)", "23700s (~6.58 hours)"),
#'   pod_ecotype = c("J, L", "J"),
#'   location = c("East Sooke", "Haro Strait"),
#'   begin_latitude = c(48.3108329772949, 48.5881652832031),
#'   begin_longitude = c(-123.664001464844, -123.203163146973),
#'   end_latitude = c(48.300666809082, 48.7113342285156),
#'   end_longitude = c(-123.747329711914, -123.272163391113),
#'   encounter_summary = c(
#'     "While out scanning the western...",
#'     "After receiving reports of J pod ..."
#'   ),
#'   link = c(
#'     "https://www.whaleresearch.com/2023-14",
#'     "https://www.whaleresearch.com/2023-13"
#'   )
#' )
#'   make_leaflet(df)
#' }
make_leaflet <- function(data) {
  # Set up icon list
  icons <- leaflet::iconList(
    whale = leaflet::makeIcon(
      "./inst/whale.png",
      iconWidth = 60
    ),
    tail = leaflet::makeIcon(
      "./inst/tail.png",
      iconWidth = 50
    )
  )

  # Set up icon legend/controls
  # https://stackoverflow.com/questions/37423002/r-leaflet-custom-image-to-layer-control
  whale <- "<img src='https://raw.githack.com/jadeynryan/orcas/master/inst/whale.png' width='60px'>Encounter Began"

  tail <- "<img src='https://raw.githack.com/jadeynryan/orcas/master/inst/tail.png' width='50px;'>Encounter Ended"

  # Create map
  leaflet::leaflet(data) |>
    leaflet::addProviderTiles("Stamen.Terrain") |>
    leaflet::addMarkers(~begin_longitude, ~begin_latitude,
      icon = ~ icons$whale,
      group = whale,
      popup = ~ lapply(paste0(
        "<b>Encounter Number</b>: ", encounter_number,
        "<br><b>Date</b>: ", date,
        "<br><b>Duration</b>: ", duration,
        "<br><b>Pod and/or Ecotype</b>: ", pod_ecotype,
        "<br><b>Location</b>: ", location,
        "<br><b>Encounter Summary</b>:<br>", encounter_summary,
        "<br><b>Link to Encounter Webpage</b>:",
        "<br><a href='", link, "' target='_blank'>", link, "</a>"
      ), htmltools::HTML),
      popupOptions = leaflet::popupOptions(
        maxHeight = 200,
        closeOnClick = TRUE
      )
    ) |>
    leaflet::addMarkers(~end_longitude, ~end_latitude,
      icon = ~ icons$tail,
      group = tail,
      popup = ~ lapply(paste0(
        "<b>Encounter Number</b>: ", encounter_number,
        "<br><b>Date</b>: ", date,
        "<br><b>Duration</b>: ", duration,
        "<br><b>Pod and/or Ecotype</b>: ", pod_ecotype,
        "<br><b>Location</b>: ", location,
        "<br><b>Encounter Summary</b>:<br>", encounter_summary,
        "<br><b>Link to Encounter Webpage</b>:",
        "<br><a href='", link, "' target='_blank'>", link, "</a>"
      ), htmltools::HTML),
      popupOptions = leaflet::popupOptions(
        maxHeight = 200,
        closeOnClick = TRUE
      ),
    ) |>
    leaflet::addLayersControl(
      overlayGroups = c(whale, tail),
      position = "bottomleft",
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
}
