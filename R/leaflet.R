#' Make leaflet map of CWR encounters with Stamen.Terrain tiles and
#' whale icons
#'
#' @param data Tidy dataframe with latitude and longitude columns
#'
#' @returns Leaflet map
#' @export
#'
#' @examples
#'   make_leaflet(orcas::cwr_tidy[1:5, ])
make_leaflet <- function(data) {

  # Icon urls
  whale_url <- "https://raw.githack.com/jadeynryan/orcas/master/inst/whale.png"
  tail_url <- "https://raw.githack.com/jadeynryan/orcas/master/inst/tail.png"

  # Set up icon list
  icons <- leaflet::iconList(
    whale = leaflet::makeIcon(
      whale_url,
      iconWidth = 60
    ),
    tail = leaflet::makeIcon(
      tail_url,
      iconWidth = 50
    )
  )

  # Set up icon legend/controls
  # https://stackoverflow.com/questions/37423002/r-leaflet-custom-image-to-layer-control
  whale <- paste0("<img src='", whale_url, "' width='60px'><strong>Encounter Began")

  tail <- paste0("<img src='", tail_url, "' width='50px;'><strong>Encounter Ended")

  # Create map
  leaflet::leaflet(data) |>
    leaflet::addProviderTiles(
      "Stamen.Terrain"
    ) |>
    leaflet::addMarkers(~begin_longitude, ~begin_latitude,
      icon = ~ icons$whale,
      group = whale,
      popup = ~ lapply(paste0(
        "<b>Encounter Number</b>: ", encounter_number,
        "<br><b>Date</b>: ", date,
        "<br><b>Duration</b>: ", duration,
        "<br><b>Pod and/or Ecotype</b>: ", pods_or_ecotype,
        "<br><b>Location</b>: ", location,
        "<br><b>Link to Encounter Webpage</b>:",
        "<br>", href(link)
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
        "<br><b>Pod and/or Ecotype</b>: ", pods_or_ecotype,
        "<br><b>Location</b>: ", location,
        "<br><b>Link to Encounter Webpage</b>:",
        "<br>", href(link)
      ), htmltools::HTML),
      popupOptions = leaflet::popupOptions(
        maxHeight = 200,
        closeOnClick = TRUE
      ),
    ) |>
    leaflet::addLayersControl(
      overlayGroups = c(whale, tail),
      position = "topright",
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
}
