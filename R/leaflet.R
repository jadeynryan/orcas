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
#'   pod_ecotype = c(
#'     "Transients", "Transients",
#'     "Transients", "J pod and L87"
#'   ),
#'   begin_latitude = c(
#'     48.485668182373, 48.6871681213379,
#'     48.4233322143555, 48.5499992370605
#'   ),
#'   begin_longitude = c(
#'     -123.124664306641, -123.237503051758,
#'     -123.251663208008, -123.171669006348
#'   ),
#'   end_latitude = c(
#'     48.3826675415039, 48.716667175293,
#'     48.4383316040039, 48.3856658935547
#'   ),
#'   end_longitude = c(
#'     -123.192497253418, -123.157836914062,
#'     -123.276664733887, -123.257499694824
#'   )
#' )
#'
#' make_leaflet(df)
#' }

make_leaflet <- function(data) {
  icons <- leaflet::iconList(
    whale = leaflet::makeIcon(
      "./inst/whale.png",
      iconWidth = 28
    ),
    tail = leaflet::makeIcon(
      "./inst/tail.png",
      iconWidth = 24
    )
  )

  leaflet::leaflet(data) |>
    leaflet::addProviderTiles("Stamen.Terrain") |>
    leaflet::addMarkers(~begin_longitude, ~begin_latitude,
      icon = ~ icons$whale
    ) |>
    leaflet::addMarkers(~end_longitude, ~end_latitude,
      icon = ~ icons$tail
    )
}