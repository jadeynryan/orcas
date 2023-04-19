test_that("make_leaflet() works", {
  df <- data.frame(
    encounter_number = c(14, 13),
    date = c("2023-04-08", "2023-03-25"),
    duration = c("4980s (~1.38 hours)", "23700s (~6.58 hours)"),
    pods_or_ecotype = c("J, L", "J"),
    location = c("East Sooke", "Haro Strait"),
    begin_latitude = c(48.3108329772949, 48.5881652832031),
    begin_longitude = c(-123.664001464844, -123.203163146973),
    end_latitude = c(48.300666809082, 48.7113342285156),
    end_longitude = c(-123.747329711914, -123.272163391113),
    encounter_summary = c(
      "While out scanning the western...",
      "After receiving reports of J pod ..."
    ),
    link = c(
      "https://www.whaleresearch.com/2023-14",
      "https://www.whaleresearch.com/2023-13"
    )
  )

  expect_s3_class(make_leaflet(df), "leaflet")
})
