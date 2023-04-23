
<!-- README.md is generated from README.Rmd. Please edit that file -->

# orcas

<!-- badges: start -->

[![R-CMD-check](https://github.com/jadeynryan/orcas/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jadeynryan/orcas/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jadeynryan/orcas/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jadeynryan/orcas?branch=master)

<!-- badges: end -->

The goal of `orcas` is to scrape orca sighting data from the web and
visualize it in maps and tables.

[R Ladies Seattle](https://rladiesseattle.org "R Ladies Seattle URL")
invited me to give a talk for the ‘R in the Outdoors’
[meetup](https://www.meetup.com/rladies-seattle/ "meetup URL"). This was
my first in-person talk of my professional career! I used this as an
opportunity to learn new skills through a personal project. I’ve always
had an affinity for the Southern Resident Killer Whales in the Salish
Sea. The [Center for Whale
Research](https://www.whaleresearch.com "Center for Whale Research URL")
does a lot of really fascinating and important work monitoring their
population. They post their survey data on their website; each encounter
with the orcas is a separate webpage. Lately, I’ve been curious and
intimidated by web scraping so I decided this would make a great case
study and personal project.

`orcas` is still a work in progress, as the `cwr_tidy` dataset is mostly
tidy but not completely clean. There are still missing values and typos,
as evident from some encounters having a negative duration.

> All photos and data belong to the Center for Whale Research, a 501c3
> nonprofit organization registered in Washington State.

## Installation

You can install the development version of `orcas` from
[GitHub](https://github.com/ "GitHub URL") with:

``` r
# install.packages("devtools")
devtools::install_github("jadeynryan/orcas")
```

## R in the Outdoors Presentation

While there is no recording of the talk, you can view the
[slides](https://rpubs.com/jadeynryan/orcas_RLadiesSeattleTalk "slides URL").

[![Title slide: “Web Scraping & Mapping {orcas} Encounters”, R in the
Outdoors hosted by R Ladies Seattle, presented by Jadey Ryan on April
20,
2023](inst/img/title_slide_J53_breach_2022.png)](https://rpubs.com/jadeynryan/orcas_RLadiesSeattleTalk)

## Examples

**Scrape the two most recent encounters from 2022 and 2023:**

``` r
orcas::make_encounter_df(years = 2022:2023, max_urls = 2)
#> Scraped: https://www.whaleresearch.com/2022-82
#>  ■■■■■■■■■                         25% |  ETA:  7sScraped: https://www.whaleresearch.com/2022-81
#>  ■■■■■■■■■■■■■■■■                  50% |  ETA:  5sScraped: https://www.whaleresearch.com/2023-14
#>  ■■■■■■■■■■■■■■■■■■■■■■■           75% |  ETA:  2sScraped: https://www.whaleresearch.com/2023-13
#>                                                    
#> # A tibble: 4 × 17
#>   link          enc_date enc_seq enc_number observ_begin observ_end vessel staff
#>   <chr>         <chr>    <chr>   <chr>      <chr>        <chr>      <chr>  <chr>
#> 1 https://www.… "27/12/… 1       82         03:20 PM     04:19 PM   Mike 1 Mark…
#> 2 https://www.… "08/12/… 1       81         02:40 PM     03:42 PM   Mike 1 Mark…
#> 3 https://www.… "08/04/… 1       14         04:22 PM     05:45 PM   Mike 1 Mark…
#> 4 https://www.… "25/03/… 1       13         09:30 AM     02:55 PM   Orcin… Dave…
#> # ℹ 9 more variables: other_observers <chr>, pods <chr>, location_descr <chr>,
#> #   start_latitude <chr>, start_longitude <chr>, end_latitude <chr>,
#> #   end_longitude <chr>, enc_summary <chr>, nmfs_permit <chr>
```

**Make an interactive DT of 2023 encounters:**

``` r
orcas::cwr_tidy |> 
  subset(year == 2023) |> 
  orcas::make_dt()
```

![Screenshot of datatable with two rows and columns: “Date”, “Encounter
number”, “Begin time”, “End time”, “Duration (min)”, “Vessel”,
“Observers”, “Pods or ecotype”, “Location”, and “Encounter
summary”.](inst/img/dt_screenshot.png)

**Make an interactive leaflet map of last two 2023 encounters:**

``` r
orcas::cwr_tidy |> 
  subset(year == 2023) |> 
  head(2) |> 
  orcas::make_leaflet()
```

![Screenshot of leaflet map with whale icons representing the start and
end of the encounters.](inst/img/leaflet_screenshot.png)
