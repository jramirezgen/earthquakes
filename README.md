# earthquakes

[![Build Status](https://app.travis-ci.com/jramirezgen/earthquakes.svg?branch=main)](https://app.travis-ci.com/jramirezgen/earthquakes)

`earthquakes` cleans, visualizes and maps the NOAA (National Oceanic and
Atmospheric Administration) Significant Earthquake Database. It was built as the
capstone of the Coursera specialization *Mastering Software Development in R*.
The package includes a copy of the database (`inst/extdata/earthquakes.tsv.gz`).

## Installation

```r
# install.packages("devtools")
devtools::install_github("jramirezgen/earthquakes", build_vignettes = TRUE)
```

## Functions

| Function | What it does |
| --- | --- |
| `eq_clean_data()` | Adds a `DATE` column (class `Date`) and makes `LATITUDE`/`LONGITUDE` numeric |
| `eq_location_clean()` | Removes the country from `LOCATION_NAME` and uses title case |
| `geom_timeline()` | ggplot2 geom: time line of earthquakes (size = magnitude, colour = deaths, one line per factor level) |
| `geom_timeline_label()` | ggplot2 geom: labels the `n_max` largest earthquakes of each time line |
| `theme_timeline()` | Minimal theme for the time line |
| `eq_map()` | Interactive leaflet map with a pop-up taken from `annot_col` |
| `eq_create_label()` | HTML pop-up text: location, magnitude and total deaths |

## Example

```r
library(earthquakes)
library(dplyr)
library(ggplot2)

file <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
data <- readr::read_delim(file, delim = "\t") %>% eq_clean_data()

# time line
data %>%
  filter(COUNTRY %in% c("USA", "CHINA"), lubridate::year(DATE) >= 2000) %>%
  eq_location_clean() %>%
  ggplot(aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, colour = TOTAL_DEATHS)) +
  geom_timeline(alpha = 0.5) +
  geom_timeline_label(aes(label = LOCATION_NAME), n_max = 3) +
  theme_timeline()

# interactive map with HTML labels
data %>%
  filter(COUNTRY == "MEXICO", lubridate::year(DATE) >= 2000) %>%
  mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")
```

See `vignette("earthquakes")` for a full walkthrough.
