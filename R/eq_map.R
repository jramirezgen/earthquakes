#' Interactive map of earthquakes
#'
#' Maps the epicentres (\code{LATITUDE}, \code{LONGITUDE}) of the earthquakes
#' in an interactive \pkg{leaflet} map. Each earthquake is a circle whose
#' radius is proportional to its magnitude (\code{EQ_PRIMARY}) and has a pop-up
#' window with the text of one column.
#'
#' @param data A (cleaned and filtered) data frame with the columns
#'   \code{LATITUDE}, \code{LONGITUDE} and \code{EQ_PRIMARY}.
#' @param annot_col Name of the column whose values are shown in the pop-up
#'   windows. Default \code{"DATE"}.
#'
#' @return A \pkg{leaflet} map (htmlwidget).
#'
#' @examples
#' d <- data.frame(LATITUDE = c(16.8, 19.4), LONGITUDE = c(-99.9, -99.1),
#'                 EQ_PRIMARY = c(6.3, 7.1),
#'                 DATE = as.Date(c("2014-07-29", "2017-09-19")))
#' eq_map(d, annot_col = "DATE")
#'
#' @export
eq_map <- function(data, annot_col = "DATE") {
  if (!annot_col %in% names(data))
    stop("annot_col '", annot_col, "' is not a column of data")
  mag <- data$EQ_PRIMARY
  radius <- ifelse(is.na(mag), 2, mag * 1.5)

  leaflet::leaflet(data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = ~LONGITUDE, lat = ~LATITUDE, radius = radius,
      weight = 1, fillOpacity = 0.4,
      popup = as.character(data[[annot_col]])
    )
}

#' HTML labels for the earthquake map
#'
#' Builds, for each earthquake, an HTML string with the cleaned location (see
#' \code{\link{eq_location_clean}}), the magnitude and the total number of
#' deaths, each with a bold heading. A value that is missing is left out of the
#' label.
#'
#' @param data A data frame with the columns \code{LOCATION_NAME},
#'   \code{EQ_PRIMARY} and \code{TOTAL_DEATHS}.
#'
#' @return A character vector with one HTML label per row of \code{data}.
#'
#' @examples
#' d <- data.frame(LOCATION_NAME = "MEXICO:  OAXACA", EQ_PRIMARY = 6.3,
#'                 TOTAL_DEATHS = 1, stringsAsFactors = FALSE)
#' eq_create_label(d)
#'
#' @export
eq_create_label <- function(data) {
  loc <- eq_location_clean(data)$LOCATION_NAME
  part <- function(title, value) {
    ifelse(is.na(value) | value == "", "",
           paste0("<b>", title, ":</b> ", value, "<br>"))
  }
  paste0(part("Location", loc),
         part("Magnitude", data$EQ_PRIMARY),
         part("Total deaths", data$TOTAL_DEATHS))
}
