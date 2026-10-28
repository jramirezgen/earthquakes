#' @importFrom magrittr %>%
NULL

#' Clean the NOAA earthquake data
#'
#' Takes the raw NOAA Significant Earthquake Database (as a data frame) and
#' returns a clean version with a \code{DATE} column of class
#' \code{\link[base]{Date}} (built from \code{YEAR}, \code{MONTH} and
#' \code{DAY}) and numeric \code{LATITUDE}, \code{LONGITUDE}, \code{EQ_PRIMARY}
#' and \code{TOTAL_DEATHS} columns. A missing month or day is replaced by 1;
#' negative years (B.C.) are supported.
#'
#' @param data A data frame with the raw NOAA data, with at least the columns
#'   \code{YEAR}, \code{MONTH}, \code{DAY}, \code{LATITUDE} and
#'   \code{LONGITUDE}.
#'
#' @return A data frame (tibble if the input was one) with the added column
#'   \code{DATE} and the numeric columns described above.
#'
#' @examples
#' raw <- data.frame(YEAR = c(2014, -44), MONTH = c(7, NA), DAY = c(29, NA),
#'                   LATITUDE = c("16.8", "40"), LONGITUDE = c("-99.9", "14"),
#'                   stringsAsFactors = FALSE)
#' eq_clean_data(raw)
#'
#' @export
eq_clean_data <- function(data) {
  month <- ifelse(is.na(data$MONTH), 1, data$MONTH)
  day <- ifelse(is.na(data$DAY), 1, data$DAY)
  date <- lubridate::make_date(data$YEAR, month, day)
  # fechas imposibles (p. ej. 30 de febrero): se cae al dia 1 del mes
  bad <- is.na(date) & !is.na(data$YEAR)
  date[bad] <- lubridate::make_date(data$YEAR[bad], month[bad], 1)

  data$DATE <- date
  data$LATITUDE <- as.numeric(data$LATITUDE)
  data$LONGITUDE <- as.numeric(data$LONGITUDE)
  if ("EQ_PRIMARY" %in% names(data))
    data$EQ_PRIMARY <- as.numeric(data$EQ_PRIMARY)
  if ("TOTAL_DEATHS" %in% names(data))
    data$TOTAL_DEATHS <- as.numeric(data$TOTAL_DEATHS)
  data
}

#' Clean the earthquake location names
#'
#' Removes the country name (and the colon) from the \code{LOCATION_NAME}
#' column and converts the rest to title case, e.g.
#' \code{"MEXICO:  OAXACA"} becomes \code{"Oaxaca"}.
#'
#' @param data A data frame with a \code{LOCATION_NAME} column.
#'
#' @return The same data frame with a cleaned \code{LOCATION_NAME} column.
#'
#' @examples
#' d <- data.frame(LOCATION_NAME = c("MEXICO:  OAXACA", "JAPAN:  TOKYO"),
#'                 stringsAsFactors = FALSE)
#' eq_location_clean(d)
#'
#' @export
eq_location_clean <- function(data) {
  loc <- as.character(data$LOCATION_NAME)
  loc <- stringr::str_replace(loc, "^[^:]*:\\s*", "")
  data$LOCATION_NAME <- stringr::str_to_title(stringr::str_trim(loc))
  data
}
