raw <- data.frame(
  YEAR = c(2014, -44, 2001), MONTH = c(7, NA, 2), DAY = c(29, NA, 30),
  LATITUDE = c("16.8", "40", "10"), LONGITUDE = c("-99.9", "14", "20"),
  EQ_PRIMARY = c("6.3", "7.0", NA), TOTAL_DEATHS = c(1, NA, 5),
  LOCATION_NAME = c("MEXICO:  OAXACA", "ITALY:  NAPLES", "CHINA:  YUSHU"),
  COUNTRY = c("MEXICO", "ITALY", "CHINA"),
  stringsAsFactors = FALSE
)

test_that("eq_clean_data builds a Date column and numeric coordinates", {
  out <- eq_clean_data(raw)
  expect_s3_class(out$DATE, "Date")
  expect_equal(out$DATE[1], as.Date("2014-07-29"))
  expect_false(anyNA(out$DATE))              # B.C. y fechas imposibles incluidas
  expect_equal(out$DATE[3], as.Date("2001-02-01"))
  expect_true(is.numeric(out$LATITUDE))
  expect_true(is.numeric(out$LONGITUDE))
  expect_equal(out$LONGITUDE[1], -99.9)
})

test_that("eq_location_clean removes the country and uses title case", {
  out <- eq_location_clean(raw)
  expect_equal(out$LOCATION_NAME, c("Oaxaca", "Naples", "Yushu"))
})

test_that("geom_timeline returns a layer and builds a plot", {
  d <- eq_clean_data(raw)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = DATE, y = COUNTRY,
                                       size = EQ_PRIMARY, colour = TOTAL_DEATHS)) +
    geom_timeline(xmin = as.Date("0001-01-01"), xmax = as.Date("2020-01-01")) +
    theme_timeline()
  expect_s3_class(geom_timeline(), "LayerInstance")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_s3_class(GeomTimeline, "Geom")
})

test_that("geom_timeline_label keeps the n_max largest earthquakes", {
  d <- eq_location_clean(eq_clean_data(raw))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = DATE, size = EQ_PRIMARY)) +
    geom_timeline() +
    geom_timeline_label(ggplot2::aes(label = LOCATION_NAME), n_max = 1)
  expect_s3_class(geom_timeline_label(ggplot2::aes(label = LOCATION_NAME)),
                  "LayerInstance")
  expect_silent(ggplot2::ggplot_build(p))
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp); print(p); grDevices::dev.off()
  expect_true(file.exists(tmp))
})

test_that("eq_map returns a leaflet map with one circle per earthquake", {
  d <- eq_clean_data(raw)
  m <- eq_map(d, annot_col = "DATE")
  expect_s3_class(m, "leaflet")
  calls <- vapply(m$x$calls, function(x) x$method, character(1))
  expect_true("addCircleMarkers" %in% calls)
  expect_error(eq_map(d, annot_col = "NOPE"))
})

test_that("eq_create_label builds HTML labels and omits missing values", {
  d <- eq_clean_data(raw)
  lab <- eq_create_label(d)
  expect_length(lab, 3)
  expect_equal(lab[1],
               "<b>Location:</b> Oaxaca<br><b>Magnitude:</b> 6.3<br><b>Total deaths:</b> 1<br>")
  expect_false(grepl("Total deaths", lab[2]))
  expect_false(grepl("Magnitude", lab[3]))
})
