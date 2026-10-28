#' Time line of earthquakes
#'
#' \code{geom_timeline} draws a time line of earthquakes: a horizontal line from
#' \code{xmin} to \code{xmax} with one point per earthquake. If a factor is
#' mapped to \code{y} (for example the country), one time line is drawn per
#' level.
#'
#' @inheritParams ggplot2::layer
#' @param xmin,xmax Optional dates (class \code{Date} or anything coercible
#'   with \code{as.Date}) that limit the earthquakes drawn. By default the whole
#'   range of the data.
#' @param na.rm If \code{FALSE}, missing values are removed with a warning.
#' @param ... Other arguments passed on to the layer, for example fixed values
#'   of the aesthetics.
#'
#' @section Aesthetics:
#' Required: \code{x} (a date). Optional: \code{y} (a factor that stratifies the
#' time lines), \code{colour} (for example the number of deaths), \code{size}
#' (for example the magnitude), \code{alpha}.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' d <- data.frame(DATE = as.Date("2000-01-01") + c(0, 400, 900, 1500),
#'                 EQ_PRIMARY = c(5, 6, 7, 6.5),
#'                 TOTAL_DEATHS = c(1, 10, 100, 5))
#' ggplot2::ggplot(d, ggplot2::aes(x = DATE, size = EQ_PRIMARY,
#'                                 colour = TOTAL_DEATHS)) +
#'   geom_timeline(alpha = 0.5) +
#'   theme_timeline()
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE,
                          xmin = NULL, xmax = NULL, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, xmin = xmin, xmax = xmax, ...)
  )
}

#' @rdname geom_timeline
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline", ggplot2::Geom,
  required_aes = c("x"),
  optional_aes = c("y"),
  default_aes = ggplot2::aes(y = 0.5, colour = "grey30", size = 3, alpha = 0.5,
                             shape = 19, stroke = 0.5),
  draw_key = ggplot2::draw_key_point,

  setup_data = function(data, params) {
    if (!is.null(params$xmin))
      data <- data[data$x >= as.numeric(as.Date(params$xmin)), , drop = FALSE]
    if (!is.null(params$xmax))
      data <- data[data$x <= as.numeric(as.Date(params$xmax)), , drop = FALSE]
    data
  },

  draw_panel = function(data, panel_params, coord, xmin = NULL, xmax = NULL) {
    coords <- coord$transform(data, panel_params)
    # sin magnitud conocida el punto se dibuja pequeno en vez de fallar
    coords$size[is.na(coords$size)] <- 1

    # extremos de cada linea: xmin/xmax o el rango de todos los datos
    lo <- if (is.null(xmin)) min(coords$x) else
      coord$transform(data.frame(x = as.numeric(as.Date(xmin)), y = data$y[1]),
                      panel_params)$x
    hi <- if (is.null(xmax)) max(coords$x) else
      coord$transform(data.frame(x = as.numeric(as.Date(xmax)), y = data$y[1]),
                      panel_params)$x

    ys <- unique(coords$y)
    lines <- grid::segmentsGrob(
      x0 = grid::unit(rep(lo, length(ys)), "npc"),
      x1 = grid::unit(rep(hi, length(ys)), "npc"),
      y0 = grid::unit(ys, "npc"), y1 = grid::unit(ys, "npc"),
      gp = grid::gpar(col = "grey50", lwd = 1)
    )
    points <- grid::pointsGrob(
      coords$x, coords$y, pch = coords$shape,
      gp = grid::gpar(
        col = scales::alpha(coords$colour, coords$alpha),
        fill = scales::alpha(coords$colour, coords$alpha),
        fontsize = coords$size * ggplot2::.pt + coords$stroke * ggplot2::.stroke / 2
      )
    )
    grid::gTree(children = grid::gList(lines, points))
  }
)

#' Theme for the earthquake time line
#'
#' A minimal \pkg{ggplot2} theme: only the bottom axis is drawn, the legend goes
#' at the bottom and the y axis is hidden (except its labels when a factor is
#' mapped to \code{y}).
#'
#' @return A ggplot2 theme.
#'
#' @examples
#' ggplot2::ggplot(data.frame(x = Sys.Date()), ggplot2::aes(x, 1)) +
#'   ggplot2::geom_point() + theme_timeline()
#'
#' @export
theme_timeline <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "bottom",
      panel.background = ggplot2::element_blank()
    )
}

#' Labels for the earthquake time line
#'
#' \code{geom_timeline_label} adds a vertical line and a text annotation (for
#' example the location) to the largest earthquakes of a time line made with
#' \code{\link{geom_timeline}}.
#'
#' @inheritParams ggplot2::layer
#' @param n_max Maximum number of earthquakes to label per time line: those with
#'   the largest magnitude (the \code{size} aesthetic). By default all.
#' @param na.rm If \code{FALSE}, missing values are removed with a warning.
#' @param ... Other arguments passed on to the layer.
#'
#' @section Aesthetics:
#' Required: \code{x} (a date) and \code{label} (the column the annotations are
#' taken from). Optional: \code{y} (the stratifying factor) and \code{size}
#' (the magnitude, used to choose the \code{n_max} largest earthquakes).
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' d <- data.frame(DATE = as.Date("2000-01-01") + c(0, 400, 900, 1500),
#'                 EQ_PRIMARY = c(5, 6, 7, 6.5),
#'                 LOCATION_NAME = c("A", "B", "C", "D"))
#' ggplot2::ggplot(d, ggplot2::aes(x = DATE, size = EQ_PRIMARY)) +
#'   geom_timeline() +
#'   geom_timeline_label(ggplot2::aes(label = LOCATION_NAME), n_max = 2) +
#'   theme_timeline()
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE,
                                n_max = NULL, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}

#' @rdname geom_timeline_label
#' @format NULL
#' @usage NULL
#' @export
GeomTimelineLabel <- ggplot2::ggproto(
  "GeomTimelineLabel", ggplot2::Geom,
  required_aes = c("x", "label"),
  optional_aes = c("y", "size"),
  default_aes = ggplot2::aes(y = 0.5, size = 1),

  draw_panel = function(data, panel_params, coord, n_max = NULL) {
    ngroups <- length(unique(data$y))
    if (!is.null(n_max)) {
      data <- do.call(rbind, lapply(split(data, data$y), function(d)
        utils::head(d[order(-d$size), , drop = FALSE], n_max)))
    }
    coords <- coord$transform(data, panel_params)
    height <- 0.25 / max(ngroups, 1)

    lines <- grid::segmentsGrob(
      x0 = grid::unit(coords$x, "npc"), x1 = grid::unit(coords$x, "npc"),
      y0 = grid::unit(coords$y, "npc"),
      y1 = grid::unit(coords$y + height, "npc"),
      gp = grid::gpar(col = "grey50", lwd = 0.7)
    )
    text <- grid::textGrob(
      label = coords$label,
      x = grid::unit(coords$x, "npc"),
      y = grid::unit(coords$y + height, "npc"),
      rot = 45, just = "left", gp = grid::gpar(fontsize = 8)
    )
    grid::gTree(children = grid::gList(lines, text))
  }
)
