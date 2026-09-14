plot_map <- function(data, diff_to = NULL, strokecolor = NA, fillcolor = c('#097FB3', '#A13675'),
                     alpha = 1, graticules = FALSE, zoom_to = NULL, zoom_level = NULL) {
  target_crs <- sf::st_crs(data)

  if (!is.null(zoom_level)) {
    if (is.null(zoom_to)) {
      zoom_to_xy <- sf::st_sfc(sf::st_point(c(0, 0)), crs = target_crs)
    } else {
      zoom_to_xy <- sf::st_transform(sf::st_sfc(sf::st_point(zoom_to), crs = 4326), crs = target_crs)
    }

    C <- 40075016.686   # ~ circumference of Earth in meters
    x_span <- C / 2^zoom_level
    y_span <- C / 2^(zoom_level+1)

    disp_window <- sf::st_sfc(
      sf::st_point(sf::st_coordinates(zoom_to_xy - c(x_span / 2, y_span / 2))),
      sf::st_point(sf::st_coordinates(zoom_to_xy + c(x_span / 2, y_span / 2))),
      crs = target_crs
    )

    xlim <- sf::st_coordinates(disp_window)[,'X']
    ylim <- sf::st_coordinates(disp_window)[,'Y']
  } else {
    xlim <- NULL
    ylim <- NULL
  }

  if (!is.null(diff_to)) {
    shapebase <- diff_to
    shapediff <- sf::st_sym_difference(data, diff_to)
  } else {
    shapebase <- data
    shapediff <- NULL
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = shapebase, color = strokecolor, fill = fillcolor[1], alpha = alpha)

  if (!is.null(shapediff)) {
    p <- p + ggplot2::geom_sf(data = shapediff, color = strokecolor, fill = fillcolor[2], alpha = alpha)
  }

  p +
    ggplot2::coord_sf(xlim = xlim,
             ylim = ylim,
             crs = target_crs,
             datum = ifelse(graticules, target_crs$input, NA)) +
    ggplot2::theme_bw()
}
