utils::globalVariables(c("X", "Y", "feature", "x", "y", "width", "height", ".fill",
                         "xmin", "xmax", "ymin", "ymax", "rectangle"))

#' Describe CWP grid cells as tiles (centre, width, height)
#'
#' CWP grid cells are rectangles, so a map does not need one polygon per cell:
#' a centre and a size are enough and are much faster to draw. Only the cells
#' listed in `codes` are processed.
#'
#' @param shape An `sf` object of the CWP grid, with a `cwp_code` (or `code`) column.
#' @param codes Character vector of the CWP codes to describe.
#' @return A `data.table` with columns `code`, `x`, `y`, `width`, `height`, or
#'   `NULL` when the cells cannot be described as tiles (not an `sf` object, no
#'   code column, or cells that are not simple rectangles).
#' @keywords internal
#' @noRd
cwp_grid_tiles <- function(shape, codes) {
  if (!inherits(shape, "sf")) return(NULL)
  code_col <- intersect(c("cwp_code", "code", "CWP_CODE"), names(shape))
  if (length(code_col) == 0) return(NULL)

  shape_codes <- as.character(shape[[code_col[1]]])
  keep <- which(shape_codes %in% unique(as.character(codes)) & !duplicated(shape_codes))
  if (length(keep) == 0) {
    return(data.table::data.table(code = character(), x = numeric(), y = numeric(),
                                  width = numeric(), height = numeric()))
  }

  coords <- tryCatch(sf::st_coordinates(sf::st_geometry(shape)[keep]), error = function(e) NULL)
  if (is.null(coords) || nrow(coords) == 0) return(NULL)

  # The last column of st_coordinates() is the index of the feature
  coords <- data.table::data.table(feature = coords[, ncol(coords)],
                                   X = coords[, "X"], Y = coords[, "Y"])
  extent <- coords[, .(
    xmin = min(X), xmax = max(X), ymin = min(Y), ymax = max(Y),
    rectangle = .N == 5L &&
      all((X == min(X) | X == max(X)) & (Y == min(Y) | Y == max(Y)))
  ), by = feature]
  if (nrow(extent) != length(keep) || !all(extent$rectangle)) return(NULL)

  data.table::data.table(
    code   = shape_codes[keep][extent$feature],
    x      = (extent$xmin + extent$xmax) / 2,
    y      = (extent$ymin + extent$ymax) / 2,
    width  = extent$xmax - extent$xmin,
    height = extent$ymax - extent$ymin
  )
}

#' Draw a faceted map of CWP cells as tiles
#'
#' @param plot_data A data.frame with the tile columns (`x`, `y`, `width`,
#'   `height`) and the columns named in `fill`, `facet_rows`, `facet_cols`.
#' @param fill,facet_rows,facet_cols Column names.
#' @param fill_scale A ggplot2 fill scale.
#' @param fill_label Legend title.
#' @param continent Optional `sf`/`sfc` layer drawn as borders on every panel.
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
cwp_tile_map <- function(plot_data, fill, facet_rows, facet_cols, fill_scale,
                         fill_label = fill, continent = NULL) {
  plot_data <- as.data.frame(plot_data)
  plot_data$.fill <- plot_data[[fill]]
  plot_data$.facet_row <- plot_data[[facet_rows]]
  plot_data$.facet_col <- plot_data[[facet_cols]]

  xlim <- c(min(plot_data$x - plot_data$width / 2), max(plot_data$x + plot_data$width / 2))
  ylim <- c(min(plot_data$y - plot_data$height / 2), max(plot_data$y + plot_data$height / 2))

  map <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_tile(ggplot2::aes(x = x, y = y, width = width, height = height, fill = .fill)) +
    fill_scale +
    ggplot2::facet_grid(.facet_row ~ .facet_col) +
    ggplot2::labs(x = NULL, y = NULL, fill = fill_label) +
    ggplot2::theme_bw()

  if (inherits(continent, "sfc")) continent <- sf::st_sf(geometry = continent)
  if (inherits(continent, "sf")) {
    map <- map +
      ggplot2::geom_sf(data = continent, fill = NA, colour = "grey30",
                       linewidth = 0.2, inherit.aes = FALSE) +
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
  } else {
    map <- map + ggplot2::coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE)
  }
  map
}
