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
#' @param fill_scale A ggplot2 fill scale, or the name of a predefined one
#'   (`"impact"` or `"value"`, see `cwp_tile_fill_scale()`). A name keeps the
#'   description of a deferred map free of ggplot2 objects.
#' @param fill_label Legend title.
#' @param continent Optional `sf`/`sfc` layer drawn as borders on every panel,
#'   or a reference to the continent layer of the package (see
#'   `cwp_continent_ref()`).
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
cwp_tile_map <- function(plot_data, fill, facet_rows, facet_cols, fill_scale,
                         fill_label = fill, continent = NULL) {
  if (is.character(fill_scale)) fill_scale <- cwp_tile_fill_scale(fill_scale)
  if (inherits(continent, "cwp_default_continent")) continent <- cwp_default_continent()

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

# Categories of the map of differences between two datasets, best to worst
cwp_impact_levels <- c("Appearing data", "Gain (more than double)", "Gain",
                       "No differences", "Loss", "All data lost")

#' Predefined fill scales of the tile maps
#'
#' @param name `"impact"` (categories of difference between two datasets) or
#'   `"value"` (measurement values, diverging around 0).
#' @return A ggplot2 fill scale.
#' @keywords internal
#' @noRd
cwp_tile_fill_scale <- function(name) {
  switch(
    name,
    impact = {
      palette <- rev(RColorBrewer::brewer.pal(6, "PiYG"))
      names(palette) <- cwp_impact_levels
      ggplot2::scale_fill_manual(values = palette, drop = FALSE, na.value = "grey80")
    },
    value = ggplot2::scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF",
                                          high = "#1A9850", midpoint = 0),
    stop("Unknown tile fill scale: ", name)
  )
}

#' Continent layer shipped with the package
#'
#' Read once per session.
#'
#' @return An `sf` object (CRS EPSG:4326).
#' @keywords internal
#' @noRd
cwp_default_continent <- function() {
  cwp_cached("continent", {
    rds_file <- system.file("extdata", "continent.rds", package = "CWP.dataset")
    continent <- if (nzchar(rds_file)) {
      readRDS(rds_file)
    } else {
      # Layer not converted yet: see data-raw/convert_continent.R
      cwp_read_legacy_qs(system.file("extdata", "continent.qs", package = "CWP.dataset"))
    }
    sf::st_crs(continent) <- 4326
    continent
  })
}

#' Replace the continent layer of the package by a reference to it
#'
#' The continent layer is by far the largest input of a map. When it is the one
#' shipped with the package, the description of a deferred map only keeps a
#' reference to it, resolved when the map is drawn, instead of one copy per map.
#' Any other layer is kept as it is.
#'
#' @param continent An `sf`/`sfc` layer, or `NULL`.
#' @return `continent`, or an object of class `cwp_default_continent`.
#' @keywords internal
#' @noRd
cwp_continent_ref <- function(continent) {
  is_default <- inherits(continent, "sf") &&
    isTRUE(tryCatch(identical(continent, cwp_default_continent()), error = function(e) FALSE))
  if (is_default) structure(list(), class = "cwp_default_continent") else continent
}

#' Arguments of `cwp_tile_map()` for a map, reduced to what it draws
#'
#' @inheritParams cwp_tile_map
#' @return A list of arguments for `cwp_tile_map()`.
#' @keywords internal
#' @noRd
cwp_tile_map_args <- function(plot_data, fill, facet_rows, facet_cols, fill_scale, continent = NULL) {
  columns <- unique(c("x", "y", "width", "height", fill, facet_rows, facet_cols))
  list(
    plot_data = as.data.frame(plot_data)[, columns, drop = FALSE],
    fill = fill, facet_rows = facet_rows, facet_cols = facet_cols,
    fill_scale = fill_scale, continent = cwp_continent_ref(continent)
  )
}

#' Continent layer, from the database when there is one
#'
#' @param con Optional DBI connection. When valid, the layer is read from
#'   `public.continent`; otherwise, or if that fails, the layer shipped with the
#'   package is returned.
#' @return An `sf` object (CRS EPSG:4326).
#' @keywords internal
#' @noRd
cwp_continent_layer <- function(con = NULL) {
  if (!is.null(con) && isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE))) {
    from_db <- try(sf::st_read(con, query = "SELECT * FROM public.continent", quiet = TRUE),
                   silent = TRUE)
    if (!inherits(from_db, "try-error")) {
      sf::st_crs(from_db) <- 4326
      return(from_db)
    }
  }
  cwp_default_continent()
}
