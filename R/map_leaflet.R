utils::globalVariables(c(".group"))

#' Escape text for use in an HTML popup
#' @keywords internal
#' @noRd
cwp_html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Colours of the interactive map, matching the static one
#'
#' @param name `"impact"` or `"value"` (see `cwp_tile_fill_scale()`).
#' @param values The values to colour.
#' @return A leaflet palette function.
#' @keywords internal
#' @noRd
cwp_leaflet_palette <- function(name, values) {
  switch(
    name,
    impact = leaflet::colorFactor(
      palette = rev(RColorBrewer::brewer.pal(6, "PiYG")),
      levels = cwp_impact_levels, ordered = TRUE, na.color = "#CCCCCC"
    ),
    value = {
      limit <- suppressWarnings(max(abs(values), na.rm = TRUE))
      if (!is.finite(limit) || limit == 0) limit <- 1
      leaflet::colorNumeric(
        palette = c("#D73027", "#FFFFBF", "#1A9850"),
        domain = c(-limit, limit), na.color = "#CCCCCC"
      )
    },
    stop("Unknown tile fill scale: ", name)
  )
}

#' Lighten a land layer for use as the background of an interactive map
#'
#' The outline is simplified (about 0.2 degree), which is enough behind 1 and
#' 5 degree cells and keeps the report small: the layer is embedded in every
#' map.
#'
#' @param layer An `sf` or `sfc` object.
#' @return An `sfc` of multipolygons, or the original geometries if the
#'   simplification fails.
#' @keywords internal
#' @noRd
cwp_simplify_layer <- function(layer) {
  geometries <- sf::st_geometry(layer)
  tryCatch({
    # Planar simplification, with a tolerance in degrees
    s2_was_on <- suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(s2_was_on)), add = TRUE)
    light <- suppressWarnings(suppressMessages(
      sf::st_simplify(geometries, dTolerance = 0.2, preserveTopology = FALSE)
    ))
    light <- light[!sf::st_is_empty(light)]
    sf::st_cast(light, "MULTIPOLYGON")
  }, error = function(e) geometries)
}

#' Land layer drawn behind the cells of an interactive map
#'
#' @param continent An `sf`/`sfc` layer, a reference to the layer of the
#'   package (see `cwp_continent_ref()`), or `NULL` for the layer of the package.
#' @return An `sfc`, or `NULL` if no layer can be loaded.
#' @keywords internal
#' @noRd
cwp_leaflet_land <- function(continent = NULL) {
  tryCatch({
    if (is.null(continent) || inherits(continent, "cwp_default_continent")) {
      cwp_cached("continent_light", cwp_simplify_layer(cwp_default_continent()))
    } else if (inherits(continent, c("sf", "sfc"))) {
      cwp_simplify_layer(continent)
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

#' Empty interactive map with its background
#'
#' The background is the land layer, embedded in the map: it needs no internet
#' connection and no API key. Online tiles can be added on top of it with
#' `options(CWP.dataset.leaflet_provider = "Esri.OceanBasemap")` (any provider
#' name known to leaflet).
#'
#' @param land Output of `cwp_leaflet_land()`.
#' @param height Height of the map in pixels.
#' @return A leaflet widget.
#' @keywords internal
#' @noRd
cwp_leaflet_base <- function(land = NULL, height = 400) {
  # Canvas rendering: a 1 degree grid can hold tens of thousands of cells
  map <- leaflet::leaflet(width = "100%", height = height,
                          options = leaflet::leafletOptions(preferCanvas = TRUE))
  provider <- getOption("CWP.dataset.leaflet_provider", NULL)
  if (is.character(provider) && length(provider) == 1 && nzchar(provider)) {
    map <- leaflet::addProviderTiles(map, provider)
  }
  if (!is.null(land)) {
    map <- tryCatch(
      leaflet::addPolygons(map, data = land, stroke = TRUE, color = "#8C8C8C", weight = 0.5,
                           opacity = 1, fillColor = "#E0E0E0", fillOpacity = 1,
                           options = leaflet::pathOptions(interactive = FALSE)),
      error = function(e) map
    )
  }
  map
}

#' Draw CWP cells on an interactive map
#'
#' Interactive counterpart of `cwp_tile_map()`, with the same arguments: the
#' cells are drawn as rectangles on leaflet maps that can be zoomed and panned,
#' and clicking a cell shows its values.
#'
#' With `layout = "panels"` (default), each panel of the static map is its own
#' map, side by side and moving together when the {leafsync} package is
#' installed. With `layout = "layers"`, there is a single map and the panels
#' are layers, one visible at a time, chosen with a control.
#'
#' @inheritParams cwp_tile_map
#' @param continent Land layer drawn behind the cells: an `sf`/`sfc` layer, a
#'   reference to the layer of the package, or `NULL` for the layer of the
#'   package.
#' @param layout `"panels"` or `"layers"`. Default from
#'   `options(CWP.dataset.map_layout = )`.
#' @return A leaflet widget, or several of them in an HTML tag list.
#' @keywords internal
#' @noRd
cwp_leaflet_map <- function(plot_data, fill, facet_rows, facet_cols, fill_scale,
                            fill_label = fill, continent = NULL, popup_cols = NULL,
                            layout = getOption("CWP.dataset.map_layout", "panels")) {
  cwp_require_package("leaflet", "to draw interactive maps")
  if (!is.character(fill_scale)) {
    stop("An interactive map needs a named fill scale (\"impact\" or \"value\").")
  }
  plot_data <- as.data.frame(plot_data)
  values <- plot_data[[fill]]
  if (identical(fill_scale, "impact")) {
    values <- factor(as.character(values), levels = cwp_impact_levels)
  }
  palette <- cwp_leaflet_palette(fill_scale, values)

  # One group per panel of the static map. A faceting variable with a single
  # value does not need to appear in the name of the groups.
  row_values <- as.character(plot_data[[facet_rows]])
  col_values <- as.character(plot_data[[facet_cols]])
  parts <- list(row_values, col_values)
  parts <- parts[vapply(parts, function(p) length(unique(p)) > 1, logical(1))]
  plot_data$.group <- if (length(parts) == 0) "" else do.call(paste, c(parts, sep = " | "))
  # Same order as the static map: by row, then by column
  groups <- unique(plot_data$.group[order(row_values, col_values)])

  # Text shown when a cell is clicked
  popup_cols <- intersect(popup_cols, names(plot_data))
  popup <- if (length(popup_cols) == 0) NULL else {
    lines <- lapply(popup_cols, function(col) {
      value <- plot_data[[col]]
      if (is.numeric(value)) value <- prettyNum(signif(value, 6), big.mark = ",")
      paste0("<b>", cwp_html_escape(col), "</b>: ", cwp_html_escape(value))
    })
    do.call(paste, c(lines, sep = "<br>"))
  }

  land <- cwp_leaflet_land(continent)
  bounds <- c(min(plot_data$x - plot_data$width / 2), min(plot_data$y - plot_data$height / 2),
              max(plot_data$x + plot_data$width / 2), max(plot_data$y + plot_data$height / 2))

  add_cells <- function(map, group, as_layer) {
    rows <- which(plot_data$.group == group)
    cells <- plot_data[rows, , drop = FALSE]
    leaflet::addRectangles(
      map,
      lng1 = cells$x - cells$width / 2, lat1 = cells$y - cells$height / 2,
      lng2 = cells$x + cells$width / 2, lat2 = cells$y + cells$height / 2,
      stroke = FALSE, fillColor = palette(values[rows]), fillOpacity = 0.8,
      popup = if (is.null(popup)) NULL else popup[rows],
      group = if (as_layer) group else NULL
    )
  }
  finish <- function(map, legend = TRUE) {
    if (legend) {
      map <- leaflet::addLegend(map, position = "bottomright", pal = palette,
                                values = values, title = fill_label, opacity = 0.8)
    }
    leaflet::fitBounds(map, lng1 = bounds[1], lat1 = bounds[2], lng2 = bounds[3], lat2 = bounds[4])
  }

  if (length(groups) == 1 || identical(layout, "layers")) {
    map <- cwp_leaflet_base(land)
    for (group in groups) {
      map <- add_cells(map, group, as_layer = length(groups) > 1)
    }
    if (length(groups) > 1) {
      map <- leaflet::addLayersControl(
        map, baseGroups = groups,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    }
    return(finish(map))
  }

  # One map per panel; the legend is only repeated on the first one
  maps <- lapply(seq_along(groups), function(i) {
    map <- cwp_leaflet_base(land, height = 320)
    map <- add_cells(map, groups[i], as_layer = FALSE)
    map <- leaflet::addControl(
      map, position = "topright",
      html = paste0("<b>", cwp_html_escape(groups[i]), "</b>")
    )
    finish(map, legend = i == 1)
  })

  # Columns as in the static map when every combination has a panel
  n_cols <- length(unique(col_values))
  if (length(groups) != length(unique(row_values)) * n_cols) n_cols <- min(length(groups), 2)
  cwp_leaflet_panels(maps, n_cols)
}

#' Lay several interactive maps out side by side
#'
#' @param maps A list of leaflet widgets.
#' @param n_cols Number of columns.
#' @return An HTML tag list. With the {leafsync} package the maps move and zoom
#'   together; without it they are laid out in a grid and move independently.
#' @keywords internal
#' @noRd
cwp_leaflet_panels <- function(maps, n_cols = 2) {
  n_cols <- max(1, min(n_cols, length(maps)))
  if (requireNamespace("leafsync", quietly = TRUE)) {
    synced <- tryCatch(leafsync::sync(maps, ncol = n_cols), error = function(e) NULL)
    if (!is.null(synced)) return(synced)
  }
  cwp_require_package("htmltools", "to lay interactive maps out side by side")
  htmltools::browsable(htmltools::tagList(
    htmltools::div(
      style = paste0("display:grid;grid-template-columns:repeat(", n_cols, ",1fr);gap:8px;"),
      maps
    )
  ))
}

#' Should the plots of the report being rendered be interactive?
#'
#' Interactive versions only exist for HTML output. They can be switched off
#' with `options(CWP.dataset.interactive = FALSE)`.
#'
#' @return `TRUE` when rendering to HTML with interactivity enabled.
#' @keywords internal
#' @noRd
cwp_interactive_output <- function() {
  isTRUE(getOption("CWP.dataset.interactive", TRUE)) &&
    isTRUE(tryCatch(knitr::is_html_output(), error = function(e) FALSE))
}
