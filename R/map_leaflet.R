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

#' Draw CWP cells on an interactive map
#'
#' Interactive counterpart of `cwp_tile_map()`, with the same arguments: the
#' cells are drawn as rectangles on a leaflet map that can be zoomed and
#' panned, and clicking a cell shows its values. The panels of the static map
#' become layers, one visible at a time, chosen with a control on the map.
#'
#' The background comes from an online tile provider, so it only shows when the
#' report is read with an internet connection; the cells are always drawn.
#'
#' @inheritParams cwp_tile_map
#' @param continent Not used: the background tiles show the land.
#' @return A leaflet widget.
#' @keywords internal
#' @noRd
cwp_leaflet_map <- function(plot_data, fill, facet_rows, facet_cols, fill_scale,
                            fill_label = fill, continent = NULL, popup_cols = NULL) {
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

  # One layer per panel of the static map. A faceting variable with a single
  # value does not need to appear in the name of the layers.
  parts <- list(as.character(plot_data[[facet_rows]]), as.character(plot_data[[facet_cols]]))
  parts <- parts[vapply(parts, function(p) length(unique(p)) > 1, logical(1))]
  plot_data$.group <- if (length(parts) == 0) "" else do.call(paste, c(parts, sep = " | "))
  groups <- sort(unique(plot_data$.group))

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

  # Canvas rendering: a 1 degree grid can hold tens of thousands of cells
  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
  map <- leaflet::addProviderTiles(map, "CartoDB.Positron")

  for (group in groups) {
    rows <- which(plot_data$.group == group)
    cells <- plot_data[rows, , drop = FALSE]
    map <- leaflet::addRectangles(
      map,
      lng1 = cells$x - cells$width / 2, lat1 = cells$y - cells$height / 2,
      lng2 = cells$x + cells$width / 2, lat2 = cells$y + cells$height / 2,
      stroke = FALSE, fillColor = palette(values[rows]), fillOpacity = 0.8,
      popup = if (is.null(popup)) NULL else popup[rows],
      group = group
    )
  }
  if (length(groups) > 1) {
    map <- leaflet::addLayersControl(
      map, baseGroups = groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  }

  map <- leaflet::addLegend(map, position = "bottomright", pal = palette,
                            values = values, title = fill_label, opacity = 0.8)
  leaflet::fitBounds(
    map,
    lng1 = min(plot_data$x - plot_data$width / 2), lat1 = min(plot_data$y - plot_data$height / 2),
    lng2 = max(plot_data$x + plot_data$width / 2), lat2 = max(plot_data$y + plot_data$height / 2)
  )
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
