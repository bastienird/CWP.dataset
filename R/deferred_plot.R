# Deferred plots.
#
# A plot object (ggplot, cowplot grid) is heavy, embeds its data and the state
# of the plotting packages, and may not be readable any more after one of them
# is updated. The analysis functions can therefore return, instead of the plot,
# a small description of it: the name of the internal function that draws it
# and the (aggregated) tables and values it needs. The plot is only drawn when
# the report is rendered. What is saved between the two is plain data.

#' Describe a plot to be drawn later
#'
#' @param fun Name of an internal function of the package that draws the plot.
#' @param ... Its arguments: small tables and values only.
#' @return An object of class `cwp_plot_spec`.
#' @keywords internal
#' @noRd
cwp_deferred_plot <- function(fun, ...) {
  structure(list(fun = fun, args = list(...)), class = "cwp_plot_spec")
}

#' Draw a plot now, or return its description
#'
#' Both cases go through the same drawing function, so a deferred plot is
#' exactly the plot that would have been returned directly.
#'
#' @param deferred Logical. `TRUE` to return the description of the plot.
#' @inheritParams cwp_deferred_plot
#' @return A plot, or a `cwp_plot_spec` when `deferred` is `TRUE`.
#' @keywords internal
#' @noRd
cwp_plot_or_spec <- function(deferred, fun, ...) {
  spec <- cwp_deferred_plot(fun, ...)
  if (isTRUE(deferred)) spec else cwp_materialise_plot(spec)
}

#' Draw a deferred plot
#'
#' The analysis functions can return the description of a plot instead of the
#' plot itself (see the `deferred_plots` argument of
#' [comprehensive_cwp_dataframe_analysis()]). This function draws it. Anything
#' that is not a deferred plot is returned unchanged, so it can be called on
#' any plot.
#'
#' Maps have an interactive version (a leaflet widget), drawn instead of the
#' static one when `interactive` is `TRUE` and the {leaflet} package is
#' installed.
#'
#' @param x A deferred plot (class `cwp_plot_spec`), or any other object.
#' @param interactive Logical. Draw the interactive version of the plot when it
#'   has one? By default, `TRUE` when a report is being rendered to HTML and
#'   `options(CWP.dataset.interactive = FALSE)` has not been set, `FALSE`
#'   otherwise (PDF output, or outside a report).
#' @return The plot (usually a `ggplot` object, or an `htmlwidget` for an
#'   interactive map), or `x` unchanged.
#' @examples
#' cwp_materialise_plot(ggplot2::ggplot())
#' @export
cwp_materialise_plot <- function(x, interactive = cwp_interactive_output()) {
  if (!inherits(x, "cwp_plot_spec")) {
    return(x)
  }
  # Maps have an interactive version, used when it is asked for and possible;
  # otherwise, and for every other plot, the static version is drawn.
  if (isTRUE(interactive) && identical(x$fun, "cwp_tile_map") &&
      requireNamespace("leaflet", quietly = TRUE)) {
    widget <- tryCatch(do.call(cwp_leaflet_map, x$args), error = function(e) {
      warning("Interactive map failed (", conditionMessage(e), "), drawing the static map instead.")
      NULL
    })
    if (!is.null(widget)) return(widget)
  }
  fun <- get(x$fun, envir = asNamespace("CWP.dataset"), mode = "function")
  do.call(fun, x$args)
}

#' Draw every deferred plot found in a (nested) list of results
#'
#' @param x A deferred plot, a list of results, or anything else.
#' @return `x` with its deferred plots drawn. Only plain lists are explored.
#' @keywords internal
#' @noRd
cwp_materialise_plots <- function(x) {
  if (inherits(x, "cwp_plot_spec")) {
    return(cwp_materialise_plot(x, interactive = FALSE))
  }
  if (is.list(x) && !is.object(x) && length(x) > 0) {
    x[] <- lapply(x, cwp_materialise_plots)
  }
  x
}

#' Print a deferred plot
#'
#' Printing a deferred plot draws it and prints the result, so that a deferred
#' plot behaves like a plot at the console and in a report.
#'
#' @param x A deferred plot.
#' @param ... Passed to the print method of the plot.
#' @return `x`, invisibly.
#' @export
print.cwp_plot_spec <- function(x, ...) {
  print(cwp_materialise_plot(x, interactive = FALSE), ...)
  invisible(x)
}
