#' Create Interactive Dygraph for a Specific Unit and Measurement Type
#'
#' This function filters a dataset for a given dimension (unit) and measurement unit,
#' prepares a time series from it, and generates an interactive dygraph showing the
#' temporal difference (in percent).
#'
#' @param data A data frame containing at least the columns `Dimension`, `measurement_unit`, `Precision`, and `Difference (in %)`.
#' @param filtering_unit Character. The unit or dimension to filter on (e.g., "species", "gear_type", etc.).
#' @param measurement_unit Character. The measurement unit to filter on (e.g., "t", "no").
#'
#' @return A \code{dygraph} htmlwidget displaying the temporal difference (in %) for the selected unit and measurement.
#'
#' @importFrom xts xts
#' @importFrom dygraphs dygraph dyAxis dyRangeSelector dyLegend
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_dygraph_for_unit(my_data, "species", "t")
#' }
create_dygraph_for_unit <- function(data, filtering_unit, measurement_unit) {
  filtered_data <- data %>%
    dplyr::filter(Dimension == filtering_unit, measurement_unit == measurement_unit) %>%
    dplyr::mutate(Time = as.Date(.data$Precision)) %>%
    dplyr::arrange(Time) %>%
    dplyr::distinct(Time, .keep_all = TRUE)

  time_values <- filtered_data$Time
  diff_values <- filtered_data$`Difference (in %)`

  # Create xts object for dygraph
  diff_xts <- xts::xts(diff_values, order.by = time_values)
  colnames(diff_xts) <- paste("Difference (in %)", filtering_unit, measurement_unit)

  # Create dygraph
  dygraphs::dygraph(diff_xts, main = paste("Temporal Difference (%) for", filtering_unit, "-", measurement_unit)) %>%
    dygraphs::dyAxis("y", label = "Difference (in %)") %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyLegend(show = "always")
}
