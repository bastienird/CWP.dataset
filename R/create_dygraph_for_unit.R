create_dygraph_for_unit <- function(data, filtering_unit, measurement_unit) {
  filtered_data <- data %>%
    dplyr::filter(Dimension == filtering_unit, measurement_unit == measurement_unit) %>%
    dplyr::mutate(Time = as.Date(Precision)) %>%
    arrange(Time) %>%  # Make sure data is sorted by time
    distinct(Time, .keep_all = TRUE)  # Ensure unique time values

  time_values <- filtered_data$Time
  diff_values <- filtered_data$`Difference (in %)`

  # Create xts object for dygraph
  diff_xts <- xts(diff_values, order.by = time_values)
  colnames(diff_xts) <- paste("Difference (in %)", filtering_unit, measurement_unit)

  # Create dygraph
  dygraph(diff_xts, main = paste("Temporal Difference (%) for", filtering_unit, "-", measurement_unit)) %>%
    dyAxis("y", label = "Difference (in %)") %>%
    dyRangeSelector() %>%
    dyLegend(show = "always")
}
