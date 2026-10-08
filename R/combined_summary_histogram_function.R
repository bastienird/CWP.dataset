#' Create a Summary Histogram of Measurement Units by Dataset
#'
#' This function generates a histogram comparing the distribution of different measurement units
#' across two datasets. The histogram displays the percentage of each measurement unit type
#' relative to the total number of different strata in each dataset.
#'
#' @param init A data.table containing the initial dataset.
#' @param parameter_titre_dataset_1 A character string specifying the title for the initial dataset.
#'                                  Default is `"Init"`.
#' @param final A data.table containing the final dataset.
#' @param parameter_titre_dataset_2 A character string specifying the title for the final dataset.
#'                                  Default is `"Final"`.
#'
#' @return A ggplot2 histogram object displaying the percentage distribution of measurement units
#' across the two datasets.
#'
#' @details
#' - Converts both `init` and `final` datasets to `data.table`.
#' - Counts the number of unique strata for each measurement unit.
#' - Computes the percentage of each measurement unit within each dataset.
#' - Creates a histogram with stacked bars, displaying the percentage of each measurement unit.
#' - Uses a consistent color palette for different measurement units.
#'
#' @import ggplot2
#' @import data.table
#' @importFrom scales hue_pal percent_format
#' @param deferred Logical. If `TRUE`, the plot is returned as a deferred plot (its description,
#'   to be drawn with [cwp_materialise_plot()]) instead of a plot object. Default `FALSE`.
#' @export
combined_summary_histogram_function <- function(init, parameter_titre_dataset_1 = "Init",
                                                final, parameter_titre_dataset_2 = "Final",
                                                deferred = FALSE) {
  # Do not let the returned plots keep the full datasets alive (see R/forget.R)
  on.exit(cwp_forget(c("init", "final"), environment()), add = TRUE)
  # Convertir en data.table
  setDT(init)
  summary_number_row_init <- init[, .(Number_different_stratas = .N), by = "measurement_unit"]
  summary_number_row_init[, data_source := parameter_titre_dataset_1]

  setDT(final)
  summary_number_row_final <- final[, .(Number_different_stratas = .N), by = "measurement_unit"]
  summary_number_row_final[, data_source := parameter_titre_dataset_2]

  # Combiner les résumés
  combined_summary <- rbind(summary_number_row_init, summary_number_row_final)
  combined_summary[, Percent := Number_different_stratas / sum(Number_different_stratas) * 100, by = data_source]

  # Calcul des totaux pour chaque dataset
  total_rows <- combined_summary[, .(Total_rows = sum(Number_different_stratas)), by = data_source]
  combined_summary <- merge(combined_summary, total_rows, by = "data_source")
  unique_units <- unique(combined_summary$measurement_unit)
  color_palette <- scales::hue_pal()(length(unique_units))
  names(color_palette) <- unique_units

  combined_summary_histogram <- cwp_plot_or_spec(
    deferred, "cwp_plot_combined_summary",
    combined_summary = as.data.frame(combined_summary), color_palette = color_palette
  )

  return(combined_summary_histogram)
}

#' Plot the share of each measurement unit in the number of strata, by dataset
#'
#' @param combined_summary Table with columns `data_source`, `Percent` and
#'   `measurement_unit`.
#' @param color_palette Named vector of colours, one per measurement unit.
#' @return A ggplot.
#' @keywords internal
#' @noRd
cwp_plot_combined_summary <- function(combined_summary, color_palette) {
  # Créer le graphique principal (histogramme)
  ggplot(combined_summary,
         aes(x = factor(data_source), y = Percent, fill = measurement_unit)) +
    geom_bar(stat = "identity", position = "fill") +  # Barres empilées avec échelle à 100%
    scale_fill_manual(values = color_palette) +
    scale_y_continuous(labels = scales::percent_format()) +
    geom_text(aes(label = paste0(round(Percent, 1), "%")),
              position = position_fill(vjust = 0.5), color = "black") + # Texte à l'intérieur des barres
    labs(title = "Distribution of number strata for each measurement_unit by dataset",
         x = "Dataset",
         y = "Percentage",
         fill = "Measurement unit") +
    theme_minimal()
}
