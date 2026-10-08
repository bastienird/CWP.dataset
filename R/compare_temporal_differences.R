#' Calculate and Visualize Temporal Data Differences
#'
#' This function calculates the differences in temporal data between two datasets
#' and provides visualizations of the differences in percent for each year.
#'
#' @param parameter_time_dimension A list of time dimensions to be analyzed.
#' @param init Data frame containing initial data.
#' @param final Data frame containing final data.
#' @param titre_1 Title for the first dataset.
#' @param titre_2 Title for the second dataset.
#' @param unique_analyse Logical value indicating whether the analysis is unique.
#'
#' @return A list containing ggplot objects for visualizing the temporal differences.
#' @examples
#' \dontrun{
#' compare_temporal_differences(c("Year"), init, final, "Dataset1", "Dataset2", FALSE, "path/to/save")
#' }
#' @import ggplot2
#' @import dplyr
#' @import tmap
#' @param deferred Logical. If `TRUE`, the plots are returned as deferred plots (their description,
#'   to be drawn with [cwp_materialise_plot()]) instead of plot objects. Default `FALSE`.
#' @export
#' @author
#' Bastien Grasset, \email{bastien.grasset@@ird.fr}
compare_temporal_differences <- function(parameter_time_dimension, init, final, titre_1, titre_2, unique_analyse = FALSE, deferred = FALSE) {
  # Do not let the returned plots keep the full datasets alive (see R/forget.R)
  on.exit(cwp_forget(c("init", "final"), environment()), add = TRUE)
  init <- data.table::as.data.table(init)
  final <- data.table::as.data.table(final)
  Groupped_all_time <- data.table::rbindlist(
    lapply(parameter_time_dimension, fonction_groupement, init = init, final = final)
  )

  timediffplot <- lapply(parameter_time_dimension, function(filtering_unit, dataframe) {
    df_plot <- dataframe %>%
      dplyr::filter(Dimension == filtering_unit) %>%
      dplyr::mutate(Time = as.Date(Precision))

    cwp_plot_or_spec(deferred, "cwp_plot_temporal_difference",
                     df_plot = as.data.frame(df_plot), filtering_unit = filtering_unit)
  }, dataframe = Groupped_all_time)


  titles <- paste0("Difference in percent of value for the dimension ", parameter_time_dimension, " for ", titre_1, " and ", titre_2, " dataset ")

  return(list(plots = timediffplot, titles = titles ))
}


#' Plot the difference in percent between two datasets over time
#'
#' @param df_plot Output of `fonction_groupement()` for one time dimension,
#'   with a `Time` column.
#' @param filtering_unit Name of the time dimension (x axis label).
#' @return A ggplot.
#' @keywords internal
#' @noRd
cwp_plot_temporal_difference <- function(df_plot, filtering_unit) {
  df_equal <- df_plot %>%
    dplyr::filter(!is.na(`Difference (in %)`), `Difference (in %)` == 0)

  ggplot(df_plot) +
    aes(x = Time, y = `Difference (in %)`) +
    # Option 3: transparency + points
    geom_line(linewidth = 0.5, alpha = 0.7) +
    geom_point(size = 1.8, alpha = 0.7) +
    # Option 4: explicit marker where the two datasets are identical
    geom_point(
      data = df_equal,
      inherit.aes = TRUE,
      shape = 21, fill = "white", color = "black",
      size = 2.8, stroke = 0.8
    ) +
    theme(legend.position = "top") +
    theme_bw() +
    labs(
      x = filtering_unit,
      caption = ifelse(
        nrow(df_equal) != 0,
        "White markers indicate identical values between the two datasets (difference = 0).",
        ""
      )
    ) +
    facet_grid(rows = vars(measurement_unit), scales = "free_y")
}
