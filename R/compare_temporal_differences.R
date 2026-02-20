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
#' @export
#' @author
#' Bastien Grasset, \email{bastien.grasset@@ird.fr}
compare_temporal_differences <- function(parameter_time_dimension, init, final, titre_1, titre_2, unique_analyse = FALSE) {
  Groupped_all_time <- data.frame()
  for (i in parameter_time_dimension) {
    temporaire <- fonction_groupement(i, init, final)
    assign(paste0("Groupped", i), temporaire)

    Groupped_all_time <- rbind(Groupped_all_time, temporaire)
  }

  timediffplot <- lapply(parameter_time_dimension, function(filtering_unit, dataframe) {
    df_plot <- dataframe %>%
      dplyr::filter(Dimension == filtering_unit) %>%
      dplyr::mutate(Time = as.Date(Precision))

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

  }, dataframe = Groupped_all_time)


  titles <- paste0("Difference in percent of value for the dimension ", parameter_time_dimension, " for ", titre_1, " and ", titre_2, " dataset ")

  return(list(plots = timediffplot, titles = titles ))
}
