#' Calculate and Visualize Time Coverage
#'
#' This function calculates the time coverage for different dimensions and provides visualizations
#' of the values over time for each dataset.
#'
#' @param time_dimension_list_groupped A list of data frames, each containing time dimension data.
#' @param parameter_time_dimension The time dimension parameter.
#' @param titre_1 Title for the first dataset.
#' @param titre_2 Title for the second dataset.
#' @param unique_analyse Logical value indicating whether the analysis is unique.
#'
#' @return A list containing ggplot objects for visualizing the time coverage.
#' @examples
#' \dontrun{
#' time_coverage_analysis(time_dimension_list_groupped, "Year", "Dataset1", "Dataset2", FALSE, "path/to/save")
#' }
#' @import dplyr
#' @import ggplot2
#' @export
#' @author
#' Bastien Grasset, \email{bastien.grasset@@ird.fr}
time_coverage_analysis <- function(time_dimension_list_groupped, parameter_time_dimension, titre_1, titre_2, unique_analyse = FALSE) {
  titles_time <- if (unique_analyse) {
    paste0("Evolutions of values for the dimension ", parameter_time_dimension, " for ", titre_1, " dataset ")
  } else {
    paste0("Evolutions of values for the dimension ", parameter_time_dimension, " for ", titre_1, " and ", titre_2, " dataset ")
  }

  parse_Time_from_Precision <- function(df) {
    df %>%
      dplyr::mutate(
        Time = dplyr::case_when(
          grepl("^\\d{4}-\\d{2}-\\d{2}$", Precision) ~ as.Date(Precision),
          grepl("^\\d{4}-\\d{2}$", Precision)        ~ as.Date(paste0(Precision, "-01")),
          grepl("^\\d{2}/\\d{4}$", Precision)        ~ as.Date(paste0("01/", Precision), "%d/%m/%Y"),
          TRUE                                       ~ suppressWarnings(as.Date(Precision))
        )
      )
  }

  # pivot
  time_dimension_list_groupped_diff <- lapply(time_dimension_list_groupped, function(x) {
    x %>%
      dplyr::rename(`Values dataset 1` = "value_sum_1",
                    `Values dataset 2` = "value_sum_2") %>%
      tidyr::pivot_longer(c(`Values dataset 1`, `Values dataset 2`),
                          names_to = "Dataset", values_to = "Values") %>%
      dplyr::mutate(Dataset = dplyr::if_else(Dataset == "Values dataset 1", titre_1, titre_2)) %>%
      dplyr::distinct()
  })

  if (unique_analyse) {
    time_dimension_list_groupped_diff <- lapply(time_dimension_list_groupped_diff, \(x) dplyr::filter(x, Values != 0))
  }

  # un seul ggplot, axe conditionnel
  time_dimension_list_groupped_diff_image <- lapply(time_dimension_list_groupped_diff, function(x) {

    x2 <- x %>%
      parse_Time_from_Precision() %>%
      dplyr::filter(!is.na(Time)) %>%
      dplyr::mutate(
        is_annual  = all(lubridate::mday(Time) == 1 & lubridate::month(Time) == 1),
        is_monthly = !is_annual && all(lubridate::mday(Time) == 1),
        TimeMonth  = zoo::as.yearmon(Time),
        TimeYearD  = as.Date(paste0(lubridate::year(Time), "-01-01"))
      )

    granularity <- if (unique(x2$is_annual)) "year" else if (unique(x2$is_monthly)) "month" else "day"

    if (granularity == "year") {
      x_plot <- x2 %>% dplyr::mutate(TimeKey = TimeYearD)
      x_lab   <- "Year"
    } else if (granularity == "month") {
      x_plot <- x2 %>% dplyr::mutate(TimeKey = lubridate::floor_date(Time, "month"))
      x_lab   <- "Month"
    } else {
      x_plot <- x2 %>% dplyr::mutate(TimeKey = Time)
      x_lab   <- "Date"
    }

    x_plot <- x_plot %>%
      dplyr::group_by(Dataset, measurement_unit, TimeKey) %>%
      dplyr::summarise(Values = sum(Values, na.rm = TRUE), .groups = "drop")

    p <- ggplot2::ggplot(x_plot, ggplot2::aes(x = TimeKey, y = Values, colour = Dataset, group = Dataset)) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top") +
      ggplot2::labs(
        x = if ("Dimension" %in% names(x)) unique(x$Dimension) else x_lab,
        y = "Values"
      ) +
      ggplot2::facet_grid(rows = ggplot2::vars(measurement_unit), scales = "free_y")+ scale_x_date(labels = scales::label_date_short())


    p
  })

  list(titles = titles_time, plots = time_dimension_list_groupped_diff_image)
}
