#' Create 3D Pie Charts from Data Using plotrix
#'
#' @description This function creates 3D pie charts from measurement data for one or two datasets using plotrix.
#' @param dimension A character string indicating the dimension for grouping.
#' @param first A data frame representing the first dataset.
#' @param second An optional second data frame.
#' @param topn An integer for the number of top categories to display.
#' @param titre_1 A character string for the title of the first dataset.
#' @param titre_2 A character string for the title of the second dataset.
#' @param title_yes_no Logical indicating if a title should be displayed.
#' @param dataframe Logical indicating if a data frame should be returned.
#' @return None
#' @export
pie_chart_2_default_plotrix <- function (dimension, first, second = NULL, topn = 5, titre_1 = "first",
                                         titre_2 = "second", title_yes_no = TRUE, dataframe = FALSE)
{
  topn = 5
  first[is.na(first)] <- "NA"
  if (deparse(substitute(dimension)) == "X[[i]]") {
    r <- dimension
  } else {
    r <- deparse(substitute(dimension))
  }
  dimension <- gsub("\"", "", r)

  if (dimension == "source_authority") {
    topn = 6
  }

  name1 <- titre_1
  name2 <- titre_2
  if(is.null(second)) {
    name1 <- ""
  }

  # Summarize the data for the first dataset
  provisoire_i <- first %>%
    dplyr::group_by(dplyr::across(c(dimension, "measurement_unit"))) %>%
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
    dplyr::group_by(measurement_unit) %>%
    dplyr::arrange(desc(measurement_value)) %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::mutate(class = as.factor(ifelse(id < topn, !!rlang::sym(dimension), "Others"))) %>%
    dplyr::group_by(class, measurement_unit) %>%
    dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pourcentage = prop.table(measurement_value) * 100) %>%
    dplyr::mutate(labels = paste0(round(pourcentage), " %")) %>%
    dplyr::arrange(desc(class)) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(class))

  # Prepare for second dataset if available
  if (!is.null(second)) {
    provisoire_t <- second %>%
      dplyr::group_by(across(c(dimension, "measurement_unit"))) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
      dplyr::group_by(measurement_unit) %>%
      dplyr::arrange(desc(measurement_value)) %>%
      dplyr::mutate(id = row_number()) %>%
      dplyr::mutate(class = as.factor(ifelse(id < topn, !!rlang::sym(dimension), "Others"))) %>%
      dplyr::group_by(class, measurement_unit) %>%
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(pourcentage = prop.table(measurement_value) * 100) %>%
      dplyr::mutate(labels = paste0(round(pourcentage), " %")) %>%
      dplyr::arrange(desc(class)) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(class))
  }

  # Set up colors
  number <- length(unique(provisoire_i$class))
  pal <- brewer.pal(number, "Paired")
  pal <- setNames(pal, unique(provisoire_i$class))

  # Create a pie chart for the first dataset using plotrix
  pie_labels <- provisoire_i$labels
  pie_values <- provisoire_i$pourcentage
  pie_classes <- provisoire_i$class

  plotrix::pie3D(pie_values, labels = pie_labels, explode = 0.1,
                 main = paste("Distribution in measurement_value for the dimension:", dimension),
                 col = pal, labelcex = 0.7, radius = 0.8)

  # Plot for the second dataset if provided
  if (!is.null(second)) {
    pie_labels_t <- provisoire_t$labels
    pie_values_t <- provisoire_t$pourcentage
    pie_classes_t <- provisoire_t$class

    plotrix::pie3D(pie_values_t, labels = pie_labels_t, explode = 0.1,
                   main = paste("Distribution in measurement_value for the dimension:", dimension, " (", titre_2, ")"),
                   col = pal, labelcex = 0.7, radius = 0.8)
  }

  # If title_yes_no is TRUE, add a title using cowplot
  if (title_yes_no) {
    title <- paste0("Distribution in measurement_value for the dimension: ", r)
    if (!is.null(second)) {
      title <- paste0(title, "\nComparing ", titre_1, " and ", titre_2)
    }
    title
  }
}

