#' Comprehensive Analysis of Initial and Final Datasets
#'
#' This function performs a detailed comparative analysis between initial and final datasets.
#' It includes:
#' - A summary of differences
#' - Grouping differences
#' - Dimension comparisons (temporal, spatial, and categorical)
#' - Visualization options
#'
#' @param parameter_init Data frame. The initial dataset.
#' @param parameter_final Data frame. The final dataset.
#' @param fig.path Character. Path to save output figures. Default is the working directory.
#' @param parameter_fact Character. Fact parameter, default is `"catch"`.
#' @param parameter_short Logical. Whether to use short parameter names. Default is `FALSE`.
#' @param parameter_columns_to_keep Character vector. Columns to retain for comparison.
#' @param parameter_diff_value_or_percent Character. Difference calculation method: `"Difference (in %)"` (default) or `"Difference in value"`.
#' @param parameter_UNK_for_not_standards_unit Logical. Whether to use `"UNK"` for non-standard measurement units. Default is `TRUE`.
#' @param parameter_filtering List. Filtering parameters for species and fishing fleet. Default is `list(species = NULL, fishing_fleet = NULL)`.
#' @param parameter_time_dimension Character vector. Time-related columns to include. Default is `c("time_start")`.
#' @param parameter_geographical_dimension Character. Column name for geographic identifiers. Default is `"geographic_identifier"`.
#' @param parameter_geographical_dimension_groupping Character. Grouping column for geographical dimensions. Default is `"GRIDTYPE"`.
#' @param parameter_colnames_to_keep Character or `"all"`. Column names to retain. Default is `"all"`.
#' @param outputonly Logical. Whether to return only the analysis output without visualization. Default is `FALSE`.
#' @param plotting_type Character. Type of visualization (`"view"` by default).
#' @param print_map Logical. Whether to print the map visualization. Default is `TRUE`.
#' @param shapefile_fix Object. Optional shapefile for spatial analysis. Default is `NULL`.
#' @param continent Character. Optional filter for a specific continent. Default is `NULL`.
#' @param coverage Logical. Whether to analyze time, geographic, and other dimensions. If `FALSE`, only a summary is performed. Default is `TRUE`.
#' @param parameter_resolution_filter Object. Resolution filtering parameter. Default is `NULL`.
#' @param parameter_titre_dataset_1 Character. Title for dataset 1 in outputs. Default is `"Dataset 1"`.
#' @param parameter_titre_dataset_2 Character. Title for dataset 2 in outputs. Default is `"Dataset 2"`.
#' @param unique_analyse Logical. Whether the analysis is unique. Default is `FALSE`.
#' @param removemap Logical. Whether to remove the map from outputs. Default is `FALSE`.
#' @param topnumber Integer. Number of top characteristics to display without grouping. Default is `6`.
#'
#' @return A list containing:
#' - **Summary of dataset differences**
#' - **Grouped differences**
#' - **Comparisons across time, space, and other dimensions**
#' - **Optional visualizations**
#'
#' @export
#'
#' @import ggplot2
#' @import data.table
#' @import gridExtra
comprehensive_cwp_dataframe_analysis <- function(parameter_init, parameter_final,
                                                 fig.path = getwd(),
                                                 parameter_fact = "catch",
                                                 parameter_short = FALSE,
                                                 parameter_columns_to_keep = c("Precision", "measurement_unit", "Values dataset 1",
                                                                               "Values dataset 2", "Loss / Gain",
                                                                               "Difference (in %)", "Dimension",
                                                                               "Difference in value"),
                                                 parameter_diff_value_or_percent = "Difference (in %)",
                                                 parameter_UNK_for_not_standards_unit = TRUE,
                                                 parameter_filtering = list(species = NULL, fishing_fleet = NULL),
                                                 parameter_time_dimension = c("time_start"),
                                                 parameter_geographical_dimension = "geographic_identifier",
                                                 parameter_geographical_dimension_groupping = "GRIDTYPE",
                                                 parameter_colnames_to_keep = "all",
                                                 outputonly = FALSE,
                                                 plotting_type = "view",
                                                 print_map = TRUE,
                                                 shapefile_fix = NULL,
                                                 continent = NULL,
                                                 coverage = TRUE,
                                                 parameter_resolution_filter = NULL,
                                                 parameter_titre_dataset_1 = "Dataset 1",
                                                 parameter_titre_dataset_2 = "Dataset 2",
                                                 unique_analyse = FALSE,
                                                 removemap = FALSE, topnumber = 6) {
  # Process 'parameter_init'
  if (is.character(parameter_init)) {
    init <- read_data(parameter_init) %>%
      dplyr::filter(!is.na(measurement_value))#%>% head(10000)
  } else if (is.data.frame(parameter_init)) {
    init <- parameter_init%>%
      dplyr::filter(!is.na(measurement_value))
  } else {
    stop("Invalid 'parameter_init'")
  }

  # Process 'parameter_final'
  if (unique_analyse) {
    final <- init[0, ]
  } else {
    if (is.character(parameter_final)) {
      final <- read_data(parameter_final)%>%
        dplyr::filter(!is.na(measurement_value))
    } else if (is.data.frame(parameter_final)) {
      final <- parameter_final%>%
        dplyr::filter(!is.na(measurement_value))
    } else {
      stop("Invalid 'parameter_final'")
    }
  }
  if(!print_map | parameter_geographical_dimension_groupping %notin% colnames(init)){
    init <- init %>% dplyr::mutate(GRIDTYPE = "GRIDTYPE")
    final <- final %>% dplyr::mutate(GRIDTYPE = "GRIDTYPE")
  }
  if (is_null_or_not_exist(parameter_titre_dataset_2) & !unique_analyse) {
    if(!is.data.frame(parameter_final)) {
      parameter_titre_dataset_2 <- last_path_reduced(as.character(parameter_final))
    } else {
      parameter_titre_dataset_2 <- "Dataset 2"
    }
  } else if (unique_analyse) {
    parameter_titre_dataset_2 <- "NONE"
  } else {
    parameter_titre_dataset_2 <- parameter_titre_dataset_2
  }

  parameter_titre_dataset_2 <- gsub("_", "-", parameter_titre_dataset_2)
  parameter_titre_dataset_1 <- gsub("_", "-", parameter_titre_dataset_1)

  #cat("Tidying data...\n")
  if(length(parameter_colnames_to_keep) ==1 &&  parameter_colnames_to_keep == "all") {
    parameter_colnames_to_keep <- colnames(init)
  }
  parameter_colnames_to_keep <- unique(c(parameter_colnames_to_keep, parameter_geographical_dimension_groupping, parameter_geographical_dimension, parameter_time_dimension))

  init <- CWP.dataset::tidying_data(init, parameter_colnames_to_keep_dataframe = parameter_colnames_to_keep, time_dimension = parameter_time_dimension)
  final <- CWP.dataset::tidying_data(final, parameter_colnames_to_keep_dataframe = parameter_colnames_to_keep, time_dimension = parameter_time_dimension)

  colnames_intersect <- intersect(colnames(init), colnames(final))

  init <- init %>% dplyr::select(colnames_intersect)
  final <- final %>% dplyr::select(colnames_intersect)

  #cat("Renaming geographic identifiers and handling non-standard units...\n")
  init <- CWP.dataset::function_geographic_identifier_renaming_and_not_standards_unit(
    init,
    geo_dim = parameter_geographical_dimension,
    parameter_fact = parameter_fact,
    parameter_UNK_for_not_standards_unit = parameter_UNK_for_not_standards_unit,
    geo_dim_group = "GRIDTYPE"
  )

  final <- CWP.dataset::function_geographic_identifier_renaming_and_not_standards_unit(
    final,
    geo_dim = parameter_geographical_dimension,
    parameter_fact = parameter_fact,
    parameter_UNK_for_not_standards_unit = parameter_UNK_for_not_standards_unit,
    geo_dim_group = "GRIDTYPE"
  )
  #cat("Applying filtering function...\n")
  init <- CWP.dataset::filtering_function(init, parameter_filtering = parameter_filtering)

  if(nrow(init) == 0){
    stop("Filtering has removed all rows")
  }

  if (unique_analyse) {
    final <- init[0,]
  } else {
    final <- filtering_function(final)
  }

  # Ensure all necessary variables exist and set default values
  if (!exists("fig.path")) {
    fig.path <- getwd()
  }
  #cat("Grouping differences...\n")
  groupping_differences_list <- CWP.dataset::groupping_differences(init, final, parameter_time_dimension, parameter_geographical_dimension, parameter_geographical_dimension_groupping)

  Groupped_all <- groupping_differences_list$Groupped_all
  Other_dimensions <- groupping_differences_list$Other_dimensions
  Other_dimensions <- Other_dimensions[Other_dimensions != "time_end"]

  time_dimension_list_groupped <- groupping_differences_list$Groupped_time_dimension

  GrouppedGRIDTYPE <- groupping_differences_list$GrouppedGRIDTYPE

  if (!unique_analyse) {
    summary_of_differences <- CWP.dataset::compute_summary_of_differences(init, final, parameter_titre_dataset_1, parameter_titre_dataset_2)
    compare_strata_differences_list <- CWP.dataset::compare_strata_differences(init, final, Groupped_all, parameter_titre_dataset_1, parameter_titre_dataset_2, parameter_columns_to_keep, unique_analyse)
    compare_strata_differences_list$title <- paste0("Disappearing or appearing strata between ", parameter_titre_dataset_1, " and ", parameter_titre_dataset_2)


    compare_dimension_differences_list <- CWP.dataset::compare_dimension_differences(Groupped_all, Other_dimensions, parameter_diff_value_or_percent, parameter_columns_to_keep, topn = topnumber)
    compare_dimension_differences_list$title <-paste0("Difference between the non appearing/disappearing stratas between ", parameter_titre_dataset_1, " and ", parameter_titre_dataset_2)

    if (length(parameter_time_dimension) != 0) {
      plot_titles_list <- CWP.dataset::compare_temporal_differences(parameter_time_dimension, init, final, parameter_titre_dataset_1, parameter_titre_dataset_2, unique_analyse = FALSE)
    }

    if (length(parameter_geographical_dimension) != 0 && print_map) {
      Geographicdiff <- CWP.dataset::geographic_diff(init, final, shapefile_fix, parameter_geographical_dimension, parameter_geographical_dimension_groupping, continent, plotting_type = plotting_type, parameter_titre_dataset_1,
                                        parameter_titre_dataset_2, outputonly)
      if(removemap| !print_map){
        Geographicdiff$plott <- NULL
        gc()
      }
    } else {
      Geographicdiff <- NULL
    }

  } else {
    summary_of_differences <- NULL
    compare_strata_differences_list <- NULL
    plot_titles_list <- NULL
    Geographicdiff <- NULL
    compare_dimension_differences_list <- NULL
  }

  if (length(parameter_time_dimension) != 0 && coverage) {
    time_coverage_analysis_list <- CWP.dataset::time_coverage_analysis(time_dimension_list_groupped, parameter_time_dimension, parameter_titre_dataset_1, parameter_titre_dataset_2, unique_analyse)
  } else {
    time_coverage_analysis_list <- NULL
  }

  if(coverage){
    other_dimension_analysis_list <- CWP.dataset::other_dimension_analysis(Other_dimensions, init, final, parameter_titre_dataset_1, parameter_titre_dataset_2, unique_analyse)
  } else {
    other_dimension_analysis_list <- NULL
  }
  if (print_map && coverage) {
    spatial_coverage_analysis_list <- CWP.dataset::spatial_coverage_analysis(init, final, parameter_titre_dataset_1, parameter_titre_dataset_2, shapefile_fix, plotting_type = plotting_type, continent, TRUE, GrouppedGRIDTYPE)
  } else {
    spatial_coverage_analysis_list <- NULL
  }

  combined_summary_histogram <- CWP.dataset::combined_summary_histogram_function(init, parameter_titre_dataset_1, final, parameter_titre_dataset_2)

  rm(init)
  rm(final)
  gc()

  return(list(combined_summary_histogram = combined_summary_histogram,
    summary_of_differences = summary_of_differences,
    compare_strata_differences_list = compare_strata_differences_list,
    groupping_differences_list = groupping_differences_list,
    compare_dimension_differences_list = compare_dimension_differences_list,
    plot_titles_list = plot_titles_list,
    Geographicdiff = Geographicdiff,
    time_coverage_analysis_list = time_coverage_analysis_list,
    spatial_coverage_analysis_list = spatial_coverage_analysis_list,
    other_dimension_analysis_list = other_dimension_analysis_list,
    Other_dimensions = Other_dimensions,
    coverage = coverage, unique_analyse = unique_analyse,
    parameter_titre_dataset_1 = parameter_titre_dataset_1,
    parameter_titre_dataset_2 = parameter_titre_dataset_2,
    parameter_filtering = parameter_filtering,
    parameter_resolution_filter = parameter_resolution_filter,
    fig.path = fig.path, parameter_short = parameter_short
  ))
}
