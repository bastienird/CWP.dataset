# Comprehensive Analysis of Initial and Final Datasets

This function performs a detailed comparative analysis between initial
and final datasets. It includes:

- A summary of differences

- Grouping differences

- Dimension comparisons (temporal, spatial, and categorical)

- Visualization options

## Usage

``` r
comprehensive_cwp_dataframe_analysis(
  parameter_init,
  parameter_final,
  fig.path = getwd(),
  parameter_fact = "catch",
  parameter_short = FALSE,
  parameter_columns_to_keep = c("Precision", "measurement_unit", "Values dataset 1",
    "Values dataset 2", "Loss / Gain", "Difference (in %)", "Dimension",
    "Difference in value"),
  parameter_diff_value_or_percent = "Difference (in %)",
  parameter_filtering = list(species = NULL, fishing_fleet = NULL),
  parameter_time_dimension = c("time_start"),
  parameter_geographical_dimension = "geographic_identifier",
  parameter_geographical_dimension_groupping = "gridtype",
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
  removemap = FALSE,
  topnumber = 6,
  deferred_plots = getOption("CWP.dataset.deferred_plots", TRUE)
)
```

## Arguments

- parameter_init:

  Data frame. The initial dataset.

- parameter_final:

  Data frame. The final dataset.

- fig.path:

  Character. Path to save output figures. Default is the working
  directory.

- parameter_fact:

  Character. Fact parameter, default is `"catch"`.

- parameter_short:

  Logical. Whether to use short parameter names. Default is `FALSE`.

- parameter_columns_to_keep:

  Character vector. Columns to retain for comparison.

- parameter_diff_value_or_percent:

  Character. Difference calculation method: `"Difference (in %)"`
  (default) or `"Difference in value"`.

- parameter_filtering:

  List. Filtering parameters for species and fishing fleet. Default is
  `list(species = NULL, fishing_fleet = NULL)`.

- parameter_time_dimension:

  Character vector. Time-related columns to include. Default is
  `c("time_start")`.

- parameter_geographical_dimension:

  Character. Column name for geographic identifiers. Default is
  `"geographic_identifier"`.

- parameter_geographical_dimension_groupping:

  Character. Grouping column for geographical dimensions. Default is
  `"gridtype"`.

- parameter_colnames_to_keep:

  Character or `"all"`. Column names to retain. Default is `"all"`.

- outputonly:

  Logical. Whether to return only the analysis output without
  visualization. Default is `FALSE`.

- plotting_type:

  Character. Type of visualization (`"view"` by default).

- print_map:

  Logical. Whether to print the map visualization. Default is `TRUE`.

- shapefile_fix:

  Object. Optional shapefile for spatial analysis. Default is `NULL`.

- continent:

  Character. Optional filter for a specific continent. Default is
  `NULL`.

- coverage:

  Logical. Whether to analyze time, geographic, and other dimensions. If
  `FALSE`, only a summary is performed. Default is `TRUE`.

- parameter_resolution_filter:

  Object. Resolution filtering parameter. Default is `NULL`.

- parameter_titre_dataset_1:

  Character. Title for dataset 1 in outputs. Default is `"Dataset 1"`.

- parameter_titre_dataset_2:

  Character. Title for dataset 2 in outputs. Default is `"Dataset 2"`.

- unique_analyse:

  Logical. Whether the analysis is unique. Default is `FALSE`.

- removemap:

  Logical. Whether to remove the map from outputs. Default is `FALSE`.

- topnumber:

  Integer. Number of top characteristics to display without grouping.
  Default is `6`.

- deferred_plots:

  Logical. If `TRUE` (default), the plots are not drawn: the result
  holds deferred plots, i.e. the small aggregated tables and the name of
  the function that draws them. They are drawn when the report is
  rendered, or with
  [`cwp_materialise_plot()`](https://bastienird.github.io/CWP.dataset/reference/cwp_materialise_plot.md).
  The result is then plain data: much lighter to save, and independent
  of the version of the plotting packages. Use `FALSE`, or
  `options(CWP.dataset.deferred_plots = FALSE)`, to get plot objects as
  before. Maps drawn with tmap (`map_engine = "tmap"`) are never
  deferred.

## Value

A list containing:

- **Summary of dataset differences**

- **Grouped differences**

- **Comparisons across time, space, and other dimensions**

- **Optional visualizations**
