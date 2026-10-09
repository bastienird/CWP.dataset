# Calculate and Visualize Time Coverage

This function calculates the time coverage for different dimensions and
provides visualizations of the values over time for each dataset.

## Usage

``` r
time_coverage_analysis(
  time_dimension_list_groupped,
  parameter_time_dimension,
  titre_1,
  titre_2,
  unique_analyse = FALSE,
  deferred = FALSE
)
```

## Arguments

- time_dimension_list_groupped:

  A list of data frames, each containing time dimension data.

- parameter_time_dimension:

  The time dimension parameter.

- titre_1:

  Title for the first dataset.

- titre_2:

  Title for the second dataset.

- unique_analyse:

  Logical value indicating whether the analysis is unique.

- deferred:

  Logical. If `TRUE`, the plots are returned as deferred plots (their
  description, to be drawn with
  [`cwp_materialise_plot()`](https://bastienird.github.io/CWP.dataset/reference/cwp_materialise_plot.md))
  instead of plot objects. Default `FALSE`.

## Value

A list containing ggplot objects for visualizing the time coverage.

## Author

Bastien Grasset, <bastien.grasset@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
time_coverage_analysis(time_dimension_list_groupped, "Year", "Dataset1", "Dataset2", FALSE, "path/to/save")
} # }
```
