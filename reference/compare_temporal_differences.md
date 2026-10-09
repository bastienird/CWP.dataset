# Calculate and Visualize Temporal Data Differences

This function calculates the differences in temporal data between two
datasets and provides visualizations of the differences in percent for
each year.

## Usage

``` r
compare_temporal_differences(
  parameter_time_dimension,
  init,
  final,
  titre_1,
  titre_2,
  unique_analyse = FALSE,
  deferred = FALSE
)
```

## Arguments

- parameter_time_dimension:

  A list of time dimensions to be analyzed.

- init:

  Data frame containing initial data.

- final:

  Data frame containing final data.

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

A list containing ggplot objects for visualizing the temporal
differences.

## Author

Bastien Grasset, <bastien.grasset@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
compare_temporal_differences(c("Year"), init, final, "Dataset1", "Dataset2", FALSE, "path/to/save")
} # }
```
