# Calculate and Visualize Data Distribution for Other Dimensions

This function calculates and visualizes the data distribution for
various dimensions using pie charts and bar charts.

## Usage

``` r
other_dimension_analysis(
  Other_dimensions,
  init,
  final,
  titre_1,
  titre_2,
  unique_analyse = FALSE,
  fig.path,
  topn = 7,
  deferred = FALSE
)
```

## Arguments

- Other_dimensions:

  A vector of dimensions to analyze.

- init:

  Initial dataset.

- final:

  Final dataset.

- titre_1:

  Title for the first dataset.

- titre_2:

  Title for the second dataset.

- unique_analyse:

  Logical indicating whether the analysis is unique.

- fig.path:

  Path to save the figures.

- topn:

  An integer for the number of top categories to display.

- deferred:

  Logical. If `TRUE`, the charts are returned as deferred plots (their
  description, to be drawn with
  [`cwp_materialise_plot()`](https://bastienird.github.io/CWP.dataset/reference/cwp_materialise_plot.md))
  instead of plot objects. Default `FALSE`.

## Value

A list containing the pie charts and bar charts for each dimension.

## Author

Bastien Grasset, <bastien.grasset@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
other_dimension_analysis(c("Dimension1", "Dimension2"), init, final, "Dataset1", "Dataset2", FALSE, "path/to/save")
} # }
```
