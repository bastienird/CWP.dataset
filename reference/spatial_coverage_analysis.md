# Calculate and Visualize Spatial Coverage

This function calculates the spatial coverage for different units and
provides visualizations of the values over spatial regions for each
dataset.

## Usage

``` r
spatial_coverage_analysis(
  init,
  final,
  titre_1 = "Dataset 1",
  titre_2 = "Dataset 2",
  shapefile.fix,
  plotting_type,
  continent,
  print_map = TRUE,
  Grouppedgridtype,
  deferred = FALSE
)
```

## Arguments

- init:

  Initial dataset.

- final:

  Final dataset.

- titre_1:

  Title for the first dataset.

- titre_2:

  Title for the second dataset.

- shapefile.fix:

  Shapefile for fixing spatial data.

- plotting_type:

  Type of plotting to be used.

- continent:

  Data frame of continent shapes for reference.

- print_map:

  Logical indicating whether to print the map.

- Grouppedgridtype:

  Data frame containing grouped gridtype data.

- deferred:

  Logical. If `TRUE`, tile maps are returned as deferred plots (their
  description, to be drawn with
  [`cwp_materialise_plot()`](https://bastienird.github.io/CWP.dataset/reference/cwp_materialise_plot.md))
  instead of plot objects. Default `FALSE`.

## Value

A list containing the spatial coverage maps and related information.

## Author

Bastien Grasset, <bastien.grasset@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
spatial_coverage_analysis(units, init, final, "Dataset1", "Dataset2", shapefile.fix, "plot", continent, TRUE, Grouppedgridtype, "path/to/save")
} # }
```
