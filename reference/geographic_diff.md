# Geographic Differences

This function analyzes geographic data differences between two datasets.

## Usage

``` r
geographic_diff(
  init,
  final,
  shapefile_fix,
  parameter_geographical_dimension,
  parameter_geographical_dimension_groupping,
  continent,
  plotting_type,
  titre_1,
  titre_2,
  outputonly,
  map_engine = getOption("CWP.dataset.map_engine", "tiles"),
  deferred = FALSE
)
```

## Arguments

- init:

  Initial dataset

- final:

  Final dataset

- shapefile_fix:

  Shapefile to be used

- parameter_geographical_dimension:

  Parameter for geographical dimension

- parameter_geographical_dimension_groupping:

  Parameter for geographical dimension grouping

- continent:

  Shapefile of the continent

- plotting_type:

  Type of plotting ("plot" or other)

- titre_1:

  Title for the first dataset

- titre_2:

  Title for the second dataset

- outputonly:

  Boolean to specify if output should be saved only

- map_engine:

  Character. `"tiles"` (default) draws the grid cells as ggplot2 tiles,
  which is much faster than one polygon per cell but gives a static map.
  `"tmap"` keeps the previous tmap rendering (interactive in HTML when
  `tmap_mode("view")`). The default can be set for a whole session with
  `options(CWP.dataset.map_engine = "tmap")`. If the cells cannot be
  drawn as tiles, tmap is used.

- deferred:

  Logical. If `TRUE`, a tile map is returned as a deferred plot (its
  description, to be drawn with
  [`cwp_materialise_plot()`](https://bastienird.github.io/CWP.dataset/reference/cwp_materialise_plot.md))
  instead of a plot object. Default `FALSE`.

## Value

A list containing the geographic differences and a saved image
