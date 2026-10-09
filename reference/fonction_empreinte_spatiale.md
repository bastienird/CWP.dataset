# Spatial Footprint Function

This function generates a spatial representation of measurement values
from two datasets, allowing for comparison between initial and final
datasets using a provided shapefile.

## Usage

``` r
fonction_empreinte_spatiale(
  variable_affichee,
  initial_dataset = init,
  final_dataset = final,
  titre_1 = "Dataset 1",
  titre_2 = "Dataset 2",
  shapefile.fix = NULL,
  plotting_type = "plot",
  continent = NULL,
  map_engine = getOption("CWP.dataset.map_engine", "tiles"),
  deferred = FALSE
)
```

## Arguments

- variable_affichee:

  A character string indicating the measurement unit to be displayed.

- initial_dataset:

  A data.table containing the initial measurement data (default: init).

- final_dataset:

  A data.table containing the final measurement data (default: final).

- titre_1:

  A character string for the title of the first dataset (default:
  "Dataset 1").

- titre_2:

  A character string for the title of the second dataset (default:
  "Dataset 2").

- shapefile.fix:

  A spatial object (sf) for the polygons that defines the geographical
  areas.

- plotting_type:

  A character string indicating the type of plot ("plot" or "view").

- continent:

  An optional spatial object for adding continent borders to the plot.

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

A plot object representing the spatial footprint of the measurement
values.
