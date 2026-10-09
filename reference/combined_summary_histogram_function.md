# Create a Summary Histogram of Measurement Units by Dataset

This function generates a histogram comparing the distribution of
different measurement units across two datasets. The histogram displays
the percentage of each measurement unit type relative to the total
number of different strata in each dataset.

## Usage

``` r
combined_summary_histogram_function(
  init,
  parameter_titre_dataset_1 = "Init",
  final,
  parameter_titre_dataset_2 = "Final",
  deferred = FALSE
)
```

## Arguments

- init:

  A data.table containing the initial dataset.

- parameter_titre_dataset_1:

  A character string specifying the title for the initial dataset.
  Default is `"Init"`.

- final:

  A data.table containing the final dataset.

- parameter_titre_dataset_2:

  A character string specifying the title for the final dataset. Default
  is `"Final"`.

- deferred:

  Logical. If `TRUE`, the plot is returned as a deferred plot (its
  description, to be drawn with
  [`cwp_materialise_plot()`](https://bastienird.github.io/CWP.dataset/reference/cwp_materialise_plot.md))
  instead of a plot object. Default `FALSE`.

## Value

A ggplot2 histogram object displaying the percentage distribution of
measurement units across the two datasets.

## Details

- Converts both `init` and `final` datasets to `data.table`.

- Counts the number of unique strata for each measurement unit.

- Computes the percentage of each measurement unit within each dataset.

- Creates a histogram with stacked bars, displaying the percentage of
  each measurement unit.

- Uses a consistent color palette for different measurement units.
