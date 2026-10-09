# Create Bar Plots from Measurement Data

This function creates bar plots comparing measurement data for one or
two datasets.

## Usage

``` r
bar_plot_default(
  first,
  second = NULL,
  dimension,
  topn = 10,
  titre_1 = "first",
  titre_2 = "second",
  fill_colors = NULL,
  outline_colors = NULL
)
```

## Arguments

- first:

  A data frame representing the first dataset.

- second:

  An optional second data frame.

- dimension:

  A character string indicating the dimension for grouping.

- topn:

  An integer for the number of top categories to display.

- titre_1:

  A character string for the title of the first dataset.

- titre_2:

  A character string for the title of the second dataset.

- fill_colors:

  Optional vector of fill colors for the bars.

- outline_colors:

  Optional vector of outline colors for the bars.

## Value

A bar plot object.
