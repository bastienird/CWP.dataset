# Create Pie Charts from Data

This function creates pie charts from measurement data for one or two
datasets.

## Usage

``` r
pie_chart_2_default(
  dimension,
  first,
  second = NULL,
  topn = 5,
  titre_1 = "first",
  titre_2 = "second",
  title_yes_no = TRUE,
  dataframe = FALSE,
  deferred = FALSE
)
```

## Arguments

- dimension:

  A character string indicating the dimension for grouping.

- first:

  A data frame representing the first dataset.

- second:

  An optional second data frame.

- topn:

  An integer for the number of top categories to display.

- titre_1:

  A character string for the title of the first dataset.

- titre_2:

  A character string for the title of the second dataset.

- title_yes_no:

  Logical indicating if a title should be displayed.

- dataframe:

  Logical indicating if a data frame should be returned.

- deferred:

  Logical. If `TRUE`, the chart is returned as a deferred plot (its
  description, to be drawn with
  [`cwp_materialise_plot()`](https://bastienird.github.io/CWP.dataset/reference/cwp_materialise_plot.md))
  instead of a plot object. Default `FALSE`.

## Value

A pie chart or a list containing the pie chart and data frame, if
specified.
