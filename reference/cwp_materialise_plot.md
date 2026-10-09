# Draw a deferred plot

The analysis functions can return the description of a plot instead of
the plot itself (see the `deferred_plots` argument of
[`comprehensive_cwp_dataframe_analysis()`](https://bastienird.github.io/CWP.dataset/reference/comprehensive_cwp_dataframe_analysis.md)).
This function draws it. Anything that is not a deferred plot is returned
unchanged, so it can be called on any plot.

## Usage

``` r
cwp_materialise_plot(x, interactive = cwp_interactive_output())
```

## Arguments

- x:

  A deferred plot (class `cwp_plot_spec`), or any other object.

- interactive:

  Logical. Draw the interactive version of the plot when it has one? By
  default, `TRUE` when a report is being rendered to HTML and
  `options(CWP.dataset.interactive = FALSE)` has not been set, `FALSE`
  otherwise (PDF output, or outside a report).

## Value

The plot (usually a `ggplot` object, or an `htmlwidget` for an
interactive map), or `x` unchanged.

## Details

Maps have an interactive version (a leaflet widget), drawn instead of
the static one when `interactive` is `TRUE` and the leaflet package is
installed.

## Examples

``` r
cwp_materialise_plot(ggplot2::ggplot())
```
