# Save Plots with Subfigures in a Knitr Environment

This function saves plots and creates subfigures for rendering in
RMarkdown.

## Usage

``` r
knitting_plots_subfigures(
  plot,
  title,
  folder = "Unknown_folder",
  fig.pathinside = fig.path
)
```

## Arguments

- plot:

  The plot object to save.

- title:

  A character string for the plot title.

- folder:

  The folder where the plot will be saved.

- fig.pathinside:

  The path for saving the figure.

## Value

None
