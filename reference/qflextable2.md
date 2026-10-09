# Create and Save a Flextable

This function creates a flextable from a data frame, optionally saving
it as an image.

## Usage

``` r
qflextable2(
  x,
  captionn = NULL,
  autonumm = autonum,
  pgwidth = 6,
  columns_to_color = NULL,
  save_folder = NULL,
  fig.pathinside = "Figures",
  grouped_data = NULL,
  interactive_plot = FALSE
)
```

## Arguments

- x:

  A data frame or flextable to create the table from.

- captionn:

  An optional character string for the table caption.

- autonumm:

  An optional automatic numbering parameter.

- pgwidth:

  A numeric value for the width of the table.

- columns_to_color:

  Optional columns to apply color coding.

- save_folder:

  Optional folder to save the flextable.

- fig.pathinside:

  A character string for the path to save figures.

- grouped_data:

  Optional grouped data for formatting.

- interactive_plot:

  Logical indicating if the output should be interactive.

## Value

A flextable object or a DT datatable if interactive_plot is TRUE.
