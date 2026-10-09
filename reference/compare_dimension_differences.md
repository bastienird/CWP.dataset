# Calculate and Visualize Differences for Each Dimension

This function calculates the differences in various dimensions between
two datasets and provides a detailed summary of the differences.

## Usage

``` r
compare_dimension_differences(
  Groupped_all,
  Other_dimensions,
  parameter_diff_value_or_percent,
  parameter_columns_to_keep = c("Precision", "measurement_unit", "Values dataset 1",
    "Values dataset 2", "Loss / Gain", "Difference (in %)", "Dimension",
    "Difference in value"),
  topn = 6,
  outputonly = FALSE
)
```

## Arguments

- Groupped_all:

  Data frame containing grouped strata data.

- Other_dimensions:

  Vector of dimensions to be analyzed.

- parameter_diff_value_or_percent:

  Character string indicating the parameter to sort by ("Difference in
  value" or "Difference (in %)").

- parameter_columns_to_keep:

  Vector of column names to keep in the final output.

- topn:

  Integer indicating the number of top differences to display.

- outputonly:

  Logical value indicating whether to output only the image.

## Value

A data frame containing the summarized differences for each dimension.

## Author

Bastien Grasset, <bastien.grasset@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
compare_dimension_differences(Groupped_all, c("Dimension1", "Dimension2"), "Difference in value", c("Column1", "Column2"), 6, FALSE)
} # }
```
