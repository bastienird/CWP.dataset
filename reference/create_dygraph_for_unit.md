# Create Interactive Dygraph for a Specific Unit and Measurement Type

This function filters a dataset for a given dimension (unit) and
measurement unit, prepares a time series from it, and generates an
interactive dygraph showing the temporal difference (in percent).

## Usage

``` r
create_dygraph_for_unit(data, filtering_unit, measurement_unit)
```

## Arguments

- data:

  A data frame containing at least the columns `Dimension`,
  `measurement_unit`, `Precision`, and `Difference (in %)`.

- filtering_unit:

  Character. The unit or dimension to filter on (e.g., "species",
  "gear_type", etc.).

- measurement_unit:

  Character. The measurement unit to filter on (e.g., "t", "no").

## Value

A `dygraph` htmlwidget displaying the temporal difference (in %) for the
selected unit and measurement.

## Examples

``` r
if (FALSE) { # \dontrun{
create_dygraph_for_unit(my_data, "species", "t")
} # }
```
