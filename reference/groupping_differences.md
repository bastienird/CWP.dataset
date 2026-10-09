# Groupping Differences

This function groups differences between the initial and final datasets
based on various dimensions.

## Usage

``` r
groupping_differences(
  init,
  final,
  parameter_time_dimension,
  parameter_geographical_dimension,
  parameter_geographical_dimension_groupping
)
```

## Arguments

- init:

  Initial dataset.

- final:

  Final dataset.

- parameter_time_dimension:

  Vector of time dimensions.

- parameter_geographical_dimension:

  Vector of geographical dimensions.

- parameter_geographical_dimension_groupping:

  Vector of geographical dimension groupings.

## Value

A list containing grouped differences for all dimensions, geographical
dimension groupings, and time dimensions.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- groupping_differences(init, final, parameter_time_dimension, parameter_geographical_dimension, parameter_geographical_dimension_groupping)
} # }
```
