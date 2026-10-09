# Perform Time Coverage Analysis

This function analyzes the time coverage of values for specified time
dimensions and generates plots of the differences.

## Usage

``` r
timecoverage(
  parameter_time_dimension,
  unique_analyse,
  titre_1 = "titre_1",
  titre_2 = "titre_2",
  time_dimension_list_groupped,
  fig.path = "figures"
)
```

## Arguments

- parameter_time_dimension:

  A string representing the time dimension.

- unique_analyse:

  A boolean flag indicating whether to perform a unique analysis.
  Defaults to FALSE.

- titre_1:

  A string representing the title for the initial dataset.

- titre_2:

  A string representing the title for the final dataset.

- time_dimension_list_groupped:

  A list of data frames representing the grouped time dimensions.

- fig.path:

  A string representing the path to save the figures.

## Value

A list containing the following elements:

- time_dimension_list_groupped_diff_image_knit:

  A list of knitted plots for time dimension differences.

## Examples

``` r
if (FALSE) { # \dontrun{
timecoverage(unique_analyse, parameter_time_dimension, titre_1, titre_2, time_dimension_list_groupped, fig.path)
} # }
```
