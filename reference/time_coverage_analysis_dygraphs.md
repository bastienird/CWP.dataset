# Calculate and Visualize Time Coverage Using dygraphs

This function calculates the time coverage for different dimensions and
provides interactive visualizations of the values over time for each
dataset using dygraphs.

## Usage

``` r
time_coverage_analysis_dygraphs(
  time_dimension_list_groupped,
  parameter_time_dimension,
  titre_1,
  titre_2,
  unique_analyse = FALSE
)
```

## Arguments

- time_dimension_list_groupped:

  A list of data frames, each containing time dimension data.

- parameter_time_dimension:

  The time dimension parameter.

- titre_1:

  Title for the first dataset.

- titre_2:

  Title for the second dataset.

- unique_analyse:

  Logical value indicating whether the analysis is unique.

## Value

A list containing dygraphs objects for visualizing the time coverage.

## Author

Bastien Grasset, <bastien.grasset@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
time_coverage_analysis_dygraphs(time_dimension_list_groupped, "Year", "Dataset1", "Dataset2", FALSE)
} # }
```
