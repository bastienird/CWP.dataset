# Calculate and Compare Strata Differences

This function calculates the differences in strata between two datasets
and provides various summaries and visualizations of the results.

## Usage

``` r
compare_strata_differences(
  init,
  final,
  Groupped_all,
  titre_1 = "Dataset1",
  titre_2 = "Dataset2",
  parameter_columns_to_keep,
  unique_analyse = FALSE
)
```

## Arguments

- init:

  Data frame containing initial geographical data.

- final:

  Data frame containing final geographical data.

- Groupped_all:

  Data frame containing grouped strata data.

- titre_1:

  Title for the first dataset.

- titre_2:

  Title for the second dataset.

- parameter_columns_to_keep:

  Vector of column names to keep in the final output.

- unique_analyse:

  Logical value indicating whether the analysis is unique.

## Value

A list containing summaries and visualizations of the strata
differences.

## Examples

``` r
if (FALSE) { # \dontrun{
compare_strata_differences(init, final, Groupped_all, "Dataset1", "Dataset2", c("Column1", "Column2"), FALSE)
} # }
```
