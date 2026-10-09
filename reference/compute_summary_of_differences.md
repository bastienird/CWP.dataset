# Compute Summary of Differences

This function computes the summary of differences in measurement values
between two datasets by grouping them by measurement unit and
calculating the sum of values. It also computes the percentage
difference.

## Usage

``` r
compute_summary_of_differences(
  init,
  final,
  titre_1 = "Dataset 1",
  titre_2 = "Dataset 2"
)
```

## Arguments

- init:

  A data.table containing the initial measurement data.

- final:

  A data.table containing the final measurement data.

- titre_1:

  A character string for the title of the first dataset (default:
  "Dataset 1").

- titre_2:

  A character string for the title of the second dataset (default:
  "Dataset 2").

## Value

A data.table summarizing the differences between the two datasets,
including total measurements and percentage differences.
