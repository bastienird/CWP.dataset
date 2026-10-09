# Perform Strata Comparisons

This function performs various comparisons between initial and final
datasets to identify lost and gained strata.

## Usage

``` r
compnumberstratas(
  init,
  final,
  Groupped_all,
  titre_1,
  titre_2,
  parameter_columns_to_keep,
  unique_analyse = FALSE
)
```

## Arguments

- init:

  A data frame representing the initial dataset.

- final:

  A data frame representing the final dataset.

- Groupped_all:

  A data frame containing grouped data.

- titre_1:

  A string representing the title for the initial dataset.

- titre_2:

  A string representing the title for the final dataset.

- parameter_columns_to_keep:

  A vector of column names to keep in the final comparison.

- unique_analyse:

  A boolean flag indicating whether to perform a unique analysis.
  Defaults to FALSE.

## Value

A list containing the following elements:

- strates_perdues_first_10:

  A data frame of the first 10 lost strata.

- number_init_column_final_column:

  A data frame comparing the number of unique values in columns between
  the initial and final datasets.

- disapandap:

  A data frame showing the differences in strata.

## Author

Bastien Grasset, <bastien.grasset@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
compnumberstratas(init, final, Groupped_all, titre_1, titre_2, parameter_columns_to_keep, unique_analyse = FALSE)
} # }
```
