# Tidying Data by Keeping Specific Columns

This function tidies a data frame by selecting specified columns and
converting time columns to character.

## Usage

``` r
tidying_data(dataframe, parameter_colnames_to_keep_dataframe, time_dimension)
```

## Arguments

- dataframe:

  A data frame to tidy.

- parameter_colnames_to_keep_dataframe:

  A character vector of column names to keep.

- time_dimension:

  A character vector of time dimension column names.

## Value

A tidied data frame.
