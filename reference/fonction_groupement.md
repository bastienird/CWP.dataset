# Group and Summarize Data

This function takes two data.tables and groups them by specified
columns, summing the measurement values for each group, and then
compares the results from both data.tables. It computes the loss or gain
in measurement values and provides additional metrics related to the
comparison.

## Usage

``` r
fonction_groupement(these_col, init, final)
```

## Arguments

- these_col:

  A character vector of column names to group by.

- init:

  A data.table containing the initial measurement data.

- final:

  A data.table containing the final measurement data.

## Value

A data.table containing the results of the comparison between the two
input data.tables, including summed values, losses or gains, and
percentage differences.
