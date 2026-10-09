# Process and Plot Fisheries Data

This function processes fisheries data (catch or effort), applies
filtering, calculates statistics, and generates plots of the results.

## Usage

``` r
process_fisheries_effort_data(sub_list_dir_2, parameter_filtering)
```

## Arguments

- sub_list_dir_2:

  List of directories containing the data files.

- parameter_filtering:

  List of filtering parameters to be passed to the filtering function.

## Value

A list containing the processed data frame and the generated plots.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- process_fisheries_data(sub_list_dir_2, "catch", parameter_filtering)
print(result$processed_data)
print(result$second_graf)
print(result$no_fish_plot)
print(result$tons_plot)
} # }
```
