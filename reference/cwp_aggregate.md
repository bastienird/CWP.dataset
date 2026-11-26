# Aggregate a CWP dataset to a coarser time and space resolution

Sums a dataset in CWP format by year (or month) and by 5 degree square
(the 1 degree squares are grouped into their 5 degree square), keeping
all its other dimensions. The result is much smaller than the dataset
and keeps its totals: it can be stored, and compared with the aggregate
of another version of the dataset (see
[`cwp_split_common_periods()`](https://bastienird.github.io/CWP.dataset/reference/cwp_split_common_periods.md)
and
[`comprehensive_cwp_dataframe_analysis()`](https://bastienird.github.io/CWP.dataset/reference/comprehensive_cwp_dataframe_analysis.md)).

## Usage

``` r
cwp_aggregate(
  data,
  time_resolution = c("year", "month"),
  spatial_resolution = 5,
  dimensions = c("source_authority", "species", "gear_type", "fishing_fleet",
    "fishing_mode", "geographic_identifier", "measurement_type", "measurement_unit")
)
```

## Arguments

- data:

  A data frame in CWP format: `time_start` (or `year`),
  `measurement_value` and dimension columns.

- time_resolution:

  `"year"` or `"month"`: period of `time_start` in the result.

- spatial_resolution:

  `5` to group the 1 degree squares into their 5 degree square, `NULL`
  to keep the geographic identifiers as they are. Codes that are not 1
  degree squares are always kept.

- dimensions:

  Dimension columns to keep (those absent from `data` are ignored).

## Value

A data.table with `time_start` (first day of the period, as a string),
the dimensions, `measurement_value` (sum) and `n_rows` (number of rows
of `data` summed).

## Examples

``` r
d <- data.frame(
  source_authority = "IOTC", species = "YFT", gear_type = "01.1",
  time_start = c("2020-01-01", "2020-02-01"),
  geographic_identifier = c("5100050", "5101051"),
  measurement_unit = "t", measurement_value = c(1, 2)
)
cwp_aggregate(d)
#>    time_start source_authority species gear_type geographic_identifier
#>        <char>           <char>  <char>    <char>                <char>
#> 1: 2020-01-01             IOTC     YFT      01.1               6100050
#>    measurement_unit measurement_value n_rows
#>              <char>             <num>  <int>
#> 1:                t                 3      2
```
