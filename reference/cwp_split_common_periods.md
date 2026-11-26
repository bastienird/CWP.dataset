# Split two datasets into their common years and the years of one only

Two versions of a dataset (e.g. two releases) are only comparable on the
years they both cover: a new release usually adds a year, and a source
may cover different years in each version. This function keeps, for each
group (by default each `source_authority`), the years present in both
datasets, and lists apart the years present in one dataset only, with
their totals. The common part can then be compared with
[`fonction_groupement()`](https://bastienird.github.io/CWP.dataset/reference/fonction_groupement.md)
or
[`comprehensive_cwp_dataframe_analysis()`](https://bastienird.github.io/CWP.dataset/reference/comprehensive_cwp_dataframe_analysis.md).

## Usage

``` r
cwp_split_common_periods(
  init,
  final,
  by = "source_authority",
  titre_1 = "Dataset 1",
  titre_2 = "Dataset 2"
)
```

## Arguments

- init, final:

  Data frames in CWP format, with `time_start` (or `year`),
  `measurement_value` and `measurement_unit`.

- by:

  Columns defining the groups whose years are compared.

- titre_1, titre_2:

  Names of the two datasets in the tables.

## Value

A list:

- `init`, `final`: the rows of the years present in both datasets, for
  their group;

- `periods`: one row per group, with the years of each dataset, the
  common years and the years of one dataset only, as compact ranges
  (e.g. "1950-1980, 1983");

- `uncommon`: totals by group, year and unit of the years present in one
  dataset only (`dataset` = `titre_1` or `titre_2`).

## Examples

``` r
a <- data.frame(
  source_authority = "IOTC", time_start = c("2019-01-01", "2020-01-01"),
  measurement_unit = "t", measurement_value = c(1, 2)
)
b <- rbind(a, data.frame(
  source_authority = "IOTC", time_start = "2021-01-01",
  measurement_unit = "t", measurement_value = 3
))
cwp_split_common_periods(a, b)$periods
#>    source_authority Years Dataset 1 Years Dataset 2 Common years
#>              <char>          <char>          <char>       <char>
#> 1:             IOTC       2019-2020       2019-2021    2019-2020
#>    Only in Dataset 1 Only in Dataset 2
#>               <char>            <char>
#> 1:                                2021
```
