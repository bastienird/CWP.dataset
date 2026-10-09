# Compare Nominal and Georeferenced Data

This function compares nominal and georeferenced datasets, aggregating
values by specified strata, identifying mismatches, and analyzing
differences in measurement values.

## Usage

``` r
compare_nominal_georef_corrected(
  nominal,
  georef_mapped,
  list_strata = list(c("species", "year", "source_authority", "gear_type",
    "fishing_fleet", "geographic_identifier_nom"))
)
```

## Arguments

- nominal:

  A data.table containing nominal data with measurement values.

- georef_mapped:

  A data.table containing georeferenced data with measurement values.

- list_strata:

  A list of character vectors specifying the grouping strata for
  comparison. Default: list(c("species", "year", "source_authority",
  "gear_type", "fishing_fleet", "geographic_identifier_nom")).

## Value

A list containing multiple comparison results, including:

- `georef_no_nominal`: Strata present in georeferenced data but absent
  in nominal.

- `georef_no_nominal_with_value`: Strata missing in nominal with their
  georeferenced measurement values.

- `georef_tons_no_nominal`: Strata in tons missing from nominal data.

- `georef_sup_nominal`: Strata where georeferenced data exceeds nominal
  data.

- `tons_nei_nominal`: NEI strata in nominal data that could explain
  differences.

- `tons_nei_georef`: NEI strata in georeferenced data that could explain
  differences.

- `sum_georef_no_nominal`: Total measurement value of georeferenced data
  missing in nominal.

- `suffisant`: Boolean indicating whether georeferenced data is
  sufficient to match nominal data.

- `tons_aggregated_georef`: Measurement values of aggregated
  georeferenced strata.

- `sum_georef_sup_nom`: Total excess of georeferenced data over nominal
  data.

## Details

- Converts both `nominal` and `georef_mapped` to `data.table` format.

- Extracts `year` from `time_start` for both datasets.

- Filters georeferenced data to retain only measurements in tons.

- Aggregates measurement values by the provided strata.

- Compares common and distinct strata between the datasets.

- Evaluates the impact of `NEI` and aggregated species (`TUN`, `TUS`,
  `BIL`) on discrepancies.

## Examples

``` r
if (FALSE) { # \dontrun{

nominal <- data.table(
  species = c("YFT", "BET"),
  year = c("2020", "2020"),
  source_authority = c("RFMO_A", "RFMO_B"),
  gear_type = c("03.1.0", "01.1.0"),
  fishing_fleet = c("Fleet_1", "Fleet_2"),
  geographic_identifier_nom = c("5100000", "5100001"),
  measurement_unit = c("t", "t"),
  measurement_value = c(100, 200)
)

georef_mapped <- data.table(
  species = c("YFT", "BET"),
  year = c("2020", "2020"),
  source_authority = c("RFMO_A", "RFMO_B"),
  gear_type = c("03.1.0", "01.1.0"),
  fishing_fleet = c("Fleet_1", "Fleet_2"),
  geographic_identifier_nom = c("5100000", "5100001"),
  measurement_unit = c("t", "t"),
  measurement_value = c(150, 180),
  time_start = as.Date(c("2020-01-01", "2020-01-01"))
)

result <- compare_nominal_georef_corrected(nominal, georef_mapped)
} # }
```
