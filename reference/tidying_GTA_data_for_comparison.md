# Tidy GTA Data for Comparison

This function processes a dataset by joining it with spatial and
categorical data, standardizing measurement units, and preparing it for
analysis.

## Usage

``` r
tidying_GTA_data_for_comparison(
  dataframe,
  shape = NULL,
  species_group_dataframe = NULL,
  cl_cwp_gear_level2_dataframe = NULL
)
```

## Arguments

- dataframe:

  A data frame or a file path (character) to an RDS file containing the
  dataset.

- shape:

  An optional spatial data frame with a `gridtype` and `cwp_code` column
  for geographic matching.

- species_group_dataframe:

  An optional data frame mapping species to groups.

- cl_cwp_gear_level2_dataframe:

  An optional data frame mapping gear types to level 2 categories.

## Value

A cleaned and processed data frame with standardized geographic
identifiers, measurement units, and additional categorical information
from external data frames.

## Details

- If `dataframe` is a file path, it is loaded using
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html).

- If `shape` is provided and `geographic_identifier` exists in
  `dataframe`, a spatial join is performed.

- If `species_group_dataframe` is provided and `species` exists in
  `dataframe`, a species join is performed.

- If `cl_cwp_gear_level2_dataframe` is provided and `gear_type` exists
  in `dataframe`, a gear-type join is performed.

- Measurement units are standardized to "Tons" or "Number of fish".

## Examples

``` r
if (FALSE) { # \dontrun{
# Example dataset
df <- data.frame(
  geographic_identifier = c("5100000", "5100001"),
  species = c("YFT", "BET"),
  gear_type = c("03.1.0", "01.1.0"),
  measurement_unit = c("MT", "NO"),
  measurement_value = c(100, 200)
)

shape_data <- data.frame(gridtype = c("1deg_x_1deg", "5deg_x_5deg"), cwp_code = c("5100000", "5100001"))
species_mapping <- data.frame(species = c("YFT", "BET"), species_group = c("Yellowfin Tuna", "Bigeye Tuna"))
gear_mapping <- data.frame(Code = c("03.1.0", "01.1.0"), GearLevel2 = c("Purse Seine", "Longline"))

# Run function
cleaned_df <- tidying_GTA_data_for_comparison(df, shape_data, species_mapping, gear_mapping)
} # }
```
