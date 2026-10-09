# Enrich CWP Dataset

Completes a CWP-compliant dataset (data.frame or sf) by:

- Filling in missing descriptive labels (species, gear type, fleet,
  units, etc.);

- Attaching or validating spatial geometries.

Labels and geometries are sourced via SQL queries when `connectionDB` is
provided, with fallbacks to local files or online downloads otherwise.

## Usage

``` r
enrich_dataset_if_needed(
  data,
  connectionDB = NULL,
  save_prefix = NULL,
  shp_raw = NULL,
  with_geom = TRUE
)
```

## Arguments

- data:

  A `data.frame` or `sf` object following CWP conventions.

- connectionDB:

  Optional. A DBI-compatible database connection for querying codelists.

- save_prefix:

  Optional. Filename prefix for saving outputs (`.rds` and `.csv`).

- shp_raw:

  Optional. To prevent reading it every time that can be time consuming,
  we can provide it directly

- with_geom:

  Logical. Should the geometries be attached and the `sf` version
  returned? Default `TRUE`. Use `FALSE` when only `without_geom` is
  needed: attaching one polygon per row and parsing the grid WKT is by
  far the slowest part of this function.

## Value

A `list` with two elements:

- `with_geom`:

  An `sf` object enriched with geometries (CRS EPSG:4326), or `NULL`
  when `with_geom = FALSE`.

- `without_geom`:

  A `data.frame` enriched without geometries.

## Details

Enrich a CWP Dataset with Missing Labels and Geometries

Steps performed:

1.  Load required codelists (species, measurements, gear, fleet, etc.)
    via SQL or fallback.

2.  Normalize measurement units (e.g. tons number of fish.

3.  Left-join to add each `*_label` column alongside its code.

4.  Reorder columns so that each code is immediately followed by its
    label.

5.  Convert to `sf` with CRS EPSG:4326.

6.  Optionally save to disk if `save_prefix` is provided.

## See also

[`st_as_sf`](https://r-spatial.github.io/sf/reference/st_as_sf.html),
[`dbGetQuery`](https://dbi.r-dbi.org/reference/dbGetQuery.html)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  species                     = c("YFT", "SKJ"),
  measurement_unit            = c("Tons", "Number of fish"),
  geographic_identifier       = c("0001", "0002"),
  fishing_mode                = "UNK",
  fishing_fleet               = "NEI",
  measurement_type            = "NC",
  measurement                  = "catch",
  measurement_processing_level = "raised",
  gear_type                   = "99.9"
)
result <- enrich_dataset_if_needed(df)
str(result)
} # }
```
