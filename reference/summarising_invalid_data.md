# Summarizes invalid data in the provided dataset

This function identifies, aggregates, and processes invalid data within
a geoflow entity from rawdata in geoflow-tunaatlas workflow by analyzing
various entities and their respective tRFMOs. It checks for missing
data, incorrect values, and geographic inconsistencies. The function can
optionally upload results to a database and Google Drive.

## Usage

``` r
summarising_invalid_data(
  main_dir,
  connectionDB,
  upload_drive = FALSE,
  upload_DB = TRUE
)
```

## Arguments

- main_dir:

  The main working directory containing the dataset and necessary files.

- connectionDB:

  A database connection object used for querying relevant tables.

- upload_drive:

  Deprecated and ignored: the upload to Google Drive has been removed.
  Kept so that existing calls do not fail.

- upload_DB:

  Logical, whether to upload processed data to a database (default:
  TRUE).

## Value

Writes multiple summary CSV files and optional database tables,
returning no explicit value.
