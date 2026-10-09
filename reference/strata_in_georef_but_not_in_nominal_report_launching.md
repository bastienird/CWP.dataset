# Launch Strata in Georef but not in Nominal Report

This function launches a report comparing strata in georef data but not
in nominal data.

## Usage

``` r
strata_in_georef_but_not_in_nominal_report_launching(
  main.dir,
  connectionDB,
  uploadgoogledrive = FALSE
)
```

## Arguments

- main.dir:

  Directory containing the main data files.

- connectionDB:

  Database connection object.

- uploadgoogledrive:

  Deprecated and ignored: the upload to Google Drive has been removed.
  Kept so that existing calls do not fail.

## Value

Data frame containing the upgraded nominal data.
