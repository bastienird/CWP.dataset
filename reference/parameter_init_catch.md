# Example subset of catch dataset (initial)

A small sample of initial catch data, structured according to CWP
standards.

## Usage

``` r
parameter_init_catch
```

## Format

A tibble with 200 rows and 14 variables:

- source_authority:

  Source of the data (e.g. RFMO)

- gear_type:

  CWP gear type code

- fishing_fleet:

  Fishing fleet country or code

- fishing_mode:

  Fishing mode (e.g., UNK for unknown)

- time_start:

  Start date of the operation

- time_end:

  End date of the operation

- geographic_identifier:

  CWP geographic identifier

- measurement_unit:

  Unit of measurement (e.g., t, no, etc.)

- measurement_value:

  Measurement value (e.g., catch in tons)

- gridtype:

  Spatial resolution label

- Gear:

  Gear label

- fishing_mode_label:

  Fishing mode label

- fishing_fleet_label:

  Fishing fleet label

- species:

  Species code (e.g., YFT, SKJ, etc.)
