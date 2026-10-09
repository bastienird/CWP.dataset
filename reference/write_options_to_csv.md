# Write options to CSV and return structured output

This function writes a CSV file containing all the options of the data
column of the entity of geoflow for GTA workflows Additionally, it
returns a structured list containing the options renamed as a named
list.

## Usage

``` r
write_options_to_csv(opts)
```

## Arguments

- opts:

  A named list where each element is a vector of options corresponding
  to an entity.

## Value

A list containing:

- options:

  A named list where each key is `options_<name>` and the value is a
  concatenated string of options.

- table:

  A data frame with two columns: "Options" (names of the options) and
  "Position" (concatenated values).
