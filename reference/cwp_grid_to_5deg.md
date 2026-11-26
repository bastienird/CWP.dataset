# 5 degree square of CWP 1 degree square codes

A CWP 1 degree square code (7 digits starting with 5: size, quadrant,
latitude on 2 digits, longitude on 3 digits) is replaced by the code of
the 5 degree square that contains it. Other codes are returned
unchanged.

## Usage

``` r
cwp_grid_to_5deg(code)
```

## Arguments

- code:

  Character vector of CWP grid codes.

## Value

Character vector of the same length.

## Examples

``` r
cwp_grid_to_5deg(c("5212124", "6230150", "IOTC_WEST"))
#> [1] "6210120"   "6230150"   "IOTC_WEST"
```
