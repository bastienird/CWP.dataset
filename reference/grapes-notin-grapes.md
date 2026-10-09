# Negate `%in%` Operator

This operator checks if elements are not present in a given vector.

## Usage

``` r
x %notin% table
```

## Arguments

- x:

  A vector of elements to check.

- table:

  A vector to check against.

## Value

Logical vector indicating if each element of `x` is not in `table`.

## Examples

``` r
5 %notin% c(1, 2, 3) # TRUE
#> [1] TRUE
"a" %notin% c("b", "c") # TRUE
#> [1] TRUE
2 %notin% c(1, 2, 3) # FALSE
#> [1] FALSE
```
