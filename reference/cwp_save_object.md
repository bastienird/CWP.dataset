# Save and read R objects on disk

The intermediate results of a report (lists of results, environments)
are R objects, not tables: they are stored as `.rds`, the native format
of R, which needs no package and stays readable by any version of R.
Tables go to Parquet instead (see
[`read_data()`](https://bastienird.github.io/CWP.dataset/reference/read_data.md)).

## Usage

``` r
cwp_save_object(x, file, compress = FALSE)

cwp_read_object(file)
```

## Arguments

- x:

  Any R object.

- file:

  Path of the file. `cwp_save_object()` expects a `.rds` path.

- compress:

  Logical. Compress the file? Default `FALSE`: writing and reading are
  faster, which matters more than size for intermediate results.

## Value

`cwp_save_object()` returns `file`, invisibly. `cwp_read_object()`
returns the object.

## Details

`cwp_read_object()` also reads `.qs` files written by earlier versions
of the package, provided the qs package is installed.

## Examples

``` r
file <- tempfile(fileext = ".rds")
cwp_save_object(list(a = 1), file)
cwp_read_object(file)
#> $a
#> [1] 1
#> 
```
