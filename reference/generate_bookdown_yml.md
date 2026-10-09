# Generate a `_bookdown.yml` file dynamically

This function creates a `_bookdown.yml` file in the specified directory
to define the structure and configuration for a Bookdown project. It
allows specifying the output directory, merging behavior, and the list
of `.Rmd` files required for the Bookdown compilation.

## Usage

``` r
generate_bookdown_yml(
  destination = system.file("rmd", package = "CWP.dataset"),
  new_session = FALSE
)
```

## Arguments

- destination:

  Character. The directory where the `_bookdown.yml` file will be
  created. Default is `"bookdown_run"`.

- new_session:

  Boolean. Is the bookdown yml with new_session parameter to true or
  false

## Value

No return value. The function writes `_bookdown.yml` in the specified
destination directory.

## Examples

``` r
# Generate a `_bookdown.yml` in a custom directory
generate_bookdown_yml("custom_bookdown_dir")
#> [1] "/home/runner/work/CWP.dataset/CWP.dataset/docs/reference/_bookdown.yml"
```
