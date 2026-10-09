# First

``` r

library(CWP.dataset)
#> Warning: replacing previous import 'data.table::first' by 'dplyr::first' when
#> loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::between' by 'dplyr::between'
#> when loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::last' by 'dplyr::last' when
#> loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::wday' by 'lubridate::wday' when
#> loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::second' by 'lubridate::second'
#> when loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::isoweek' by
#> 'lubridate::isoweek' when loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::yday' by 'lubridate::yday' when
#> loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::hour' by 'lubridate::hour' when
#> loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::year' by 'lubridate::year' when
#> loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::month' by 'lubridate::month'
#> when loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::week' by 'lubridate::week' when
#> loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::isoyear' by
#> 'lubridate::isoyear' when loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::minute' by 'lubridate::minute'
#> when loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::mday' by 'lubridate::mday' when
#> loading 'CWP.dataset'
#> Warning: replacing previous import 'data.table::quarter' by
#> 'lubridate::quarter' when loading 'CWP.dataset'
#> 
#> Attaching package: 'CWP.dataset'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
```

``` r

# Example: Reading a CSV file
# csv_data <- read_data(here::here("inst/CWP_dataset.csv"))
# head(csv_data)  # Display the first few rows
```

## Last path to be handled in workflow

``` r

#' Extract Last Part of a File Path
#'
#' @description This function returns the last part of a file path after removing a specific suffix.
#' @param x A character string representing the file path.
#' @return A character string representing the last part of the file path.
#' @export
last_path <- function(x){
  x <- gsub("/rds.rds", "", x)
  substr(x, max(gregexpr("/", x)[[1]]) + 1, nchar(x))
}
```

## filtering_function

## Functions for reporting

## render_subfigures

### bar_plot_default

### generate_plot

### pie_chart_2_default

### fonction_empreinte_spatiale

### save_image

### cat_title

### is_ggplot

### qflextable2

#### Function Definitions

##### Function: function_multiple_comparison

##### Function: compute_summary_of_differences

##### Function: fonction_groupement

##### Function: separate_chunks_and_text

## Inflate your package

You’re one inflate from paper to box. Build your package from this very
Rmd using
[`fusen::inflate()`](https://thinkr-open.github.io/fusen/reference/inflate.html)

- Verify your `"DESCRIPTION"` file has been updated
- Verify your function is in `"R/"` directory
- Verify your test is in `"tests/testthat/"` directory
- Verify this Rmd appears in `"vignettes/"` directory
