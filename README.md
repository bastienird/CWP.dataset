--
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# CWP.dataset

<!-- badges: start -->
[![R-CMD-check](https://github.com/bastienird/CWP.dataset/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bastienird/CWP.dataset/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/bastienird/CWP.dataset/graph/badge.svg)](https://app.codecov.io/gh/bastienird/CWP.dataset)
<!-- badges: end -->

The goal of CWP.dataset is to ...

## Installation

You can install the development version of CWP.dataset from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("bastienird/CWP.dataset")
```

## Documentation

Full documentation website on: https://bastienird.github.io/CWP.dataset

## Example



``` r
fusen::draw_package_structure()
#> ── Reading NAMESPACE file ─────────────────────────────────────────────────────────────────────────────────────────
#> ── flat_additional.Rmd ────────────────────────────────────────────────────────────────────────────────────────────
#> ── flat_first.Rmd ─────────────────────────────────────────────────────────────────────────────────────────────────
#> 
#> - flat_additional.Rmd
#>   - flat_title
#>       + flat_additional.Rmd empty
#>   - path
#>       + dev/flat_additional.Rmd
#>   - state
#>       + 🍏 active
#>   - R
#>     - R/my_fun.R
#>       + 👀 my_fun
#>   - tests
#>       + tests/testthat/test-my_fun.R
#>   - vignettes
#>       + vignettes/go-further.Rmd
#> - flat_first.Rmd
#>   - flat_title
#>       + flat_first.Rmd for working package
#>   - path
#>       + dev/flat_first.Rmd
#>   - state
#>       + 🍏 active
#>   - R
#>     - R/bar_plot_default.R
#>       + 👀 bar_plot_default
#>       + 👀 cat_title
#>       + 👀 fonction_empreinte_spatiale
#>       + 👀 is_ggplot
#>       + 👀 pie_chart_2_default
#>       + 👀 qflextable2
#>       + 👀 save_image
#>     - R/compute_summary_of_differences.R
#>       + 👀 compute_summary_of_differences
#>     - R/isnulllist.R
#>       + 👀 filtering_function
#>       + 👀 isNullList
#>     - R/fonction_groupement.R
#>       + 👀 fonction_groupement
#>       + 👀 separate_chunks_and_text
#>     - R/function_multiple_comparison.R
#>       + 👀 function_multiple_comparison
#>     - R/knitting_plots_subfigures.R
#>       + 👀 knitting_plots_subfigures
#>       + 👀 render_subfigures
#>     - R/read_data.R
#>       + 👀 read_data
#>     - R/comparison-between-multiple-cwp-datasets.R
#>       + 🙈 
#>   - tests
#>       + tests/testthat/test-read_data.R
#>       + tests/testthat/test-fonction_groupement.R
#>       + tests/testthat/test-function_multiple_comparison.R
#>   - vignettes
#>       + vignettes/first.Rmd
```

