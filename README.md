
|                         |
|-------------------------|
| output: github_document |

<!-- README.md is generated from README.Rmd. Please edit that file -->

# CWP.dataset

<!-- badges: start -->

[![R-CMD-check](https://github.com/bastienird/CWP.dataset/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bastienird/CWP.dataset/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bastienird/CWP.dataset/graph/badge.svg)](https://app.codecov.io/gh/bastienird/CWP.dataset)
<!-- badges: end -->

CWP.dataset is an R package designed for analyzing and comparing
fisheries datasets which are compliant with the CWP standards.

It includes functions for: ✔ Data harmonization and structuring ✔
Spatiotemporal analyses ✔ Conversion between different measurement units
(e.g., tons ↔ numbers) ✔ Comparing nominal and georeferenced datasets

## Installation

You can install the development version of CWP.dataset from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("bastienird/CWP.dataset")
```

## Documentation

Full documentation website on:
<https://bastienird.github.io/CWP.dataset>

## Example

``` r
fusen::draw_package_structure()
#> ── Reading NAMESPACE file ───────────────────────────────────────────
#> ── flat_additional.Rmd ──────────────────────────────────────────────
#> ── flat_first.Rmd ───────────────────────────────────────────────────
#> ── keep ─────────────────────────────────────────────────────────────
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
#>     - R/qflextable2.R
#>       + 🙈 
#>       + 👀 qflextable2
#>     - R/bar_plot_default.R
#>       + 👀 bar_plot_default
#>     - R/cat_title.R
#>       + 👀 cat_title
#>     - R/compute_summary_of_differences.R
#>       + 👀 compute_summary_of_differences
#>     - R/filtering_function.R
#>       + 👀 filtering_function
#>       + 👀 last_path_reduced
#>     - R/fonction_empreinte_spatiale.R
#>       + 👀 fonction_empreinte_spatiale
#>     - R/fonction_groupement.R
#>       + 👀 fonction_groupement
#>     - R/function_multiple_comparison.R
#>       + 👀 function_multiple_comparison
#>     - R/generate_plot.R
#>       + 👀 generate_plot
#>     - R/is_ggplot.R
#>       + 👀 is_ggplot
#>     - R/isnulllist.R
#>       + 👀 isNullList
#>     - R/knitting_plots_subfigures.R
#>       + 👀 knitting_plots_subfigures
#>     - R/pie_chart_2_default.R
#>       + 👀 pie_chart_2_default
#>     - R/read_data.R
#>       + 👀 read_data
#>     - R/render_subfigures.R
#>       + 👀 render_subfigures
#>     - R/save_image.R
#>       + 👀 save_image
#>     - R/separate_chunks_and_text.R
#>       + 👀 separate_chunks_and_text
#>   - tests
#>       + tests/testthat/test-read_data.R
#>       + tests/testthat/test-isnulllist.R
#>       + tests/testthat/test-filtering_function.R
#>       + tests/testthat/test-knitting_plots_subfigures.R
#>       + tests/testthat/test-render_subfigures.R
#>       + tests/testthat/test-bar_plot_default.R
#>       + tests/testthat/test-generate_plot.R
#>       + tests/testthat/test-pie_chart_2_default.R
#>       + tests/testthat/test-fonction_empreinte_spatiale.R
#>       + tests/testthat/test-save_image.R
#>       + tests/testthat/test-cat_title.R
#>       + tests/testthat/test-is_ggplot.R
#>       + tests/testthat/test-qflextable2.R
#>       + tests/testthat/test-function_multiple_comparison.R
#>       + tests/testthat/test-compute_summary_of_differences.R
#>       + tests/testthat/test-fonction_groupement.R
#>       + tests/testthat/test-separate_chunks_and_text.R
#>   - vignettes
#>       + vignettes/first.Rmd
#> - keep
#>   - path
#>       + keep
#>   - state
#>       + 🍏 active
#>   - R
#>     - R/calculate_rf_to_reach_final.R
#>       + 👀 calculate_rf_to_reach_final
#>     - R/combined_summary_histogram_function.R
#>       + 🙈 combined_summary_histogram_function
#>     - R/compare_dimension_differences.R
#>       + 👀 compare_dimension_differences
#>     - R/compare_georef_nominal.R
#>       + 👀 compare_georef_nominal
#>     - R/compare_nominal_georef_corrected.R
#>       + 🙈 compare_nominal_georef_corrected
#>     - R/compare_strata_differences.R
#>       + 👀 compare_strata_differences
#>     - R/compare_temporal_differences_dygraphs.R
#>       + 🙈 compare_temporal_differences_dygraphs
#>     - R/compare_temporal_differences.R
#>       + 👀 compare_temporal_differences
#>     - R/compnumberstratas.R
#>       + 🙈 compnumberstratas
#>     - R/comprehensive_cwp_dataframe_analysis.R
#>       + 👀 comprehensive_cwp_dataframe_analysis
#>     - R/copy_project_files.R
#>       + 🙈 copy_project_files
#>     - R/copyrmd.R
#>       + 🙈 copyrmd
#>     - R/create_dygraph_for_unit.R
#>       + 🙈 create_dygraph_for_unit
#>     - R/function_geographic_identifier_renaming_and_not_standards_unit.R
#>       + 👀 function_geographic_identifier_renaming_and_not_standards_unit
#>     - R/function_recap_each_step.R
#>       + 👀 function_recap_each_step
#>     - R/Functions_markdown.R
#>       + 🙈 
#>     - R/generate_bookdown_yml.R
#>       + 👀 generate_bookdown_yml
#>     - R/geographic_diff.R
#>       + 👀 geographic_diff
#>     - R/groupping_differences.R
#>       + 👀 groupping_differences
#>     - R/is_null_or_not_exist.R
#>       + 👀 is_null_or_not_exist
#>     - R/last_path_reduced.R
#>       + 👀 last_path_reduced
#>     - R/last_path.R
#>       + 👀 last_path
#>     - R/map_species_to_aggregated.R
#>       + 🙈 map_species_to_aggregated
#>     - R/other_dimension_analysis_dygraphs.R
#>       + 👀 other_dimension_analysis_dygraphs
#>     - R/other_dimension_analysis.R
#>       + 👀 other_dimension_analysis
#>     - R/Parameters_settings.R
#>       + 🙈 Parameters_settings
#>     - R/pie_chart_2_default_plotrix.R
#>       + 👀 pie_chart_2_default_plotrix
#>     - R/process_fisheries_data_by_species.R
#>       + 🙈 process_fisheries_data_by_species
#>     - R/process_fisheries_data.R
#>       + 🙈 process_fisheries_data
#>     - R/process_fisheries_effort_data.R
#>       + 🙈 process_fisheries_effort_data
#>     - R/recap_all_markdown.R
#>       + 🙈 recap_all_markdown
#>     - R/spatial_coverage_analysis.R
#>       + 👀 spatial_coverage_analysis
#>     - R/species_and_gear_group.R
#>       + 🙈 
#>     - R/strata_in_georef_but_not_in_nominal_report_launching.R
#>       + 👀 strata_in_georef_but_not_in_nominal_report_launching
#>     - R/strata_with_catches_without_effort.R
#>       + 🙈 strata_with_catches_without_effort
#>     - R/Summarising_invalid_data.R
#>       + 👀 Summarising_invalid_data
#>     - R/summarising_step.R
#>       + 👀 summarising_step
#>     - R/tidying_data.R
#>       + 👀 tidying_data
#>     - R/tidying_GTA_data_for_comparison.R
#>       + 🙈 tidying_GTA_data_for_comparison
#>     - R/time_coverage_analysis_dygraphs.R
#>       + 👀 time_coverage_analysis_dygraphs
#>     - R/time_coverage_analysis.R
#>       + 👀 time_coverage_analysis
#>     - R/timecoverage.R
#>       + 👀 timecoverage
#>     - R/write_options_to_csv.R
#>       + 🙈 write_options_to_csv
#>   - tests
#>       + tests/testthat/test-calculate_rf_to_reach_final.R
#>       + tests/testthat/test-combined_summary_histogram_function.R
#>       + tests/testthat/test-compare_dimension_differences.R
#>       + tests/testthat/test-compare_georef_nominal.R
#>       + tests/testthat/test-compare_nominal_georef_corrected.R
#>       + tests/testthat/test-compare_strata_differences.R
#>       + tests/testthat/test-compare_temporal_differences_dygraphs.R
#>       + tests/testthat/test-compare_temporal_differences.R
#>       + tests/testthat/test-comparison-between-multiple-cwp-datasets.R
#>       + tests/testthat/test-compnumberstratas.R
#>       + tests/testthat/test-comprehensive_cwp_dataframe_analysis.R
#>       + tests/testthat/test-copy_project_files.R
#>       + tests/testthat/test-copyrmd.R
#>       + tests/testthat/test-create_dygraph_for_unit.R
#>       + tests/testthat/test-function_geographic_identifier_renaming_and_not_standards_unit.R
#>       + tests/testthat/test-function_recap_each_step.R
#>       + tests/testthat/test-Functions_markdown.R
#>       + tests/testthat/test-geographic_diff.R
#>       + tests/testthat/test-groupping_differences.R
#>       + tests/testthat/test-is_null_or_not_exist.R
#>       + tests/testthat/test-last_path_reduced.R
#>       + tests/testthat/test-last_path.R
#>       + tests/testthat/test-other_dimension_analysis_dygraphs.R
#>       + tests/testthat/test-other_dimension_analysis.R
#>       + tests/testthat/test-Parameters_settings.R
#>       + tests/testthat/test-pie_chart_2_default_plotrix.R
#>       + tests/testthat/test-process_fisheries_data_by_species.R
#>       + tests/testthat/test-process_fisheries_data.R
#>       + tests/testthat/test-recap_all_markdown.R
#>       + tests/testthat/test-spatial_coverage_analysis.R
#>       + tests/testthat/test-species_and_gear_group.R
#>       + tests/testthat/test-strata_in_georef_but_not_in_nominal_report_launching.R
#>       + tests/testthat/test-strata_with_catches_without_effort.R
#>       + tests/testthat/test-Summarising_step.R
#>       + tests/testthat/test-tidying_data.R
#>       + tests/testthat/test-tidying_GTA_data_for_comparison.R
#>       + tests/testthat/test-time_coverage_analysis_dygraphs.R
#>       + tests/testthat/test-time_coverage_analysis.R
#>       + tests/testthat/test-timecoverage.R
#>       + tests/testthat/test-write_options_to_csv.R
#>   - vignettes
```
