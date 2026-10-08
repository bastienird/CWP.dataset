# CWP.dataset (development version)

## Performance

* `enrich_dataset_if_needed()` gains a `with_geom` argument. With `with_geom = FALSE`
  the grid WKT is not parsed and no polygon is attached to the rows; `summarising_step()`
  and `process_fisheries_data()` now use it since they only keep `without_geom`.
* Codelists and the packaged CWP grid are read once per session instead of on every call.
* `function_multiple_comparison()` no longer reads each `data.qs` twice.
* `groupping_differences()` and `compare_temporal_differences()` convert the datasets to
  `data.table` once instead of once per dimension.
* `enrich_dataset_if_needed()` no longer calls `library()`.
* `geographic_diff()` and `fonction_empreinte_spatiale()` (hence the spatial coverage maps) draw
  the grid cells as ggplot2 tiles instead of one tmap polygon per cell. These maps are static:
  set `options(CWP.dataset.map_engine = "tmap")`, or `map_engine = "tmap"`, to get the previous
  tmap maps back (interactive in HTML output).

## Lighter saved results

* Functions that receive or read a full dataset and return plots (`comprehensive_cwp_dataframe_analysis()`,
  `function_multiple_comparison()`, `compare_temporal_differences()`, `pie_chart_2_default()`,
  `combined_summary_histogram_function()`, `geographic_diff()`, `fonction_empreinte_spatiale()`,
  `process_fisheries_data()`, `process_fisheries_effort_data()`) now drop the datasets from their
  environment when they exit. A ggplot keeps references to the environment it was built in, so
  saved results (`.qs`) embedded the full datasets with each plot.

## Bug fixes

* `time_coverage_analysis()`: `&&` was used on a vector, which is an error from R 4.3.
* Removed several tidyselect deprecation warnings (`all_of()`).
* `pie_chart_2_default()`: the "same distribution" check now matches classes by name and unit
  instead of by position (it compared vectors of different lengths or orders).
* `pie_chart_2_default()`: appearing / disappearing strata were computed by comparing the first
  dataset with itself, so none was ever reported; the second dataset is now used.
* `pie_chart_2_default()`: no more palette warning with fewer than 3 classes.

* `summarising_step()`: the `sizepdf = "short"` cache fallback now reads the `long` render
  environment (it pointed to a file that did not exist).
* `summarising_step()`: entities are matched to their directory by identifier, with a
  fallback on position, instead of by position only.
* `fonction_groupement()`: `number_lines1` / `number_lines2` and the difference in number
  of lines are 0-based instead of `NA` for strata missing from one dataset.
* Cache messages in `summarising_step()` are now actually logged.

# CWP.dataset 0.0.1
## Changelog
- Correction des conflits d'import (`data.table` vs `dplyr`)
- Ajout d'une gestion conditionnelle pour `dygraphs`
