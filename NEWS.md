# CWP.dataset (development version)

## Deferred plots

* `comprehensive_cwp_dataframe_analysis()` no longer draws the plots: its result holds deferred
  plots, i.e. the small aggregated tables and the name of the internal function that draws each
  plot. They are drawn when the report is rendered (`knitting_plots_subfigures()`,
  `render_subfigures()` and the templates do it), or with the new `cwp_materialise_plot()`.
  Saved results are plain data: much lighter, cheaper to load at rendering, and no longer tied
  to the version of ggplot2 or cowplot they were created with.
* `deferred_plots = FALSE`, or `options(CWP.dataset.deferred_plots = FALSE)`, gives plot objects
  as before. The functions that build the plots (`compare_temporal_differences()`,
  `time_coverage_analysis()`, `combined_summary_histogram_function()`, `geographic_diff()`,
  `fonction_empreinte_spatiale()`, `spatial_coverage_analysis()`, `pie_chart_2_default()`,
  `other_dimension_analysis()`) gain a `deferred` argument, `FALSE` by default.
* Deferred maps refer to the continent layer of the package instead of carrying one copy each.
* Not deferred: maps drawn with tmap, and the plots of `process_fisheries_data()`.

## Report rendering

* `summarising_step()` gains `render_pdf` (default `FALSE`): the HTML report, which only needs
  pandoc, is always rendered; the PDF is rendered only when asked for and when `lualatex` is
  available. The availability check used to look for `pdflatex`, which is not the engine of the
  report.

## Storage format

* The dataset of each processing step is now saved as Parquet (`data.parquet`, with
  {nanoparquet}) instead of `data.qs`: an open, stable format, readable outside R.
  `function_recap_each_step()` and `summarising_step()` write it; every reader goes through
  the same internal helpers.
* Steps written as `data.qs` by earlier versions are still read, so existing jobs keep working.
  If a table cannot be written as Parquet, it is saved as `data.qs` with a warning.
* `read_data()` reads `.parquet` files.
* `summarising_step()` orders the steps by the date of their oldest dataset file, and the
  untouched copy kept at enrichment keeps the date of the original, so the order survives an
  interrupted run.

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
