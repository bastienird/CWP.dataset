# CWP.dataset (development version)

## Dependencies

* The upload to Google Drive is removed, and {googledrive} with it. `summarising_invalid_data()`
  and `strata_in_georef_but_not_in_nominal_report_launching()` keep their `upload_drive` /
  `uploadgoogledrive` arguments so that existing calls do not fail; they are ignored, with a
  warning when `TRUE`.
* `pie_chart_2_default_plotrix()` is removed (it was not used anywhere), and {plotrix} with it.
* Imports go from 37 to 30 packages. {tmap}, {dygraphs}, {xts} and {DT} move to Suggests: each is only needed by an optional feature, whose function now stops
  with an explicit message when the package is missing. {tinytex} is declared in Suggests.
* The report setup (`Setup_markdown.Rmd`) only attaches {tinytex}, {tmap}, {kableExtra} and
  {webshot} when they are installed.
* `time_coverage_analysis()` no longer calls {zoo}, which was not declared.
* `render_subfigures()` calls {grid} and {gridExtra} explicitly; the HTML sub-figures relied on
  {grid} being attached.

## Interactive maps

* In the HTML report, the maps (differences between two datasets, spatial coverage) are now
  interactive leaflet maps: zoom, pan, and the values of a cell on click. The panels of the
  static map become layers, chosen with a control on the map. The PDF report keeps the static
  maps. This is decided when the report is rendered, from the same deferred map.
* `summarising_step()` gains `interactive` (default `TRUE`);
  `options(CWP.dataset.interactive = FALSE)` does the same outside it.
* {leaflet} is in Suggests: without it, the static maps are used in the HTML report too.
* The background of the interactive maps comes from an online tile provider and needs an
  internet connection when the report is read; the cells are always drawn.

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

* The package no longer depends on {qs}, which moves from Imports to Suggests. Report results
  (lists of results, environments) are saved as `.rds` through the new `cwp_save_object()` and
  `cwp_read_object()`; tables go to Parquet. `.qs` files from earlier versions are still read
  when {qs} is installed.
* Result files are renamed from `.qs` to `.rds`, so results cached by an earlier version
  (`usesave = TRUE`) are computed again.
* `enrich_dataset_if_needed(save_prefix = )` writes `<prefix>_with_geom.rds`.
* `summarising_step()` no longer writes a `UN_CONTINENT2` copy of the continent layer in the
  working directory.
* The continent layer shipped with the package is read from `inst/extdata/continent.rds` when it
  exists; run `data-raw/convert_continent.R` once to convert it.
* The dataset of each processing step is now saved as Parquet (`data.parquet`, with
  {nanoparquet}) instead of `data.qs`: an open, stable format, readable outside R.
  `function_recap_each_step()` and `summarising_step()` write it; every reader goes through
  the same internal helpers.
* Steps written as `data.qs` by earlier versions are still read, so existing jobs keep working.
  If a table cannot be written as Parquet, it is saved as `data.rds` with a warning.
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
