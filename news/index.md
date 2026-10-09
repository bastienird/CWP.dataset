# Changelog

## CWP.dataset (development version)

### Continuous integration

- New end-to-end test: a two-step job is created from the example
  datasets in a temporary directory, and
  [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md)
  renders its HTML report. It checks the report pages, the enriched step
  datasets and that the maps are interactive. It is the first test going
  through the report templates; it needs pandoc and is skipped on CRAN.
- New `tests` workflow: it runs the test suite on every pull request and
  writes a report (totals, failing and skipped tests, duration by file)
  on the summary page of the run. The same report can be produced
  locally with `Rscript .github/scripts/test-report.R`.
- `R-CMD-check` runs on one platform (Ubuntu, R release) instead of
  five, only fails on errors for now, and can be started by hand.

### Dependencies

- The upload to Google Drive is removed, and {googledrive} with it.
  [`summarising_invalid_data()`](https://bastienird.github.io/CWP.dataset/reference/summarising_invalid_data.md)
  and
  [`strata_in_georef_but_not_in_nominal_report_launching()`](https://bastienird.github.io/CWP.dataset/reference/strata_in_georef_but_not_in_nominal_report_launching.md)
  keep their `upload_drive` / `uploadgoogledrive` arguments so that
  existing calls do not fail; they are ignored, with a warning when
  `TRUE`.
- `pie_chart_2_default_plotrix()` is removed (it was not used anywhere),
  and {plotrix} with it.
- Imports go from 37 to 30 packages. {tmap}, {dygraphs}, {xts} and {DT}
  move to Suggests: each is only needed by an optional feature, whose
  function now stops with an explicit message when the package is
  missing. {tinytex} is declared in Suggests.
- The report setup (`Setup_markdown.Rmd`) only attaches {tinytex},
  {tmap}, {kableExtra} and {webshot} when they are installed.
- [`time_coverage_analysis()`](https://bastienird.github.io/CWP.dataset/reference/time_coverage_analysis.md)
  no longer calls {zoo}, which was not declared.
- [`render_subfigures()`](https://bastienird.github.io/CWP.dataset/reference/render_subfigures.md)
  calls {grid} and {gridExtra} explicitly; the HTML sub-figures relied
  on {grid} being attached.

### Interactive maps

- In the HTML report, the maps (differences between two datasets,
  spatial coverage) are now interactive leaflet maps: zoom, pan, and the
  values of a cell on click. The PDF report keeps the static maps. This
  is decided when the report is rendered, from the same deferred map.
- Each panel of the static map is its own interactive map, side by side;
  with {leafsync} they move and zoom together.
  `options(CWP.dataset.map_layout = "layers")` gives a single map whose
  panels are layers, one visible at a time.
- The background is the land layer shipped with the package, simplified
  and embedded in the map: no internet connection or API key is needed.
  Online tiles can be added with
  `options(CWP.dataset.leaflet_provider = "Esri.OceanBasemap")`.
- [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md)
  gains `interactive` (default `TRUE`);
  `options(CWP.dataset.interactive = FALSE)` does the same outside it.
- {leaflet}, {leafsync} and {htmltools} are in Imports, so they are
  installed with the package.

### Deferred plots

- [`comprehensive_cwp_dataframe_analysis()`](https://bastienird.github.io/CWP.dataset/reference/comprehensive_cwp_dataframe_analysis.md)
  no longer draws the plots: its result holds deferred plots, i.e. the
  small aggregated tables and the name of the internal function that
  draws each plot. They are drawn when the report is rendered
  ([`knitting_plots_subfigures()`](https://bastienird.github.io/CWP.dataset/reference/knitting_plots_subfigures.md),
  [`render_subfigures()`](https://bastienird.github.io/CWP.dataset/reference/render_subfigures.md)
  and the templates do it), or with the new
  [`cwp_materialise_plot()`](https://bastienird.github.io/CWP.dataset/reference/cwp_materialise_plot.md).
  Saved results are plain data: much lighter, cheaper to load at
  rendering, and no longer tied to the version of ggplot2 or cowplot
  they were created with.
- `deferred_plots = FALSE`, or
  `options(CWP.dataset.deferred_plots = FALSE)`, gives plot objects as
  before. The functions that build the plots
  ([`compare_temporal_differences()`](https://bastienird.github.io/CWP.dataset/reference/compare_temporal_differences.md),
  [`time_coverage_analysis()`](https://bastienird.github.io/CWP.dataset/reference/time_coverage_analysis.md),
  [`combined_summary_histogram_function()`](https://bastienird.github.io/CWP.dataset/reference/combined_summary_histogram_function.md),
  [`geographic_diff()`](https://bastienird.github.io/CWP.dataset/reference/geographic_diff.md),
  [`fonction_empreinte_spatiale()`](https://bastienird.github.io/CWP.dataset/reference/fonction_empreinte_spatiale.md),
  [`spatial_coverage_analysis()`](https://bastienird.github.io/CWP.dataset/reference/spatial_coverage_analysis.md),
  [`pie_chart_2_default()`](https://bastienird.github.io/CWP.dataset/reference/pie_chart_2_default.md),
  [`other_dimension_analysis()`](https://bastienird.github.io/CWP.dataset/reference/other_dimension_analysis.md))
  gain a `deferred` argument, `FALSE` by default.
- Deferred maps refer to the continent layer of the package instead of
  carrying one copy each.
- Not deferred: maps drawn with tmap, and the plots of
  [`process_fisheries_data()`](https://bastienird.github.io/CWP.dataset/reference/process_fisheries_data.md).

### Report rendering

- [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md)
  gains `render_pdf` (default `FALSE`): the HTML report, which only
  needs pandoc, is always rendered; the PDF is rendered only when asked
  for and when `lualatex` is available. The availability check used to
  look for `pdflatex`, which is not the engine of the report.

### Storage format

- The package no longer depends on {qs}, which moves from Imports to
  Suggests. Report results (lists of results, environments) are saved as
  `.rds` through the new
  [`cwp_save_object()`](https://bastienird.github.io/CWP.dataset/reference/cwp_save_object.md)
  and
  [`cwp_read_object()`](https://bastienird.github.io/CWP.dataset/reference/cwp_save_object.md);
  tables go to Parquet. `.qs` files from earlier versions are still read
  when {qs} is installed.
- Result files are renamed from `.qs` to `.rds`, so results cached by an
  earlier version (`usesave = TRUE`) are computed again.
- `enrich_dataset_if_needed(save_prefix = )` writes
  `<prefix>_with_geom.rds`.
- [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md)
  no longer writes a `UN_CONTINENT2` copy of the continent layer in the
  working directory.
- The continent layer shipped with the package is read from
  `inst/extdata/continent.rds` when it exists; run
  `data-raw/convert_continent.R` once to convert it.
- The dataset of each processing step is now saved as Parquet
  (`data.parquet`, with {nanoparquet}) instead of `data.qs`: an open,
  stable format, readable outside R.
  [`function_recap_each_step()`](https://bastienird.github.io/CWP.dataset/reference/function_recap_each_step.md)
  and
  [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md)
  write it; every reader goes through the same internal helpers.
- Steps written as `data.qs` by earlier versions are still read, so
  existing jobs keep working. If a table cannot be written as Parquet,
  it is saved as `data.rds` with a warning.
- [`read_data()`](https://bastienird.github.io/CWP.dataset/reference/read_data.md)
  reads `.parquet` files.
- [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md)
  orders the steps by the date of their oldest dataset file, and the
  untouched copy kept at enrichment keeps the date of the original, so
  the order survives an interrupted run.

### Performance

- [`enrich_dataset_if_needed()`](https://bastienird.github.io/CWP.dataset/reference/enrich_dataset_if_needed.md)
  gains a `with_geom` argument. With `with_geom = FALSE` the grid WKT is
  not parsed and no polygon is attached to the rows;
  [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md)
  and
  [`process_fisheries_data()`](https://bastienird.github.io/CWP.dataset/reference/process_fisheries_data.md)
  now use it since they only keep `without_geom`.
- Codelists and the packaged CWP grid are read once per session instead
  of on every call.
- [`function_multiple_comparison()`](https://bastienird.github.io/CWP.dataset/reference/function_multiple_comparison.md)
  no longer reads each `data.qs` twice.
- [`groupping_differences()`](https://bastienird.github.io/CWP.dataset/reference/groupping_differences.md)
  and
  [`compare_temporal_differences()`](https://bastienird.github.io/CWP.dataset/reference/compare_temporal_differences.md)
  convert the datasets to `data.table` once instead of once per
  dimension.
- [`enrich_dataset_if_needed()`](https://bastienird.github.io/CWP.dataset/reference/enrich_dataset_if_needed.md)
  no longer calls [`library()`](https://rdrr.io/r/base/library.html).
- [`geographic_diff()`](https://bastienird.github.io/CWP.dataset/reference/geographic_diff.md)
  and
  [`fonction_empreinte_spatiale()`](https://bastienird.github.io/CWP.dataset/reference/fonction_empreinte_spatiale.md)
  (hence the spatial coverage maps) draw the grid cells as ggplot2 tiles
  instead of one tmap polygon per cell. These maps are static: set
  `options(CWP.dataset.map_engine = "tmap")`, or `map_engine = "tmap"`,
  to get the previous tmap maps back (interactive in HTML output).

### Lighter saved results

- Functions that receive or read a full dataset and return plots
  ([`comprehensive_cwp_dataframe_analysis()`](https://bastienird.github.io/CWP.dataset/reference/comprehensive_cwp_dataframe_analysis.md),
  [`function_multiple_comparison()`](https://bastienird.github.io/CWP.dataset/reference/function_multiple_comparison.md),
  [`compare_temporal_differences()`](https://bastienird.github.io/CWP.dataset/reference/compare_temporal_differences.md),
  [`pie_chart_2_default()`](https://bastienird.github.io/CWP.dataset/reference/pie_chart_2_default.md),
  [`combined_summary_histogram_function()`](https://bastienird.github.io/CWP.dataset/reference/combined_summary_histogram_function.md),
  [`geographic_diff()`](https://bastienird.github.io/CWP.dataset/reference/geographic_diff.md),
  [`fonction_empreinte_spatiale()`](https://bastienird.github.io/CWP.dataset/reference/fonction_empreinte_spatiale.md),
  [`process_fisheries_data()`](https://bastienird.github.io/CWP.dataset/reference/process_fisheries_data.md),
  [`process_fisheries_effort_data()`](https://bastienird.github.io/CWP.dataset/reference/process_fisheries_effort_data.md))
  now drop the datasets from their environment when they exit. A ggplot
  keeps references to the environment it was built in, so saved results
  (`.qs`) embedded the full datasets with each plot.

### Bug fixes

- [`time_coverage_analysis()`](https://bastienird.github.io/CWP.dataset/reference/time_coverage_analysis.md):
  `&&` was used on a vector, which is an error from R 4.3.

- Removed several tidyselect deprecation warnings (`all_of()`).

- [`pie_chart_2_default()`](https://bastienird.github.io/CWP.dataset/reference/pie_chart_2_default.md):
  the “same distribution” check now matches classes by name and unit
  instead of by position (it compared vectors of different lengths or
  orders).

- [`pie_chart_2_default()`](https://bastienird.github.io/CWP.dataset/reference/pie_chart_2_default.md):
  appearing / disappearing strata were computed by comparing the first
  dataset with itself, so none was ever reported; the second dataset is
  now used.

- [`pie_chart_2_default()`](https://bastienird.github.io/CWP.dataset/reference/pie_chart_2_default.md):
  no more palette warning with fewer than 3 classes.

- [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md):
  the `sizepdf = "short"` cache fallback now reads the `long` render
  environment (it pointed to a file that did not exist).

- [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md):
  entities are matched to their directory by identifier, with a fallback
  on position, instead of by position only.

- [`fonction_groupement()`](https://bastienird.github.io/CWP.dataset/reference/fonction_groupement.md):
  `number_lines1` / `number_lines2` and the difference in number of
  lines are 0-based instead of `NA` for strata missing from one dataset.

- Cache messages in
  [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md)
  are now actually logged.

## CWP.dataset 0.0.1

### Changelog

- Correction des conflits d’import (`data.table` vs `dplyr`)
- Ajout d’une gestion conditionnelle pour `dygraphs`
