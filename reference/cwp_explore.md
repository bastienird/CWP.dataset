# Explore CWP datasets interactively

Opens a Shiny application to filter one or several CWP datasets on the
fly (for example on a species, a fishing fleet and/or a tRFMO) and see
the result as time series, tables and maps. The totals are recomputed
for every selection, so they are always exact. With several datasets, a
tab shows the differences between two of them, by dimension and by cell
(by default the first and the last one).

## Usage

``` r
cwp_explore(..., filters = NULL, selected = list())
```

## Arguments

- ...:

  The datasets to explore: data.frames, paths of step directories (as in
  `Markdown/<step>`) or paths of data files (`.parquet`, `.rds`,
  `.csv`). Name them to choose the names shown in the application, for
  example
  `cwp_explore(init = "Markdown/rawdata", final = "Markdown/step5")`.

- filters:

  Columns offered as filters. By default `source_authority`, `species`,
  `fishing_fleet`, `gear_type` and `fishing_mode`, when present.

- selected:

  Values selected in the filters when the application opens, as a named
  list, for example `list(source_authority = "IOTC")`. A column listed
  here is offered as a filter even if it is not in `filters`.

## Value

A Shiny application. Printing it (at the console) launches it; it can
also be given to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

The reports stay the reference document of each processing step; this
application is for looking at the data in more detail.

## Examples

``` r
if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
  cwp_explore(init = parameter_init_catch, final = parameter_final_catch)
}
```
