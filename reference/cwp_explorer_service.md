# Serve the explorer for the reports of a folder

Starts the explorer as a service, so that the links of the HTML reports
open it directly on their data. A link gives the steps to explore and
the filters to select in its address, for example
`http://127.0.0.1:8765/?steps=entities/my_dataset/Markdown/rawdata,entities/my_dataset/Markdown/final&source_authority=IOTC`.

## Usage

``` r
cwp_explorer_service(
  root = ".",
  port = cwp_explorer_port(),
  host = "127.0.0.1",
  launch.browser = FALSE
)
```

## Arguments

- root:

  Folder served: only the steps inside it can be explored. The paths in
  the links are relative to it. For the reports of
  [`summarising_step()`](https://bastienird.github.io/CWP.dataset/reference/summarising_step.md),
  the folder of the job (its `main_dir`).

- port, host:

  Where the service listens. The default port is the one of the links,
  `options(CWP.dataset.explorer_port = )` to change it. Use
  `host = "0.0.0.0"` on a server.

- launch.browser:

  Open the start page in a browser.

## Value

Does not return while the service runs.

## Details

Run it on your computer (the default address is the one the reports link
to), or on a server (RStudio Server, a Docker container...) with access
to the folder of the jobs. In that case, render the reports with
`options(CWP.dataset.explorer_url = "<address of the service>", CWP.dataset.explorer_root = "<folder served>")`
so that their links point to it.

## Examples

``` r
if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
  cwp_explorer_service("path/to/the/job")
}
```
