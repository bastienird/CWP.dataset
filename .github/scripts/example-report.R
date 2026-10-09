# Render the example report: the HTML report of a tiny two-step job built from
# the example datasets of the package (the same job as the end-to-end test).
#
# Usage, from the package root, with CWP.dataset installed:
#   Rscript .github/scripts/example-report.R [output directory]
# The output directory defaults to "example-report"; open its index.html.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1) args[1] else "example-report"
output_dir <- file.path(normalizePath(dirname(output_dir), mustWork = TRUE), basename(output_dir))

library(CWP.dataset)
source(file.path("tests", "testthat", "helper-mini-job.R"))

utils::data("parameter_init_catch", package = "CWP.dataset", envir = environment())
utils::data("parameter_final_catch", package = "CWP.dataset", envir = environment())

job <- create_mini_job(
  list(step1_raw = parameter_init_catch, step2_processed = parameter_final_catch),
  entity_id = "example_catch_dataset"
)

old_wd <- getwd()
summarising_step(
  main_dir = job$main_dir,
  connectionDB = NULL,
  config = job$config,
  source_authoritylist = "all",
  sizepdf = "long",
  render_pdf = FALSE
)
setwd(old_wd)

report_dir <- file.path(job$entity_dir, "longallrecappdf")
if (length(list.files(report_dir, pattern = "\\.html$")) == 0) {
  stop("The example report was not rendered: no HTML page in ", report_dir)
}

unlink(output_dir, recursive = TRUE)
dir.create(output_dir, recursive = TRUE)
copied <- file.copy(list.files(report_dir, full.names = TRUE), output_dir, recursive = TRUE)
if (!all(copied)) stop("Some files of the report could not be copied to ", output_dir)

# The pages are named after the chapters of the report. An index.html pointing
# to the first one gives the report a fixed address.
pages <- list.files(output_dir, pattern = "\\.html$")
if (!"index.html" %in% pages) {
  has_previous <- vapply(pages, function(page) {
    any(grepl("navigation-prev", readLines(file.path(output_dir, page), warn = FALSE), fixed = TRUE))
  }, logical(1))
  first_page <- if (any(!has_previous)) pages[!has_previous][1] else pages[1]
  writeLines(c(
    "<!DOCTYPE html>",
    "<html><head>",
    "<meta charset=\"utf-8\">",
    sprintf("<meta http-equiv=\"refresh\" content=\"0; url=%s\">", first_page),
    "<title>Example report</title>",
    "</head><body>",
    sprintf("<p><a href=\"%s\">Open the example report</a></p>", first_page),
    "</body></html>"
  ), file.path(output_dir, "index.html"))
  message("First page of the report: ", first_page)
}
size_mb <- sum(file.info(list.files(output_dir, recursive = TRUE, full.names = TRUE))$size) / 1e6
message("Example report written to ", output_dir, ": ", length(pages), " pages, ",
        round(size_mb, 1), " MB")
