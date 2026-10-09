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
if (!file.exists(file.path(report_dir, "index.html"))) {
  stop("The example report was not rendered: no index.html in ", report_dir)
}

unlink(output_dir, recursive = TRUE)
dir.create(output_dir, recursive = TRUE)
copied <- file.copy(list.files(report_dir, full.names = TRUE), output_dir, recursive = TRUE)
if (!all(copied)) stop("Some files of the report could not be copied to ", output_dir)

pages <- list.files(output_dir, pattern = "\\.html$")
size_mb <- sum(file.info(list.files(output_dir, recursive = TRUE, full.names = TRUE))$size) / 1e6
message("Example report written to ", output_dir, ": ", length(pages), " pages, ",
        round(size_mb, 1), " MB")
