test_that("optional packages are not imported by the package", {
  optional <- c("tmap", "dygraphs", "xts", "googledrive", "plotrix", "DT", "qs", "tinytex", "zoo", "leaflet")
  imported <- names(getNamespaceImports("CWP.dataset"))

  # If this fails after devtools::document(), a roxygen @import / @importFrom
  # tag for one of these packages is back in the R files
  expect_equal(intersect(optional, imported), character())
})

test_that("a missing optional package gives a clear error", {
  expect_error(
    cwp_require_package("aPackageThatDoesNotExist", "to do something"),
    "'aPackageThatDoesNotExist' package is needed to do something"
  )
  expect_true(cwp_require_package("stats", "to do something"))
})

test_that("time_coverage_analysis does not need zoo", {
  grouped <- list(data.frame(
    Precision = c("2020-01-01", "2021-01-01"),
    measurement_unit = "t",
    value_sum_1 = c(1, 2),
    value_sum_2 = c(2, 3),
    number_lines1 = c(1L, 1L),
    number_lines2 = c(1L, 1L),
    stringsAsFactors = FALSE
  ))

  res <- time_coverage_analysis(grouped, "time_start", "Dataset 1", "Dataset 2")
  expect_length(res$plots, 1)
  expect_error(ggplot2::ggplot_build(res$plots[[1]]), NA)
})

test_that("the Google Drive upload arguments are ignored with a warning", {
  expect_true("upload_drive" %in% names(formals(summarising_invalid_data)))
  expect_true("uploadgoogledrive" %in% names(formals(strata_in_georef_but_not_in_nominal_report_launching)))
  code <- c(deparse(body(summarising_invalid_data)),
            deparse(body(strata_in_georef_but_not_in_nominal_report_launching)))
  expect_false(any(grepl("drive_upload", code, fixed = TRUE)))
})
