# End-to-end test of the report: a two-step job is created, then
# summarising_step() computes the analyses and renders the HTML report. This
# is the only test that goes through the report templates.

test_that("a two-step job is written as Parquet with its summaries", {
  utils::data("parameter_init_catch", envir = environment())
  utils::data("parameter_final_catch", envir = environment())
  job <- create_mini_job(list(step1_raw = parameter_init_catch,
                              step2_processed = parameter_final_catch))

  step_dirs <- cwp_list_step_dirs(file.path(job$entity_dir, "Markdown"))
  expect_equal(basename(step_dirs), c("step1_raw", "step2_processed"))

  for (step_dir in step_dirs) {
    expect_true(file.exists(file.path(step_dir, "data.parquet")))
    expect_true(all(file.exists(file.path(step_dir, c("sums.csv", "explanation.txt", "functions.txt")))))
  }
  first <- cwp_read_step_data(step_dirs[1])
  expect_s3_class(first, "data.frame")
  expect_equal(sum(first$measurement_value, na.rm = TRUE),
               sum(as.numeric(parameter_init_catch$measurement_value), na.rm = TRUE))
  expect_false(exists("explanation_total", envir = .GlobalEnv, inherits = FALSE))
})

test_that("summarising_step renders the HTML report of a two-step job", {
  skip_on_cran()
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is not available")

  utils::data("parameter_init_catch", envir = environment())
  utils::data("parameter_final_catch", envir = environment())
  job <- create_mini_job(list(step1_raw = parameter_init_catch,
                              step2_processed = parameter_final_catch))
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  suppressWarnings(suppressMessages(
    summarising_step(
      main_dir = job$main_dir,
      connectionDB = NULL,
      config = job$config,
      source_authoritylist = "all",
      sizepdf = "middle",
      render_pdf = FALSE
    )
  ))

  # The working directory is restored
  expect_equal(normalizePath(getwd()), normalizePath(old_wd))

  # Each step keeps an untouched copy and an enriched dataset
  step_dirs <- cwp_list_step_dirs(file.path(job$entity_dir, "Markdown"))
  expect_equal(basename(step_dirs), c("step1_raw", "step2_processed"))
  for (step_dir in step_dirs) {
    expect_true(file.exists(file.path(step_dir, "ancient.parquet")))
    expect_true("gridtype" %in% names(cwp_read_step_data(step_dir)))
  }

  # The report
  report_dir <- file.path(job$entity_dir, "middleallrecappdf")
  expect_true(dir.exists(report_dir))
  pages <- list.files(report_dir, pattern = "\\.html$", full.names = TRUE)
  # The pages are named after the chapters of the report, not "index.html"
  expect_gt(length(pages), 0)

  html <- unlist(lapply(pages, readLines, warn = FALSE))
  # The entity and both steps appear in the report
  expect_true(any(grepl("mini_catch_dataset", html, fixed = TRUE)))
  expect_true(any(grepl("step2_processed", html, fixed = TRUE)))
  # No R error message was written in the pages
  expect_false(any(grepl("## Error", html, fixed = TRUE)))
  # No PDF was asked for
  expect_length(list.files(report_dir, pattern = "\\.pdf$"), 0)
})

test_that("the maps of the HTML report are interactive", {
  skip_on_cran()
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is not available")
  skip_if_not_installed("leaflet")

  utils::data("parameter_init_catch", envir = environment())
  utils::data("parameter_final_catch", envir = environment())
  job <- create_mini_job(list(step1_raw = parameter_init_catch,
                              step2_processed = parameter_final_catch))
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  suppressWarnings(suppressMessages(
    summarising_step(job$main_dir, NULL, job$config, source_authoritylist = "all",
                     sizepdf = "middle", render_pdf = FALSE, interactive = TRUE)
  ))
  pages <- list.files(file.path(job$entity_dir, "middleallrecappdf"), pattern = "\\.html$",
                      full.names = TRUE)
  expect_gt(length(pages), 0)
  html <- unlist(lapply(pages, readLines, warn = FALSE))
  expect_true(any(grepl("leaflet", html, fixed = TRUE)))

  # ... and static when interactivity is switched off
  job_static <- create_mini_job(list(step1_raw = parameter_init_catch,
                                     step2_processed = parameter_final_catch))
  suppressWarnings(suppressMessages(
    summarising_step(job_static$main_dir, NULL, job_static$config, source_authoritylist = "all",
                     sizepdf = "middle", render_pdf = FALSE, interactive = FALSE)
  ))
  pages_static <- list.files(file.path(job_static$entity_dir, "middleallrecappdf"),
                             pattern = "\\.html$", full.names = TRUE)
  html_static <- unlist(lapply(pages_static, readLines, warn = FALSE))
  expect_false(any(grepl("html-widget", html_static, fixed = TRUE)))
})
