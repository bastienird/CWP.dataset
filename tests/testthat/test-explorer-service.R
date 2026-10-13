make_job <- function() {
  root <- tempfile("jobs_")
  step_dirs <- file.path(root, "entities", "my_dataset", "Markdown", c("rawdata", "final"))
  for (step_dir in step_dirs) {
    dir.create(step_dir, recursive = TRUE)
    cwp_write_step_data(data.frame(source_authority = c("IOTC", "ICCAT"), species = "YFT",
                                   measurement_unit = "t", time_start = "2000-01-01",
                                   measurement_value = c(1, 2)), step_dir)
  }
  list(root = root, step_dirs = step_dirs)
}

test_that("a link gives the steps relative to the folder served and the filters", {
  skip_if_not_installed("shiny")
  job <- make_job()
  url <- cwp_explorer_link(job$step_dirs, job$root,
                           selected = list(source_authority = "IOTC", species = NULL,
                                           fishing_fleet = "all", gear_type = c("01.1", "09.1")),
                           base_url = "http://127.0.0.1:8765")
  expect_match(url, "^http://127\\.0\\.0\\.1:8765/\\?steps=")
  query <- shiny::parseQueryString(sub("^[^?]*\\?", "", url))
  expect_equal(query$steps, "entities/my_dataset/Markdown/rawdata,entities/my_dataset/Markdown/final")
  expect_equal(query$source_authority, "IOTC")
  expect_equal(query$gear_type, "01.1,09.1")
  expect_null(query$species)
  expect_null(query$fishing_fleet)

  expect_error(cwp_explorer_link(job$step_dirs, file.path(job$root, "entities", "other")),
               "not inside")
})

test_that("the address of the service can be configured", {
  old <- options(CWP.dataset.explorer_url = NULL, CWP.dataset.explorer_port = 9000L)
  on.exit(options(old), add = TRUE)
  expect_equal(cwp_explorer_base_url(), "http://127.0.0.1:9000")
  options(CWP.dataset.explorer_url = "https://explorer.example.org/")
  expect_equal(cwp_explorer_base_url(), "https://explorer.example.org")
})

test_that("the service reads the steps and the filters of a link", {
  job <- make_job()
  root <- normalizePath(job$root, winslash = "/")
  request <- cwp_explorer_request(
    list(steps = "entities/my_dataset/Markdown/rawdata,entities/my_dataset/Markdown/final",
         source_authority = "IOTC,ICCAT", not_a_column = "x"),
    root
  )
  expect_equal(levels(request$data$.dataset), c("rawdata", "final"))
  expect_equal(request$selected, list(source_authority = c("IOTC", "ICCAT")))
  expect_true(all(c("source_authority", "species") %in% request$filters))
})

test_that("the service only serves the steps inside its folder", {
  job <- make_job()
  root <- normalizePath(file.path(job$root, "entities"), winslash = "/")
  expect_error(cwp_explorer_request(list(), root), "No step")
  expect_error(cwp_explorer_request(list(steps = "my_dataset/Markdown/missing"), root), "not found")
  # A step outside the folder, even when it exists
  outside <- file.path(tempdir(), basename(tempfile("outside_")))
  dir.create(outside)
  expect_error(cwp_explorer_request(list(steps = file.path("..", "..", basename(outside))), root),
               "not found")
})

test_that("the service shows the explorer of the link, or a message", {
  skip_if_not_installed("shiny")
  job <- make_job()
  app <- cwp_explorer_service_app(job$root)
  expect_s3_class(app, "shiny.appobj")

  service <- cwp_explorer_service_parts(job$root)
  page <- function(query) as.character(service$ui(list(QUERY_STRING = query)))
  expect_match(page(""), "Open it from the link", fixed = TRUE)
  expect_match(page("?steps=nowhere"), "Step not found", fixed = TRUE)
  explorer <- page("?steps=entities/my_dataset/Markdown/rawdata&source_authority=IOTC")
  expect_match(explorer, '<option value="IOTC" selected>', fixed = TRUE)
})

test_that("the report box links to the explorer and explains how to start it", {
  html <- cwp_explorer_link_html(list(url = "http://127.0.0.1:8765/?steps=a%2Cb&species=YFT",
                                      base_url = "http://127.0.0.1:8765",
                                      root = "/jobs/my_job", script = "explorer.R"))
  expect_match(html, 'href="http://127.0.0.1:8765/?steps=a%2Cb&amp;species=YFT"', fixed = TRUE)
  expect_match(html, 'data-base="http://127.0.0.1:8765"', fixed = TRUE)
  expect_match(html, 'cwp_explorer_service("/jobs/my_job")', fixed = TRUE)
  expect_match(html, "explorer.R", fixed = TRUE)
  expect_false(grepl("explorer.R", cwp_explorer_link_html(list(url = "u", base_url = "b",
                                                               root = "r", script = NULL)),
                     fixed = TRUE))
})

test_that("the box is added once at the top of each page of a report", {
  report_dir <- tempfile("report_")
  dir.create(report_dir)
  writeLines(c("<html><body>", "<div class=\"book\">", "<div class=\"page-inner\">",
               "<section class=\"normal\">Text</section>", "</div></div></body></html>"),
             file.path(report_dir, "chapter-1.html"))
  writeLines(c("<html>", "<body>", "<p>Other</p>", "</body></html>"),
             file.path(report_dir, "other.html"))
  info_file <- file.path(report_dir, "explorer.rds")
  cwp_save_object(list(url = "http://127.0.0.1:8765/?steps=a", base_url = "http://127.0.0.1:8765",
                       root = "/jobs", script = "explorer.R"), info_file)

  expect_equal(cwp_add_explorer_box(report_dir, info_file), 2L)
  page <- readLines(file.path(report_dir, "chapter-1.html"))
  expect_equal(grep("cwp-explorer\"", page, fixed = TRUE),
               grep("page-inner", page, fixed = TRUE) + 1L)
  expect_true(any(grepl("cwp-explorer\"", readLines(file.path(report_dir, "other.html")),
                        fixed = TRUE)))
  # Not added twice
  expect_equal(cwp_add_explorer_box(report_dir, info_file), 0L)
  expect_equal(cwp_add_explorer_box(report_dir, NULL), 0L)
})
