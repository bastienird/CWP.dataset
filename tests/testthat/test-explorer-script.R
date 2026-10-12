make_steps <- function(n) {
  job_dir <- tempfile("job_")
  step_dirs <- file.path(job_dir, "Markdown", paste0("step", seq_len(n)))
  for (step_dir in step_dirs) {
    dir.create(step_dir, recursive = TRUE)
    cwp_write_step_data(data.frame(species = "YFT", measurement_unit = "t",
                                   time_start = "2000-01-01", measurement_value = 1), step_dir)
  }
  list(job_dir = job_dir, step_dirs = step_dirs)
}

# Runs the script without launching the application: returns the app object
run_script <- function(script, wd) {
  old_wd <- setwd(wd)
  on.exit(setwd(old_wd), add = TRUE)
  exprs <- parse(script)
  env <- new.env()
  for (expr in exprs[-length(exprs)]) eval(expr, env)
  env
}

test_that("the script explores the first and the last steps", {
  job <- make_steps(3)
  script <- file.path(job$job_dir, "explorer.R")
  cwp_write_explorer_script(job$step_dirs, script, title = "my_dataset")

  lines <- readLines(script)
  expect_match(lines[1], "my_dataset", fixed = TRUE)
  expect_true(any(grepl('^  "step1" = "Markdown/step1",$', lines)))
  expect_true(any(grepl('^  # "step2" = "Markdown/step2",$', lines)))
  expect_true(any(grepl('^  "step3" = "Markdown/step3"$', lines)))
  expect_equal(tail(lines, 1), "shiny::runApp(app)")
  expect_false(any(grepl("selected", lines)))

  skip_if_not_installed("shiny")
  env <- run_script(script, job$job_dir)
  expect_equal(names(env$paths), c("step1", "step3"))
  expect_true(all(dir.exists(unlist(env$paths))))
  expect_s3_class(env$app, "shiny.appobj")
})

test_that("the script finds the steps from another working directory", {
  skip_if_not_installed("shiny")
  job <- make_steps(2)
  script <- file.path(job$job_dir, "explorer.R")
  cwp_write_explorer_script(job$step_dirs, script)
  env <- run_script(script, tempdir())
  expect_true(all(dir.exists(unlist(env$paths))))
})

test_that("the filters of the report are selected when the explorer opens", {
  job <- make_steps(1)
  script <- file.path(job$job_dir, "explorer.R")
  cwp_write_explorer_script(job$step_dirs, script,
                            selected = list(source_authority = "IOTC", species = NULL,
                                            fishing_fleet = "all", gear_type = c("01.1", "09.1")))
  lines <- paste(readLines(script), collapse = "\n")
  expect_match(lines, 'list(selected = list(source_authority = "IOTC", gear_type = c("01.1", "09.1")))',
               fixed = TRUE)
  expect_error(parse(script), NA)
  expect_match(lines, '"step1" = "Markdown/step1"\n)', fixed = TRUE)

  expect_error(cwp_write_explorer_script(character(), script), "No step")
})
