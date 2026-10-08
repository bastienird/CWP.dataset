test_that("a step dataset is written as Parquet and read back unchanged", {
  step_dir <- file.path(tempfile("steps"), "step_a")
  dir.create(step_dir, recursive = TRUE)
  df <- data.frame(
    species = c("YFT", "SKJ", NA),
    measurement_unit = c("t", "no", "t"),
    measurement_value = c(1.5, 2, NA),
    lines = c(1L, 2L, 3L),
    time_start = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    stringsAsFactors = FALSE
  )

  path <- cwp_write_step_data(data.table::as.data.table(df), step_dir)

  expect_equal(basename(path), "data.parquet")
  expect_equal(cwp_step_data_path(step_dir), file.path(step_dir, "data.parquet"))

  back <- cwp_read_step_data(step_dir)
  expect_identical(class(back), "data.frame")
  expect_equal(names(back), names(df))
  expect_equal(back$species, df$species)
  expect_equal(back$measurement_unit, df$measurement_unit)
  expect_equal(back$measurement_value, df$measurement_value)
  expect_equal(back$lines, df$lines)
  expect_equal(as.Date(back$time_start), df$time_start)
})

test_that("read_data reads Parquet files", {
  file <- tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(data.frame(a = 1:3, b = c("x", "y", "z")), file)

  back <- read_data(file)
  expect_identical(class(back), "data.frame")
  expect_equal(back$a, 1:3)
  expect_equal(back$b, c("x", "y", "z"))
})

test_that("step datasets written as data.qs by earlier versions are still read", {
  skip_if_not_installed("qs")
  step_dir <- file.path(tempfile("steps"), "old_step")
  dir.create(step_dir, recursive = TRUE)
  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  qs::qsave(df, file.path(step_dir, "data.qs"))

  expect_equal(basename(cwp_step_data_path(step_dir)), "data.qs")
  expect_equal(cwp_read_step_data(step_dir), df)

  # Once a Parquet version exists, it is the one that is read
  cwp_write_step_data(data.frame(a = 9L), step_dir)
  expect_equal(basename(cwp_step_data_path(step_dir)), "data.parquet")
  expect_equal(cwp_read_step_data(step_dir)$a, 9L)
})

test_that("a missing step dataset gives a clear error", {
  empty_dir <- tempfile("empty")
  dir.create(empty_dir)
  expect_error(cwp_step_data_path(empty_dir), "No step dataset")
})

test_that("steps are listed in processing order, whatever their format or name", {
  skip_if_not_installed("qs")
  root <- tempfile("Markdown")
  dirs <- file.path(root, c("zz_first", "aa_second", "mm_third"))
  for (d in dirs) dir.create(d, recursive = TRUE)
  df <- data.frame(a = 1L)

  cwp_write_step_data(df, dirs[1])
  qs::qsave(df, file.path(dirs[2], "data.qs"))
  cwp_write_step_data(df, dirs[3])

  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(file.path(dirs[1], "data.parquet"), t0)
  Sys.setFileTime(file.path(dirs[2], "data.qs"), t0 + 60)
  Sys.setFileTime(file.path(dirs[3], "data.parquet"), t0 + 120)

  expect_equal(basename(cwp_list_step_dirs(root)), c("zz_first", "aa_second", "mm_third"))

  # The first step is enriched later: its dataset is rewritten, but the copy
  # of the original keeps its date, so the order does not change
  file.copy(file.path(dirs[1], "data.parquet"), file.path(dirs[1], "ancient.parquet"),
            copy.date = TRUE)
  Sys.setFileTime(file.path(dirs[1], "data.parquet"), t0 + 600)
  expect_equal(basename(cwp_list_step_dirs(root)), c("zz_first", "aa_second", "mm_third"))

  # A directory holding only a copy is not a step
  only_copy <- file.path(root, "only_copy")
  dir.create(only_copy)
  file.copy(file.path(dirs[1], "ancient.parquet"), file.path(only_copy, "ancient.parquet"))
  expect_false("only_copy" %in% basename(cwp_list_step_dirs(root)))

  expect_equal(cwp_list_step_dirs(tempfile("nothing")), character())
})

test_that("a table Parquet cannot store is saved as data.rds, with a warning", {
  step_dir <- file.path(tempfile("steps"), "odd_step")
  dir.create(step_dir, recursive = TRUE)
  odd <- data.frame(a = 1:2)
  odd$nested <- list(list(1, "a"), list(2, "b"))

  warned <- FALSE
  path <- withCallingHandlers(
    cwp_write_step_data(odd, step_dir),
    warning = function(w) {
      warned <<- grepl("saved as data.rds", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  skip_if(basename(path) == "data.parquet", "nanoparquet can store this table")

  expect_true(warned)
  expect_equal(basename(path), "data.rds")
  expect_equal(basename(cwp_step_data_path(step_dir)), "data.rds")
  expect_equal(cwp_read_step_data(step_dir)$a, 1:2)
})
