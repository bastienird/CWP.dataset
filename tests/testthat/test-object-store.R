test_that("objects are saved as .rds and read back unchanged", {
  file <- tempfile(fileext = ".rds")
  results <- list(table = data.frame(a = 1:2, b = c("x", "y")), text = "abc", nothing = NULL)

  expect_equal(cwp_save_object(results, file), file)
  expect_true(file.exists(file))
  expect_equal(cwp_read_object(file), results)
  expect_equal(readRDS(file), results)
})

test_that("an environment of results survives the round trip", {
  file <- tempfile(fileext = ".rds")
  env <- new.env(parent = emptyenv())
  env$fig.path <- "figures"
  env$all_list <- list("step_01.rds", "step_02.rds")

  cwp_save_object(env, file)
  back <- cwp_read_object(file)

  expect_true(is.environment(back))
  expect_equal(sort(ls(back)), c("all_list", "fig.path"))
  expect_equal(back$all_list, env$all_list)
})

test_that(".qs files from earlier versions are still read when qs is installed", {
  skip_if_not_installed("qs")
  file <- tempfile(fileext = ".qs")
  getExportedValue("qs", "qsave")(list(a = 1), file)

  expect_equal(cwp_read_object(file), list(a = 1))
  expect_equal(read_data(file), list(a = 1))
})

test_that("the continent layer of the package can be loaded", {
  continent <- cwp_default_continent()
  expect_s3_class(continent, "sf")
  expect_gt(nrow(continent), 0)
  expect_identical(cwp_continent_layer(NULL), continent)
})
