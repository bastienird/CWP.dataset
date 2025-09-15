test_that("comprehensive_cwp_dataframe_analysis works on effort data", {
  data("parameter_init_effort", envir = environment())
  data("parameter_final_effort", envir = environment())

  result <- comprehensive_cwp_dataframe_analysis(
    parameter_init = parameter_init_effort,
    parameter_final = parameter_final_effort,
    parameter_fact = "effort",
    parameter_short = TRUE,
    outputonly = TRUE,
    print_map = FALSE,
    coverage = TRUE
  )

  expect_type(result, "list")
  expect_true("summary_of_differences" %in% names(result))
  expect_true("compare_strata_differences_list" %in% names(result))
  expect_true(
    is.null(result$compare_strata_differences_list) ||
      inherits(result$compare_strata_differences_list, "list")
  )
})

test_that("comprehensive_cwp_dataframe_analysis works on catch data", {
  utils::data("parameter_init_catch", envir = environment())
  utils::data("parameter_final_catch", envir = environment())

  result <- comprehensive_cwp_dataframe_analysis(
    parameter_init = parameter_init_catch,
    parameter_final = parameter_final_catch,
    parameter_fact = "catch",
    parameter_short = TRUE,
    outputonly = TRUE,
    print_map = FALSE,
    coverage = TRUE
  )
  expect_type(result, "list")
  expect_true("summary_of_differences" %in% names(result))
  expect_true("compare_strata_differences_list" %in% names(result))
  expect_true(
    is.null(result$compare_strata_differences_list) ||
      inherits(result$compare_strata_differences_list, "list")
  )
})

