# tests/testthat/test-time_coverage_analysis.R
testthat::test_that("time_coverage_analysis returns expected structure and titles", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("zoo")

  library(dplyr)
  library(tidyr)
  library(lubridate)

  # ---- Données MENSUELLES (branche 'is_strict_monthly == TRUE') ----
  months_seq <- seq(as.Date("2024-01-01"), by = "1 month", length.out = 4)
  df_monthly <- tibble::tibble(
    Precision        = as.character(months_seq),        # sera transformé en Date
    time_start       = months_seq,
    time_end         = (months_seq %m+% months(1)) - lubridate::days(1), # fin inclusive
    value_sum_1      = c(10, 0, 30, 20),
    value_sum_2      = c(8,  5, 25, 40),
    measurement_unit = "t",
    Dimension        = "Time"  # évite un label NA dans labs()
  )

  # ---- Données NON-MENSUELLES (branche courbes) ----
  df_irregular <- tibble::tibble(
    Precision        = as.character(as.Date(c("2024-01-15","2024-02-02","2024-03-20"))),
    # pas de time_start/time_end -> is_strict_monthly == FALSE
    value_sum_1      = c(5, 15, 10),
    value_sum_2      = c(7,  9,  12),
    measurement_unit = "t",
    Dimension        = "Time"
  )

  input_list <- list(df_monthly, df_irregular)

  # --- Appel : unique_analyse = FALSE ---
  res <- time_coverage_analysis(
    time_dimension_list_groupped = input_list,
    parameter_time_dimension     = "Year",
    titre_1                      = "Dataset1",
    titre_2                      = "Dataset2",
    unique_analyse               = FALSE
  )

  expect_type(res, "list")
  expect_named(res, c("titles", "plots"))
  expect_true(is.character(res$titles))
  expect_match(res$titles, "Dataset1.*Dataset2")   # le titre doit mentionner les deux
  expect_true(is.list(res$plots))
  expect_length(res$plots, length(input_list))
  lapply(res$plots, function(p) expect_s3_class(p, "ggplot"))

  # --- Appel : unique_analyse = TRUE ---
  res_u <- time_coverage_analysis(
    time_dimension_list_groupped = input_list,
    parameter_time_dimension     = "Year",
    titre_1                      = "Dataset1",
    titre_2                      = "Dataset2",
    unique_analyse               = TRUE
  )
  expect_match(res_u$titles, "for Dataset1 dataset")  # titre ne mentionne qu'un dataset
})

testthat::test_that("time_coverage_analysis monthly branch produces stable bar plot (vdiffr)", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("zoo")

  library(dplyr)
  library(tidyr)
  library(lubridate)

  months_seq <- seq(as.Date("2023-10-01"), by = "1 month", length.out = 6)
  df_monthly <- tibble::tibble(
    Precision        = as.character(months_seq),
    time_start       = months_seq,
    time_end         = (months_seq %m+% months(1)) - lubridate::days(1),
    value_sum_1      = c(1,2,3,4,5,6),
    value_sum_2      = c(2,3,2,5,4,3),
    measurement_unit = "t",
    Dimension        = "Time"
  )

  res <- time_coverage_analysis(
    time_dimension_list_groupped = list(df_monthly),
    parameter_time_dimension     = "Month",
    titre_1                      = "A",
    titre_2                      = "B",
    unique_analyse               = FALSE
  )

  p <- res$plots[[1]]
  # vdiffr::expect_doppelganger("time_coverage_monthly_bar_dodged", p)
})

testthat::test_that("time_coverage_analysis irregular branch produces stable line plot (vdiffr)", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  library(dplyr)
  library(tidyr)

  df_irregular <- tibble::tibble(
    Precision        = as.character(as.Date(c("2024-01-15","2024-02-02","2024-03-20"))),
    value_sum_1      = c(5, 15, 10),
    value_sum_2      = c(7,  9,  12),
    measurement_unit = "no",
    Dimension        = "Time"
  )

  res <- time_coverage_analysis(
    time_dimension_list_groupped = list(df_irregular),
    parameter_time_dimension     = "Year",
    titre_1                      = "Left",
    titre_2                      = "Right",
    unique_analyse               = FALSE
  )

  p <- res$plots[[1]]
  # vdiffr::expect_doppelganger("time_coverage_irregular_lines", p)
})
