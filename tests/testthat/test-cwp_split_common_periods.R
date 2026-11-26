test_that("cwp_split_common_periods() keeps the years of each source present in both datasets", {
  old <- data.frame(
    source_authority = c("IOTC", "IOTC", "WCPFC"),
    time_start = c("2019-01-01", "2020-01-01", "2020-01-01"),
    measurement_unit = "t", measurement_value = c(1, 2, 4)
  )
  new <- data.frame(
    source_authority = c("IOTC", "IOTC", "WCPFC", "WCPFC"),
    time_start = c("2020-01-01", "2021-01-01", "2018-01-01", "2020-06-01"),
    measurement_unit = "t", measurement_value = c(2, 3, 7, 5)
  )
  r <- cwp_split_common_periods(old, new, titre_1 = "old", titre_2 = "new")

  # Common part: 2020 for both sources, every row of 2020 kept
  expect_setequal(substr(r$init$time_start, 1, 4), "2020")
  expect_setequal(substr(r$final$time_start, 1, 4), "2020")
  expect_equal(sum(r$final$measurement_value), 2 + 5)
  expect_false(".year" %in% names(r$init))

  p <- r$periods
  expect_equal(p[source_authority == "IOTC"][["Only in old"]], "2019")
  expect_equal(p[source_authority == "IOTC"][["Only in new"]], "2021")
  expect_equal(p[source_authority == "WCPFC"][["Only in new"]], "2018")
  expect_equal(p[source_authority == "WCPFC"][["Common years"]], "2020")

  u <- r$uncommon
  expect_equal(u$dataset, c("old", "new", "new"))
  expect_equal(u$year, c(2019L, 2021L, 2018L))
  expect_equal(u$measurement_value, c(1, 3, 7))
})

test_that("a source present in one dataset only has no common year", {
  old <- data.frame(source_authority = "IOTC", time_start = "2020-01-01", measurement_unit = "t", measurement_value = 1)
  new <- data.frame(source_authority = c("IOTC", "CCSBT"), time_start = "2020-01-01", measurement_unit = "t", measurement_value = c(1, 9))
  r <- cwp_split_common_periods(old, new)
  expect_equal(r$periods[source_authority == "CCSBT"][["Common years"]], "")
  expect_equal(nrow(r$final), 1L)
  expect_equal(r$uncommon$source_authority, "CCSBT")
})

test_that("year ranges are compact", {
  expect_equal(cwp_year_ranges(c(1990, 1950:1952, 1960)), "1950-1952, 1960, 1990")
  expect_equal(cwp_year_ranges(integer(0)), "")
})
