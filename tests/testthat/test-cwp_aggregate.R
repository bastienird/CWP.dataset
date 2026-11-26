test_that("1 degree squares are grouped into their 5 degree square", {
  expect_equal(
    cwp_grid_to_5deg(c("5212124", "5100003", "5323179", "6230150", "IOTC_WEST", NA)),
    c("6210120", "6100000", "6320175", "6230150", "IOTC_WEST", NA)
  )
})

test_that("cwp_aggregate() sums by year and 5 degree square and keeps the totals", {
  d <- data.frame(
    source_authority = c("IOTC", "IOTC", "IOTC", "WCPFC"),
    species = c("YFT", "YFT", "SKJ", "YFT"),
    time_start = c("2020-01-01", "2020-06-01", "2020-01-01", "2021-03-01"),
    time_end = c("2020-01-31", "2020-06-30", "2020-01-31", "2021-03-31"),
    geographic_identifier = c("5100050", "5101051", "5100050", "6230150"),
    measurement_unit = "t", measurement_value = c(1, 2, 4, 5)
  )
  a <- cwp_aggregate(d)
  expect_equal(sum(a$measurement_value), sum(d$measurement_value))
  expect_equal(sum(a$n_rows), nrow(d))
  expect_false("time_end" %in% names(a))
  yft <- a[species == "YFT" & source_authority == "IOTC"]
  expect_equal(nrow(yft), 1L)
  expect_equal(yft$time_start, "2020-01-01")
  expect_equal(yft$geographic_identifier, "6100050")
  expect_equal(yft$measurement_value, 3)
  expect_equal(yft$n_rows, 2L)
})

test_that("cwp_aggregate() can keep months and geographic identifiers", {
  d <- data.frame(
    time_start = c("2020-01-01", "2020-01-15", "2020-02-01"),
    geographic_identifier = c("5100050", "5100050", "5101051"),
    measurement_unit = "t", measurement_value = c(1, 2, 3)
  )
  a <- cwp_aggregate(d, time_resolution = "month", spatial_resolution = NULL)
  expect_setequal(a$time_start, c("2020-01-01", "2020-02-01"))
  expect_setequal(a$geographic_identifier, c("5100050", "5101051"))
  expect_error(cwp_aggregate(d, spatial_resolution = 1), "5 or NULL")
})

test_that("cwp_aggregate() uses the year column when there is no time_start", {
  d <- data.frame(year = c(2020, 2020), measurement_unit = "t", measurement_value = c(1, 2))
  a <- cwp_aggregate(d)
  expect_equal(a$time_start, "2020-01-01")
  expect_equal(a$measurement_value, 3)
})
