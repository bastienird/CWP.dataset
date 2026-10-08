test_that("fonction_groupement gives the same result for data.frame and data.table inputs", {
  init <- data.frame(
    species = c("YFT", "SKJ"),
    measurement_unit = "t",
    measurement_value = c(10, 5),
    stringsAsFactors = FALSE
  )
  final <- init[init$species == "YFT", ]

  from_df <- as.data.frame(fonction_groupement("species", init, final))
  from_dt <- as.data.frame(fonction_groupement(
    "species", data.table::as.data.table(init), data.table::as.data.table(final)
  ))

  expect_equal(from_df, from_dt)
})

test_that("fonction_groupement counts zero lines for strata missing from one dataset", {
  init <- data.frame(
    species = c("YFT", "SKJ"),
    measurement_unit = "t",
    measurement_value = c(10, 5),
    stringsAsFactors = FALSE
  )
  final <- init[init$species == "YFT", ]

  res <- as.data.frame(fonction_groupement("species", init, final))
  lost <- res[res$Precision == "SKJ", ]

  expect_false(anyNA(res$number_lines1))
  expect_false(anyNA(res$number_lines2))
  expect_equal(lost$number_lines2, 0)
  expect_equal(lost[["Difference in number of lines"]], -1)
})

test_that("enrich_dataset_if_needed(with_geom = FALSE) adds labels and gridtype without geometry", {
  df <- data.frame(
    species = c("YFT", "SKJ"),
    measurement_unit = c("Tons", "Number of fish"),
    measurement_value = c(1, 2),
    geographic_identifier = c("5100000", "5100001"),
    stringsAsFactors = FALSE
  )

  res <- suppressMessages(enrich_dataset_if_needed(df, with_geom = FALSE))

  expect_null(res$with_geom)
  expect_s3_class(res$without_geom, "data.frame")
  expect_false(inherits(res$without_geom, "sf"))
  expect_equal(nrow(res$without_geom), 2)
  expect_true(all(c("species_label", "gridtype") %in% names(res$without_geom)))
  expect_false("geom" %in% names(res$without_geom))
  expect_equal(res$without_geom$gridtype, c("1deg_x_1deg", "1deg_x_1deg"))
  expect_equal(res$without_geom$measurement_unit, c("t", "no"))
})

test_that("cwp_grid_tiles describes rectangular cells and rejects other shapes", {
  grid <- sf::st_as_sf(
    data.frame(
      cwp_code = c("5100000", "6100000"),
      geom_wkt = c("MULTIPOLYGON (((0 1, 1 1, 1 0, 0 0, 0 1)))",
                   "MULTIPOLYGON (((0 5, 5 5, 5 0, 0 0, 0 5)))"),
      stringsAsFactors = FALSE
    ),
    wkt = "geom_wkt", crs = 4326
  )

  tiles <- as.data.frame(cwp_grid_tiles(grid, c("6100000", "5100000", "unknown")))
  tiles <- tiles[order(tiles$code), ]
  expect_equal(tiles$code, c("5100000", "6100000"))
  expect_equal(tiles$x, c(0.5, 2.5))
  expect_equal(tiles$y, c(0.5, 2.5))
  expect_equal(tiles$width, c(1, 5))
  expect_equal(tiles$height, c(1, 5))

  expect_equal(nrow(cwp_grid_tiles(grid, "unknown")), 0)
  expect_null(cwp_grid_tiles(as.data.frame(grid), "5100000"))

  triangle <- sf::st_as_sf(
    data.frame(cwp_code = "T1", geom_wkt = "POLYGON ((0 0, 1 0, 0 1, 0 0))",
               stringsAsFactors = FALSE),
    wkt = "geom_wkt", crs = 4326
  )
  expect_null(cwp_grid_tiles(triangle, "T1"))
})

test_that("cwp_tile_map builds a ggplot that can be rendered, with or without continent", {
  plot_data <- data.frame(
    x = c(0.5, 2.5), y = c(0.5, 2.5), width = c(1, 5), height = c(1, 5),
    measurement_value = c(10, 20),
    gridtype = c("1deg_x_1deg", "5deg_x_5deg"),
    source = "Dataset 1",
    stringsAsFactors = FALSE
  )
  scale <- ggplot2::scale_fill_gradient2(midpoint = 0)

  map <- cwp_tile_map(plot_data, fill = "measurement_value", facet_rows = "gridtype",
                      facet_cols = "source", fill_scale = scale)
  expect_s3_class(map, "ggplot")
  expect_error(ggplot2::ggplot_build(map), NA)

  land <- sf::st_as_sf(
    data.frame(name = "land", geom_wkt = "POLYGON ((1 1, 4 1, 4 4, 1 4, 1 1))",
               stringsAsFactors = FALSE),
    wkt = "geom_wkt", crs = 4326
  )
  map_land <- cwp_tile_map(plot_data, fill = "measurement_value", facet_rows = "gridtype",
                           facet_cols = "source", fill_scale = scale, continent = land)
  expect_error(ggplot2::ggplot_build(map_land), NA)
})

test_that("cwp_forget removes the listed objects and ignores unknown names", {
  holder <- function() {
    big <- numeric(10)
    kept <- 1
    cwp_forget(c("big", "does_not_exist"), environment())
    ls()
  }
  expect_equal(holder(), "kept")
})

test_that("plots returned by compare_temporal_differences do not carry the input datasets", {
  n <- 1e6
  init <- data.frame(
    time_start = rep(c("2020-01-01", "2021-01-01"), length.out = n),
    measurement_unit = "t",
    measurement_value = 1,
    filler = seq_len(n) + 0.5,
    stringsAsFactors = FALSE
  )
  final <- init
  final$measurement_value <- 2

  res <- compare_temporal_differences("time_start", init, final, "Dataset 1", "Dataset 2")

  expect_s3_class(res$plots[[1]], "ggplot")
  expect_error(ggplot2::ggplot_build(res$plots[[1]]), NA)
  expect_lt(length(serialize(res$plots[[1]], NULL)), as.numeric(object.size(init)) / 5)
})
