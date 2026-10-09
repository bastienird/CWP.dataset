# Every element of a nested list of results satisfying `pred`
collect <- function(x, pred) {
  if (pred(x)) return(list(x))
  if (is.list(x) && !is.object(x)) {
    return(do.call(c, c(list(list()), lapply(unname(x), collect, pred = pred))))
  }
  list()
}
is_spec <- function(x) inherits(x, "cwp_plot_spec")
is_drawn <- function(x) inherits(x, "gg") || inherits(x, "ggplot")

time_table <- function() {
  data.frame(
    TimeKey = as.Date(c("2020-01-01", "2021-01-01", "2020-01-01", "2021-01-01")),
    Values = c(1, 2, 3, 4),
    Dataset = c("a", "a", "b", "b"),
    measurement_unit = "t",
    stringsAsFactors = FALSE
  )
}

test_that("a deferred plot is plain data and draws the same plot as a direct call", {
  spec <- cwp_plot_or_spec(TRUE, "cwp_plot_time_coverage", x_plot = time_table(), x_label = "Year")
  direct <- cwp_plot_or_spec(FALSE, "cwp_plot_time_coverage", x_plot = time_table(), x_label = "Year")

  expect_s3_class(spec, "cwp_plot_spec")
  expect_false(is_drawn(spec))
  expect_equal(spec$fun, "cwp_plot_time_coverage")
  expect_equal(spec$args$x_plot, time_table())

  drawn <- cwp_materialise_plot(spec)
  expect_s3_class(direct, "ggplot")
  expect_s3_class(drawn, "ggplot")
  expect_equal(ggplot2::ggplot_build(drawn)$data[[1]], ggplot2::ggplot_build(direct)$data[[1]])
})

test_that("cwp_materialise_plot leaves anything else unchanged", {
  p <- ggplot2::ggplot()
  expect_identical(cwp_materialise_plot(p), p)
  expect_null(cwp_materialise_plot(NULL))
  expect_equal(cwp_materialise_plot("text"), "text")
})

test_that("comprehensive_cwp_dataframe_analysis returns deferred plots that can all be drawn", {
  utils::data("parameter_init_catch", envir = environment())
  utils::data("parameter_final_catch", envir = environment())

  result <- comprehensive_cwp_dataframe_analysis(
    parameter_init = parameter_init_catch,
    parameter_final = parameter_final_catch,
    parameter_fact = "catch",
    outputonly = TRUE,
    print_map = FALSE,
    coverage = TRUE
  )

  specs <- collect(result, is_spec)
  expect_gt(length(specs), 0)
  expect_length(collect(result, is_drawn), 0)

  for (spec in specs) {
    drawn <- cwp_materialise_plot(spec)
    expect_true(is_drawn(drawn), info = spec$fun)
    expect_error(ggplot2::ggplot_build(drawn), NA, info = spec$fun)
  }

  drawn_result <- cwp_materialise_plots(result)
  expect_length(collect(drawn_result, is_spec), 0)
  expect_equal(length(collect(drawn_result, is_drawn)), length(specs))
  expect_equal(names(drawn_result), names(result))
})

test_that("deferred_plots = FALSE still returns plot objects, and they are heavier", {
  utils::data("parameter_init_catch", envir = environment())
  utils::data("parameter_final_catch", envir = environment())
  run <- function(deferred_plots) {
    comprehensive_cwp_dataframe_analysis(
      parameter_init = parameter_init_catch,
      parameter_final = parameter_final_catch,
      parameter_fact = "catch",
      outputonly = TRUE,
      print_map = FALSE,
      coverage = TRUE,
      deferred_plots = deferred_plots
    )
  }
  with_plots <- run(FALSE)
  with_specs <- run(TRUE)

  expect_length(collect(with_plots, is_spec), 0)
  expect_gt(length(collect(with_plots, is_drawn)), 0)
  expect_equal(length(collect(with_plots, is_drawn)), length(collect(with_specs, is_spec)))
  expect_lt(length(serialize(with_specs, NULL)), length(serialize(with_plots, NULL)))
})

test_that("maps are deferred and can be drawn from the result", {
  utils::data("parameter_init_catch", envir = environment())
  utils::data("parameter_final_catch", envir = environment())
  skip_if_not("geographic_identifier" %in% names(parameter_init_catch))

  codes <- unique(as.character(c(parameter_init_catch$geographic_identifier,
                                 parameter_final_catch$geographic_identifier)))
  grid <- data.table::fread(
    system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset"),
    select = c("CWP_CODE", "GRIDTYPE", "geom_wkt"), colClasses = "character", data.table = FALSE
  )
  grid <- grid[grid$CWP_CODE %in% codes, ]
  skip_if(nrow(grid) == 0, "the example data does not use CWP grid codes")
  shape <- sf::st_as_sf(
    data.frame(cwp_code = grid$CWP_CODE, code = grid$CWP_CODE, GRIDTYPE = grid$GRIDTYPE,
               geom = grid$geom_wkt, stringsAsFactors = FALSE),
    wkt = "geom", crs = 4326
  )

  result <- comprehensive_cwp_dataframe_analysis(
    parameter_init = parameter_init_catch,
    parameter_final = parameter_final_catch,
    parameter_fact = "catch",
    outputonly = TRUE,
    print_map = TRUE,
    shapefile_fix = shape,
    continent = cwp_default_continent(),
    coverage = TRUE
  )

  difference_map <- result$Geographicdiff$plott
  expect_s3_class(difference_map, "cwp_plot_spec")
  expect_equal(difference_map$fun, "cwp_tile_map")
  # The continent layer of the package is referenced, not copied
  expect_s3_class(difference_map$args$continent, "cwp_default_continent")
  expect_error(ggplot2::ggplot_build(cwp_materialise_plot(difference_map)), NA)

  coverage_maps <- Filter(Negate(is.null), result$spatial_coverage_analysis_list$plots)
  expect_gt(length(coverage_maps), 0)
  for (map in coverage_maps) {
    expect_s3_class(map, "cwp_plot_spec")
    expect_error(ggplot2::ggplot_build(cwp_materialise_plot(map)), NA)
  }
})

test_that("only the continent layer of the package is replaced by a reference", {
  expect_s3_class(cwp_continent_ref(cwp_default_continent()), "cwp_default_continent")
  expect_null(cwp_continent_ref(NULL))

  other <- sf::st_as_sf(
    data.frame(name = "land", geom_wkt = "POLYGON ((1 1, 4 1, 4 4, 1 4, 1 1))",
               stringsAsFactors = FALSE),
    wkt = "geom_wkt", crs = 4326
  )
  expect_identical(cwp_continent_ref(other), other)
})
