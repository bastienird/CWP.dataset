map_table <- function() {
  data.frame(
    x = c(0.5, 1.5, 2.5, 7.5), y = c(0.5, 0.5, 2.5, 2.5),
    width = c(1, 1, 5, 5), height = c(1, 1, 5, 5),
    measurement_value = c(10, 2500000, 30, 40),
    gridtype = c("1deg_x_1deg", "1deg_x_1deg", "5deg_x_5deg", "5deg_x_5deg"),
    source = "Dataset 1",
    geographic_identifier = c("5100000", "5100001", "6100000", "6100005"),
    stringsAsFactors = FALSE
  )
}
map_spec <- function() {
  do.call(cwp_deferred_plot, c(
    list("cwp_tile_map"),
    cwp_tile_map_args(map_table(), fill = "measurement_value", facet_rows = "gridtype",
                      facet_cols = "source", fill_scale = "value",
                      popup_cols = c("geographic_identifier", "measurement_value", "not_a_column"))
  ))
}
methods_of <- function(widget) vapply(widget$x$calls, function(call) call$method, character(1))

test_that("the description of a map keeps the columns shown on click", {
  spec <- map_spec()
  expect_equal(spec$args$popup_cols, c("geographic_identifier", "measurement_value"))
  expect_true(all(c("geographic_identifier", "measurement_value") %in% names(spec$args$plot_data)))
})

test_that("a map is static by default and outside an HTML report", {
  expect_false(cwp_interactive_output())
  expect_s3_class(cwp_materialise_plot(map_spec()), "ggplot")
  expect_s3_class(cwp_materialise_plot(map_spec(), interactive = FALSE), "ggplot")
  expect_error(ggplot2::ggplot_build(cwp_materialise_plot(map_spec())), NA)
})

test_that("a map can be drawn as a leaflet widget, one layer per panel", {
  skip_if_not_installed("leaflet")
  widget <- cwp_materialise_plot(map_spec(), interactive = TRUE)

  expect_s3_class(widget, "leaflet")
  expect_s3_class(widget, "htmlwidget")

  methods <- methods_of(widget)
  # Two grid types, one dataset: two layers, named after the grid type only
  expect_equal(sum(methods == "addRectangles"), 2)
  expect_true("addLayersControl" %in% methods)
  expect_true("addLegend" %in% methods)
  expect_true("addProviderTiles" %in% methods)

  control <- widget$x$calls[[which(methods == "addLayersControl")]]
  expect_equal(unlist(control$args[[1]]), c("1deg_x_1deg", "5deg_x_5deg"))

  rectangles <- widget$x$calls[[which(methods == "addRectangles")[1]]]
  expect_true(any(grepl("5100000", unlist(rectangles$args), fixed = TRUE)))
  expect_true(any(grepl("2,500,000", unlist(rectangles$args), fixed = TRUE)))
})

test_that("a map with a single panel has no layer control", {
  skip_if_not_installed("leaflet")
  one_panel <- map_table()[1:2, ]
  widget <- cwp_leaflet_map(one_panel, fill = "measurement_value", facet_rows = "gridtype",
                            facet_cols = "source", fill_scale = "value")
  methods <- methods_of(widget)
  expect_equal(sum(methods == "addRectangles"), 1)
  expect_false("addLayersControl" %in% methods)
})

test_that("the map of differences uses the impact categories", {
  skip_if_not_installed("leaflet")
  impact <- map_table()
  impact$`Impact on the data` <- factor(c("Gain", "Loss", "All data lost", "No differences"),
                                        levels = cwp_impact_levels)
  impact$measurement_unit <- "Tons"
  widget <- cwp_leaflet_map(impact, fill = "Impact on the data", facet_rows = "measurement_unit",
                            facet_cols = "gridtype", fill_scale = "impact")
  expect_s3_class(widget, "leaflet")
  expect_equal(sum(methods_of(widget) == "addRectangles"), 2)
})

test_that("only maps have an interactive version", {
  skip_if_not_installed("leaflet")
  series <- cwp_deferred_plot(
    "cwp_plot_time_coverage",
    x_plot = data.frame(TimeKey = as.Date(c("2020-01-01", "2021-01-01")), Values = c(1, 2),
                        Dataset = "a", measurement_unit = "t", stringsAsFactors = FALSE),
    x_label = "Year"
  )
  expect_s3_class(cwp_materialise_plot(series, interactive = TRUE), "ggplot")
})

test_that("popup text is escaped", {
  expect_equal(cwp_html_escape("<b>a & b</b>"), "&lt;b&gt;a &amp; b&lt;/b&gt;")
})
