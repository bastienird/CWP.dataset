explore_table <- function(values = c(10, 20, 30, 40, 5, 1)) {
  data.frame(
    source_authority = c("IOTC", "IOTC", "ICCAT", "ICCAT", "IOTC", "IOTC"),
    species = c("YFT", "SKJ", "YFT", "BET", "YFT", "YFT"),
    species_label = c("Yellowfin", "Skipjack", "Yellowfin", "Bigeye", "Yellowfin", "Yellowfin"),
    fishing_fleet = factor(c("EUFRA", "EUESP", "EUFRA", "JPN", "EUFRA", "EUFRA")),
    gear_type = c("01.1", "01.1", "09.1", "03.1", "01.1", "01.1"),
    measurement_unit = c("t", "t", "t", "t", "t", "no"),
    time_start = as.Date(c("2000-01-01", "2000-06-01", "2001-01-01", "2001-01-01",
                           "2002-01-01", "2002-01-01")),
    geographic_identifier = c("5100000", "5100001", "6100000", "6100000", "5100000", "5100000"),
    measurement_value = values,
    stringsAsFactors = FALSE
  )
}

explore_data <- function() {
  cwp_explore_datasets(list(init = explore_table(), final = explore_table(c(10, 0, 60, 40, 5, 1))))
}

test_that("the datasets are combined with their names and years", {
  data <- explore_data()
  expect_s3_class(data, "data.table")
  expect_equal(levels(data$.dataset), c("init", "final"))
  expect_equal(nrow(data), 12)
  expect_equal(sort(unique(data$.year)), 2000:2002)
  # Factors and dates become character, to compare with the filters
  expect_type(data$fishing_fleet, "character")
  expect_type(data$time_start, "character")
})

test_that("unnamed datasets get default names and data.tables are not modified", {
  given <- data.table::as.data.table(explore_table())
  data <- cwp_explore_datasets(list(given, explore_table()))
  expect_equal(levels(data$.dataset), c("Dataset 1", "Dataset 2"))
  expect_false(".year" %in% names(given))
})

test_that("a step directory or a data file can be explored", {
  step_dir <- tempfile("step_")
  dir.create(step_dir)
  cwp_write_step_data(explore_table(), step_dir)

  data <- cwp_explore_datasets(list(step_dir, file.path(step_dir, "data.parquet")))
  expect_equal(levels(data$.dataset), c(basename(step_dir), "data.parquet"))
  expect_equal(sum(data$measurement_value), 2 * sum(explore_table()$measurement_value))
})

test_that("the example datasets of the package can be explored", {
  utils::data("parameter_init_catch", "parameter_final_catch", envir = environment())
  data <- cwp_explore_datasets(list(init = parameter_init_catch, final = parameter_final_catch))
  expect_equal(nrow(data), nrow(parameter_init_catch) + nrow(parameter_final_catch))
  expect_true(all(c("species", "fishing_fleet", "source_authority") %in% cwp_explore_dimensions(data)))
  summary <- cwp_explore_summary(data, "source_authority")
  expect_equal(sum(summary$init), sum(parameter_init_catch$measurement_value, na.rm = TRUE))
})

test_that("a dataset without measurement_value is refused", {
  expect_error(cwp_explore_datasets(list(data.frame(a = 1))), "measurement_value")
  expect_error(cwp_explore_datasets(list()), "at least one dataset")
})

test_that("filters show labels and keep the matching rows", {
  data <- explore_data()
  choices <- cwp_explore_choices(data, "species")
  expect_equal(unname(choices), c("BET", "SKJ", "YFT"))
  expect_equal(names(choices), c("BET - Bigeye", "SKJ - Skipjack", "YFT - Yellowfin"))
  expect_equal(cwp_explore_choices(data, "source_authority"), c("ICCAT", "IOTC"))

  expect_equal(nrow(cwp_explore_filter(data)), 12)
  expect_equal(nrow(cwp_explore_filter(data, list(species = NULL))), 12)
  yft <- cwp_explore_filter(data, list(species = "YFT", source_authority = "IOTC"))
  expect_equal(unique(yft$species), "YFT")
  expect_equal(unique(yft$source_authority), "IOTC")
  expect_equal(nrow(yft), 6)

  expect_equal(nrow(cwp_explore_filter(data, years = c(2001, 2001))), 4)
  expect_equal(nrow(cwp_explore_filter(data, list(species = "none"))), 0)
})

test_that("the dimensions leave out labels, codes of cells and technical columns", {
  dimensions <- cwp_explore_dimensions(explore_data())
  expect_true(all(c("species", "fishing_fleet", "source_authority", "gear_type") %in% dimensions))
  expect_false(any(c("species_label", "geographic_identifier", "time_start",
                     "measurement_unit", ".dataset") %in% dimensions))
})

test_that("totals by year are exact and kept apart by unit", {
  totals <- cwp_explore_timeseries(explore_data())
  init_t <- totals[.dataset == "init" & measurement_unit == "t"]
  expect_equal(init_t$.year, 2000:2002)
  expect_equal(init_t$value, c(30, 70, 5))
  expect_equal(totals[.dataset == "final" & .year == 2001 & measurement_unit == "t"]$value, 100)
  expect_equal(totals[measurement_unit == "no"]$value, c(1, 1))

  by_species <- cwp_explore_timeseries(explore_data(), by = "species")
  expect_true("species" %in% names(by_species))
  expect_equal(sum(by_species$value), sum(totals$value))

  expect_s3_class(cwp_explore_timeseries_plot(totals), "ggplot")
  expect_error(ggplot2::ggplot_build(cwp_explore_timeseries_plot(by_species, "species")), NA)
  expect_s3_class(cwp_explore_timeseries_plot(totals[0]), "ggplot")
})

test_that("the main values of a breakdown are kept, the others grouped", {
  data <- data.table::data.table(species = letters[1:12], measurement_value = 12:1)
  reduced <- cwp_explore_top(data, "species", n = 3)
  expect_equal(sort(unique(reduced$species)), c("a", "b", "c", "Other"))
  expect_equal(sum(reduced$measurement_value), sum(data$measurement_value))
  expect_equal(data$species, letters[1:12])
})

test_that("the summary table has one column per dataset", {
  summary <- cwp_explore_summary(explore_data(), "species")
  expect_equal(names(summary), c("species", "measurement_unit", "init", "final"))
  yft <- summary[species == "YFT" & measurement_unit == "t"]
  expect_equal(c(yft$init, yft$final), c(45, 75))
  # Sorted by decreasing value in the first dataset
  expect_equal(summary$init, sort(summary$init, decreasing = TRUE))

  empty <- cwp_explore_summary(cwp_explore_filter(explore_data(), list(species = "none")), "species")
  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), c("species", "measurement_unit", "init", "final"))
})

test_that("the explorer is a Shiny application", {
  skip_if_not_installed("shiny")
  app <- cwp_explore(init = explore_table(), final = explore_table())
  expect_s3_class(app, "shiny.appobj")
})

test_that("the explorer recomputes the outputs when a filter changes", {
  skip_if_not_installed("shiny")
  data <- explore_data()
  filters <- c("source_authority", "species", "fishing_fleet")
  shiny::testServer(cwp_explore_server(data, filters), {
    session$setInputs(by = "species", years = c(2000, 2002))
    session$elapse(1000)
    expect_equal(nrow(selection()), 12)
    expect_match(output$selection, "12 of 12 rows")

    session$setInputs(filter_species = "YFT", filter_source_authority = "IOTC")
    session$elapse(1000)
    expect_equal(nrow(selection()), 6)
    expect_equal(unique(summary_table()$species), "YFT")
    expect_match(output$selection, "6 of 12 rows")
    expect_false(is.null(output$timeseries))
  })
})

test_that("the cells of the grid become tiles", {
  tiles <- cwp_explore_tiles(c("5100000", "6100000", "5100000", NA, "not_a_cell"))
  expect_equal(sort(tiles$code), c("5100000", "6100000"))
  one <- tiles[code == "5100000"]
  expect_equal(c(one$x, one$y, one$width, one$height), c(0.5, 0.5, 1, 1))
  expect_null(cwp_explore_tiles("not_a_cell"))
})

test_that("totals by cell are mapped one unit at a time", {
  data <- explore_data()
  tiles <- cwp_explore_tiles(data$geographic_identifier)
  cells <- cwp_explore_map_data(data, "t", tiles)
  expect_equal(sort(unique(cells$gridtype)), c("1deg_x_1deg", "5deg_x_5deg"))
  expect_equal(cells[Dataset == "init" & Cell == "6100000"]$value, 70)
  expect_equal(cells[Dataset == "init" & Cell == "5100000"]$value, 15)
  expect_equal(sum(cells$value), sum(data[measurement_unit == "t"]$measurement_value))

  expect_null(cwp_explore_map_data(data, "kg", tiles))
  expect_null(cwp_explore_map_data(data, "t", NULL))

  map <- cwp_explore_map(cells, "t")
  expect_s3_class(map, "leaflet")
  methods <- vapply(map$x$calls, function(call) call$method, character(1))
  expect_true(all(c("addRectangles", "addLegend", "addLayersControl") %in% methods))
})

test_that("the colour classes of totals cover every value", {
  values <- c(0.5, 3, 12, 150, 2000, 2000, 45000)
  palette <- cwp_leaflet_palette("magnitude", values)
  expect_false(any(is.na(palette(values)) | palette(values) == "#CCCCCC"))
  expect_false(any(palette(c(5, 5)) == "#CCCCCC"))
  expect_s3_class(cwp_tile_fill_scale("magnitude"), "ScaleContinuous")
})

test_that("the explorer maps the selection", {
  skip_if_not_installed("shiny")
  data <- explore_data()
  shiny::testServer(cwp_explore_server(data, c("species")), {
    session$setInputs(by = "", unit = "t")
    session$elapse(1000)
    expect_equal(sum(map_cells()$value), sum(data[measurement_unit == "t"]$measurement_value))

    session$setInputs(filter_species = "BET")
    session$elapse(1000)
    expect_equal(unique(map_cells()$Cell), "6100000")
    expect_false(is.null(output$map))
  })
})

test_that("differences are classified like in the reports", {
  impact <- cwp_impact_category(reference = c(0, 10, 10, 10, 10, 10, 0),
                                compared  = c(5, 25, 15, 10, 4, 0, 0))
  expect_equal(as.character(impact),
               c("Appearing data", "Gain (more than double)", "Gain", "No differences",
                 "Loss", "All data lost", "No differences"))
  expect_equal(levels(impact), cwp_impact_levels)
  expect_equal(cwp_difference_percent(c(0, 0, 10), c(0, 3, 5)), c(0, Inf, -50))
})

test_that("the differences between two datasets are computed by dimension", {
  data <- explore_data()
  differences <- cwp_explore_differences(data, "species", "init", "final")
  expect_equal(names(differences), c("species", "measurement_unit", "init", "final",
                                     "Difference", "Difference (%)", "Impact"))
  skj <- differences[species == "SKJ"]
  expect_equal(c(skj$init, skj$final, skj$Difference, skj$`Difference (%)`), c(20, 0, -20, -100))
  expect_equal(as.character(skj$Impact), "All data lost")
  yft <- differences[species == "YFT" & measurement_unit == "t"]
  expect_equal(yft$Difference, 30)
  # Largest differences first
  expect_equal(abs(differences$Difference), sort(abs(differences$Difference), decreasing = TRUE))

  # The other way round
  reversed <- cwp_explore_differences(data, "species", "final", "init")
  expect_equal(as.character(reversed[species == "SKJ"]$Impact), "Appearing data")

  expect_error(cwp_explore_differences(data, "species", "init", "init"), "two different")
  empty <- cwp_explore_differences(cwp_explore_filter(data, list(species = "none")), "species",
                                   "init", "final")
  expect_equal(nrow(empty), 0)
})

test_that("the explorer compares two datasets", {
  skip_if_not_installed("shiny")
  data <- explore_data()
  shiny::testServer(cwp_explore_server(data, c("species")), {
    session$setInputs(by = "species", unit = "t", reference = "init", compared = "final")
    session$elapse(1000)
    expect_equal(differences_table()[species == "SKJ"]$Difference, -20)

    cells <- difference_cells()
    # Cell 5100001 only has SKJ, lost in the final dataset
    expect_equal(as.character(cells[geographic_identifier == "5100001"]$Impact), "All data lost")
    expect_equal(as.character(cells[geographic_identifier == "6100000"]$Impact), "Gain")
    expect_false(is.null(output$difference_map))

    session$setInputs(compared = "init")
    expect_error(pair())
  })
})

test_that("the explorer of a single dataset has no Differences tab", {
  skip_if_not_installed("shiny")
  single <- cwp_explore_datasets(list(explore_table()))
  expect_false(grepl("Differences", as.character(cwp_explore_ui(single, "species"))))
  expect_true(grepl("Differences", as.character(cwp_explore_ui(explore_data(), "species"))))
})

test_that("the explorer can open with values selected and compares first and last", {
  skip_if_not_installed("shiny")
  three <- cwp_explore_datasets(list(a = explore_table(), b = explore_table(), c = explore_table()))
  ui <- as.character(cwp_explore_ui(three, c("species", "gear_type"),
                                    selected = list(species = c("YFT", "not_a_code"))))
  expect_match(ui, '<option value="YFT" selected>', fixed = TRUE)
  expect_false(grepl("not_a_code", ui, fixed = TRUE))
  # The last dataset is compared with the first one
  expect_match(ui, '<option value="c" selected>c</option>', fixed = TRUE)

  # A column given in `selected` becomes a filter
  app <- cwp_explore(explore_table(), filters = "species", selected = list(gear_type = "01.1"))
  expect_s3_class(app, "shiny.appobj")
})
