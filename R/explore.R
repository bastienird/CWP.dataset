utils::globalVariables(c(".dataset", ".year", ".colour", ".line", "measurement_value",
                         "measurement_unit", "time_start", "value", "CWP_CODE",
                         "geographic_identifier", "gridtype", "width", "height"))

# Columns offered as filters when they exist, in this order
# (source_authority is the tRFMO in the Global Tuna Atlas datasets)
cwp_explore_default_filters <- c("source_authority", "species", "fishing_fleet",
                                 "gear_type", "fishing_mode")

# Columns that are never offered as a breakdown
cwp_explore_not_dimensions <- c("geographic_identifier", "time_start", "time_end",
                                "geom", "geometry", "geom_wkt")

#' Explore CWP datasets interactively
#'
#' Opens a Shiny application to filter one or several CWP datasets on the fly
#' (for example on a species, a fishing fleet and/or a tRFMO) and see the
#' result as time series, tables and maps. The totals are recomputed for every
#' selection, so they are always exact.
#'
#' The reports stay the reference document of each processing step; this
#' application is for looking at the data in more detail.
#'
#' @param ... The datasets to explore: data.frames, paths of step directories
#'   (as in `Markdown/<step>`) or paths of data files (`.parquet`, `.rds`,
#'   `.csv`). Name them to choose the names shown in the application, for
#'   example `cwp_explore(init = "Markdown/rawdata", final = "Markdown/step5")`.
#' @param filters Columns offered as filters. By default `source_authority`,
#'   `species`, `fishing_fleet`, `gear_type` and `fishing_mode`, when present.
#' @return A Shiny application. Printing it (at the console) launches it; it
#'   can also be given to [shiny::runApp()].
#' @export
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   cwp_explore(init = parameter_init_catch, final = parameter_final_catch)
#' }
cwp_explore <- function(..., filters = NULL) {
  cwp_require_package("shiny", "to explore datasets interactively")
  data <- cwp_explore_datasets(list(...))
  if (is.null(filters)) filters <- intersect(cwp_explore_default_filters, names(data))
  filters <- intersect(filters, names(data))

  shiny::shinyApp(
    ui = cwp_explore_ui(data, filters),
    server = cwp_explore_server(data, filters)
  )
}

#' Read and combine the datasets to explore
#'
#' @param datasets A list of data.frames, step directories or data files.
#' @return A single `data.table`, with the name of each dataset in `.dataset`
#'   (a factor, in the order given) and the year in `.year`.
#' @keywords internal
#' @noRd
cwp_explore_datasets <- function(datasets) {
  if (length(datasets) == 0) stop("Give at least one dataset to explore.")
  given <- names(datasets)
  if (is.null(given)) given <- rep("", length(datasets))
  default <- vapply(seq_along(datasets), function(i) {
    x <- datasets[[i]]
    if (is.character(x) && length(x) == 1) basename(sub("/+$", "", x)) else paste("Dataset", i)
  }, character(1))
  dataset_names <- ifelse(nzchar(given), given, default)
  dataset_names <- make.unique(dataset_names, sep = " ")

  tables <- lapply(seq_along(datasets), function(i) {
    table <- cwp_explore_prepare(datasets[[i]])
    table[, .dataset := dataset_names[i]]
    table
  })
  data <- data.table::rbindlist(tables, use.names = TRUE, fill = TRUE)
  data[, .dataset := factor(.dataset, levels = dataset_names)]
  data
}

#' Prepare one dataset to explore
#'
#' @param x A data.frame, a step directory or a data file.
#' @return A `data.table` with a numeric `measurement_value`, a
#'   `measurement_unit` (`""` when the dataset has none) and the year of
#'   `time_start` in `.year`. Codes are stored as character.
#' @keywords internal
#' @noRd
cwp_explore_prepare <- function(x) {
  if (is.character(x) && length(x) == 1) {
    x <- if (dir.exists(x)) cwp_read_step_data(x) else read_data(x)
  }
  if (!is.data.frame(x)) stop("A dataset to explore must be a data.frame or the path of one.")
  if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
  if (!"measurement_value" %in% names(x)) stop("A dataset to explore needs a measurement_value column.")

  # A data.table given by the user is copied: columns are added by reference
  data <- if (data.table::is.data.table(x)) data.table::copy(x) else data.table::as.data.table(x)
  data[, measurement_value := as.numeric(measurement_value)]
  if (!"measurement_unit" %in% names(data)) data[, measurement_unit := ""]
  data[, .year := if ("time_start" %in% names(data)) {
    suppressWarnings(as.integer(substr(as.character(time_start), 1, 4)))
  } else {
    NA_integer_
  }]
  # Factors, dates and logicals become character, so that they compare with
  # the values chosen in the filters
  for (column in setdiff(names(data), c("measurement_value", ".year"))) {
    values <- data[[column]]
    if (is.factor(values) || !(is.character(values) || is.numeric(values) || is.list(values))) {
      data.table::set(data, j = column, value = as.character(values))
    }
  }
  data
}

#' Columns a dataset can be broken down by
#'
#' @param data Output of `cwp_explore_datasets()`.
#' @return Names of the character columns, labels and technical columns excluded.
#' @keywords internal
#' @noRd
cwp_explore_dimensions <- function(data) {
  columns <- names(data)[vapply(data, is.character, logical(1))]
  columns <- columns[!startsWith(columns, ".") & !endsWith(columns, "_label")]
  setdiff(columns, c(cwp_explore_not_dimensions, "measurement_unit"))
}

#' Values a filter can take
#'
#' @param data Output of `cwp_explore_datasets()`.
#' @param column Column of the filter.
#' @return The sorted codes of `column`. When a `<column>_label` column
#'   exists, the codes are named "code - label", which is what the filter shows.
#' @keywords internal
#' @noRd
cwp_explore_choices <- function(data, column) {
  label_col <- paste0(column, "_label")
  if (label_col %in% names(data)) {
    pairs <- unique(data, by = c(column, label_col))
    pairs <- data.frame(code = as.character(pairs[[column]]),
                        label = as.character(pairs[[label_col]]),
                        stringsAsFactors = FALSE)
    pairs <- pairs[!is.na(pairs$code) & !duplicated(pairs$code), , drop = FALSE]
    pairs <- pairs[order(pairs$code), , drop = FALSE]
    shown <- ifelse(is.na(pairs$label) | pairs$label == "", pairs$code,
                    paste(pairs$code, "-", pairs$label))
    return(stats::setNames(pairs$code, shown))
  }
  values <- unique(as.character(data[[column]]))
  sort(values[!is.na(values)])
}

#' Keep the rows matching the filters
#'
#' @param data Output of `cwp_explore_datasets()`.
#' @param filters Named list: for each column, the values to keep. An empty
#'   selection keeps everything.
#' @param years Optional range of years to keep.
#' @return The matching rows.
#' @keywords internal
#' @noRd
cwp_explore_filter <- function(data, filters = list(), years = NULL) {
  keep <- rep(TRUE, nrow(data))
  for (column in names(filters)) {
    values <- filters[[column]]
    if (length(values) == 0 || !column %in% names(data)) next
    keep <- keep & as.character(data[[column]]) %in% values
  }
  if (length(years) == 2 && any(!is.na(data$.year))) {
    keep <- keep & !is.na(data$.year) & data$.year >= years[1] & data$.year <= years[2]
  }
  data[keep]
}

#' Keep the main values of a column, the others grouped
#'
#' @param data A `data.table`.
#' @param column Column to reduce.
#' @param n Number of values kept, the largest in total value.
#' @return `data`, with the other values of `column` replaced by "Other".
#' @keywords internal
#' @noRd
cwp_explore_top <- function(data, column, n = 10) {
  totals <- data[, .(value = sum(measurement_value, na.rm = TRUE)), by = column]
  if (nrow(totals) <= n) return(data)
  main <- totals[order(-value)][[column]][seq_len(n)]
  data <- data.table::copy(data)
  data.table::set(data, j = column,
                  value = ifelse(data[[column]] %in% main, data[[column]], "Other"))
  data
}

#' Totals by year
#'
#' @param data Rows to sum (output of `cwp_explore_filter()`).
#' @param by Optional column to break the totals down by; its 10 main values
#'   are kept, the others grouped as "Other".
#' @return A `data.table` with `.dataset`, `.year`, `measurement_unit`, `by`
#'   and the total `value`.
#' @keywords internal
#' @noRd
cwp_explore_timeseries <- function(data, by = NULL) {
  by <- intersect(by, names(data))
  data <- data[!is.na(.year)]
  if (length(by) == 1) data <- cwp_explore_top(data, by)
  totals <- data[, .(value = sum(measurement_value, na.rm = TRUE)),
                 by = c(".dataset", ".year", "measurement_unit", by)]
  data.table::setorderv(totals, c(".dataset", by, ".year"))
  totals
}

#' Plot the totals by year
#'
#' @param totals Output of `cwp_explore_timeseries()`.
#' @param by Column the totals are broken down by, or `NULL`.
#' @return A ggplot: one panel per unit; one colour per dataset, or per value
#'   of `by` with one line type per dataset.
#' @keywords internal
#' @noRd
cwp_explore_timeseries_plot <- function(totals, by = NULL) {
  totals <- as.data.frame(totals)
  if (nrow(totals) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "No data for this selection") +
             ggplot2::theme_void())
  }
  by <- intersect(by, names(totals))
  several_datasets <- length(unique(totals$.dataset)) > 1
  totals$.colour <- if (length(by) == 1) totals[[by]] else totals$.dataset
  totals$.line <- paste(totals$.dataset, totals$.colour)

  mapping <- if (length(by) == 1 && several_datasets) {
    ggplot2::aes(x = .year, y = value, colour = .colour, linetype = .dataset, group = .line)
  } else {
    ggplot2::aes(x = .year, y = value, colour = .colour, group = .line)
  }
  ggplot2::ggplot(totals, mapping) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 0.8) +
    ggplot2::facet_wrap(~measurement_unit, scales = "free_y", ncol = 1) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(x = NULL, y = NULL,
                  colour = if (length(by) == 1) by else "Dataset", linetype = "Dataset") +
    ggplot2::theme_bw()
}

#' Totals by a dimension, one column per dataset
#'
#' @param data Rows to sum (output of `cwp_explore_filter()`).
#' @param by Columns to group by; the unit is always added.
#' @return A `data.table` with `by`, `measurement_unit` and one column of
#'   totals per dataset, sorted by decreasing total of the first dataset.
#' @keywords internal
#' @noRd
cwp_explore_summary <- function(data, by = NULL) {
  by <- unique(c(intersect(by, names(data)), "measurement_unit"))
  totals <- data[, .(value = sum(measurement_value, na.rm = TRUE)), by = c(by, ".dataset")]
  datasets <- levels(data$.dataset)
  if (is.null(datasets)) datasets <- unique(as.character(data$.dataset))
  totals[, .dataset := factor(.dataset, levels = datasets)]
  if (nrow(totals) == 0) {
    empty <- data[0, by, with = FALSE]
    for (dataset in datasets) data.table::set(empty, j = dataset, value = numeric())
    return(empty)
  }
  formula <- stats::as.formula(paste(paste0("`", by, "`", collapse = " + "), "~ .dataset"))
  wide <- data.table::dcast(totals, formula, value.var = "value", fill = 0, drop = c(TRUE, FALSE))
  present <- intersect(datasets, names(wide))
  if (length(present) > 0) data.table::setorderv(wide, present[1], order = -1L)
  wide
}

#' Tiles of the CWP cells, from the grid shipped with the package
#'
#' Only the WKT of the cells asked for is parsed, which keeps this fast.
#'
#' @param codes CWP codes of the cells.
#' @return Output of `cwp_grid_tiles()`: a `data.table` with `code`, `x`, `y`,
#'   `width` and `height`, or `NULL` if the cells cannot be drawn as tiles.
#' @keywords internal
#' @noRd
cwp_explore_tiles <- function(codes) {
  codes <- unique(as.character(codes[!is.na(codes)]))
  grid <- cwp_cached("grid_wkt", {
    grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
    if (!nzchar(grid_file)) stop("cl_areal_grid.csv not found in the package")
    data.table::fread(grid_file, select = c("CWP_CODE", "geom_wkt"), colClasses = "character")
  })
  cells <- grid[CWP_CODE %in% codes]
  if (nrow(cells) == 0) return(NULL)
  shape <- sf::st_as_sf(as.data.frame(cells), wkt = "geom_wkt", crs = 4326)
  cwp_grid_tiles(shape, codes)
}

#' Totals by cell, ready to map
#'
#' @param data Rows to sum (output of `cwp_explore_filter()`).
#' @param unit The unit to map: values in different units are not added up.
#' @param tiles Output of `cwp_explore_tiles()`.
#' @return A `data.table` with `Dataset`, `Cell`, the total `value`, the tile
#'   columns and `gridtype`; cells without a tile are left out. `NULL` when
#'   there is nothing to map.
#' @keywords internal
#' @noRd
cwp_explore_map_data <- function(data, unit, tiles) {
  if (is.null(tiles) || nrow(tiles) == 0 || !"geographic_identifier" %in% names(data)) return(NULL)
  rows <- data[data$measurement_unit %in% unit]
  if (nrow(rows) == 0) return(NULL)
  totals <- rows[, .(value = sum(measurement_value, na.rm = TRUE)),
                 by = .(Dataset = .dataset, Cell = as.character(geographic_identifier))]
  cells <- merge(totals, tiles, by.x = "Cell", by.y = "code")
  if (nrow(cells) == 0) return(NULL)
  cells[, gridtype := sprintf("%gdeg_x_%gdeg", width, height)]
  cells
}

#' Map of the totals by cell
#'
#' @param cells Output of `cwp_explore_map_data()`.
#' @param unit Unit of the values, used as legend title.
#' @return A leaflet widget: one layer per dataset and resolution.
#' @keywords internal
#' @noRd
cwp_explore_map <- function(cells, unit) {
  cwp_leaflet_map(as.data.frame(cells), fill = "value", facet_rows = "gridtype",
                  facet_cols = "Dataset", fill_scale = "magnitude",
                  fill_label = if (nzchar(unit)) unit else "value",
                  popup_cols = c("Dataset", "Cell", "value"), layout = "layers")
}

#' User interface of the explorer
#'
#' @param data Output of `cwp_explore_datasets()`.
#' @param filters Columns offered as filters.
#' @return A Shiny UI.
#' @keywords internal
#' @noRd
cwp_explore_ui <- function(data, filters) {
  filter_inputs <- lapply(filters, function(column) {
    shiny::selectizeInput(paste0("filter_", column), column,
                          choices = cwp_explore_choices(data, column), multiple = TRUE,
                          options = list(placeholder = "All"))
  })

  years <- suppressWarnings(range(data$.year, na.rm = TRUE))
  year_input <- if (all(is.finite(years))) {
    shiny::sliderInput("years", "Years", min = years[1], max = years[2],
                       value = years, step = 1, sep = "")
  }

  dimensions <- cwp_explore_dimensions(data)
  default_by <- intersect(c("species", filters, dimensions), dimensions)[1]
  by_input <- shiny::selectInput("by", "Break down by",
                                 choices = c("Nothing" = "", dimensions),
                                 selected = if (is.na(default_by)) "" else default_by)

  units <- sort(unique(data$measurement_unit))
  unit_input <- if (length(units) > 1) {
    shiny::selectInput("unit", "Unit of the maps", choices = units,
                       selected = if ("t" %in% units) "t" else units[1])
  }

  table_output <- if (requireNamespace("DT", quietly = TRUE)) {
    DT::DTOutput("summary")
  } else {
    shiny::tableOutput("summary")
  }

  tabs <- list(
    shiny::tabPanel("Time series", shiny::plotOutput("timeseries", height = "550px")),
    shiny::tabPanel("Table", table_output)
  )
  if ("geographic_identifier" %in% names(data)) {
    tabs <- c(tabs, list(shiny::tabPanel("Map", leaflet::leafletOutput("map", height = "600px"))))
  }

  shiny::fluidPage(
    shiny::titlePanel("CWP dataset explorer", windowTitle = "CWP explorer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        filter_inputs,
        year_input,
        by_input,
        unit_input,
        shiny::helpText(shiny::textOutput("selection", inline = TRUE))
      ),
      shiny::mainPanel(
        width = 9,
        do.call(shiny::tabsetPanel, c(list(id = "tab"), tabs))
      )
    )
  )
}

#' Server of the explorer
#'
#' @inheritParams cwp_explore_ui
#' @return A Shiny server function.
#' @keywords internal
#' @noRd
cwp_explore_server <- function(data, filters) {
  function(input, output, session) {
    selection <- shiny::reactive({
      chosen <- stats::setNames(lapply(filters, function(column) input[[paste0("filter_", column)]]),
                                filters)
      cwp_explore_filter(data, chosen, input$years)
    })
    # Wait for the user to stop typing before recomputing
    selection <- shiny::debounce(selection, 300)
    by <- shiny::reactive(if (is.null(input$by) || input$by == "") NULL else input$by)

    output$selection <- shiny::renderText({
      paste(format(nrow(selection()), big.mark = ","), "of",
            format(nrow(data), big.mark = ","), "rows selected")
    })

    output$timeseries <- shiny::renderPlot({
      cwp_explore_timeseries_plot(cwp_explore_timeseries(selection(), by()), by())
    })

    summary_table <- shiny::reactive(cwp_explore_summary(selection(), by()))
    if (requireNamespace("DT", quietly = TRUE)) {
      output$summary <- DT::renderDT({
        table <- summary_table()
        numeric_cols <- names(table)[vapply(table, is.numeric, logical(1))]
        DT::formatRound(DT::datatable(table, rownames = FALSE, filter = "top",
                                      options = list(pageLength = 25, scrollX = TRUE)),
                        numeric_cols, digits = 0)
      })
    } else {
      output$summary <- shiny::renderTable(summary_table(), digits = 0)
    }

    units <- sort(unique(data$measurement_unit))
    unit <- shiny::reactive({
      if (!is.null(input$unit)) input$unit else if ("t" %in% units) "t" else units[1]
    })

    if ("geographic_identifier" %in% names(data)) {
      # Computed once, the first time a map is shown
      tiles <- shiny::reactive(cwp_explore_tiles(data$geographic_identifier))
      map_cells <- shiny::reactive(cwp_explore_map_data(selection(), unit(), tiles()))
      output$map <- leaflet::renderLeaflet({
        cells <- map_cells()
        shiny::validate(shiny::need(!is.null(cells), "No CWP grid cell to map for this selection."))
        cwp_explore_map(cells, unit())
      })
    }
  }
}
