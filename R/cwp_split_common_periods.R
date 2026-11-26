#' Split two datasets into their common years and the years of one only
#'
#' Two versions of a dataset (e.g. two releases) are only comparable on the
#' years they both cover: a new release usually adds a year, and a source may
#' cover different years in each version. This function keeps, for each group
#' (by default each `source_authority`), the years present in both datasets,
#' and lists apart the years present in one dataset only, with their totals.
#' The common part can then be compared with [fonction_groupement()] or
#' [comprehensive_cwp_dataframe_analysis()].
#'
#' @param init,final Data frames in CWP format, with `time_start` (or `year`),
#'   `measurement_value` and `measurement_unit`.
#' @param by Columns defining the groups whose years are compared.
#' @param titre_1,titre_2 Names of the two datasets in the tables.
#' @return A list:
#'   * `init`, `final`: the rows of the years present in both datasets, for
#'     their group;
#'   * `periods`: one row per group, with the years of each dataset, the
#'     common years and the years of one dataset only, as compact ranges
#'     (e.g. "1950-1980, 1983");
#'   * `uncommon`: totals by group, year and unit of the years present in one
#'     dataset only (`dataset` = `titre_1` or `titre_2`).
#' @examples
#' a <- data.frame(
#'   source_authority = "IOTC", time_start = c("2019-01-01", "2020-01-01"),
#'   measurement_unit = "t", measurement_value = c(1, 2)
#' )
#' b <- rbind(a, data.frame(
#'   source_authority = "IOTC", time_start = "2021-01-01",
#'   measurement_unit = "t", measurement_value = 3
#' ))
#' cwp_split_common_periods(a, b)$periods
#' @export
cwp_split_common_periods <- function(init, final, by = "source_authority",
                                     titre_1 = "Dataset 1", titre_2 = "Dataset 2") {
  with_year <- function(x) {
    x <- data.table::as.data.table(x)
    x[, .year := as.integer(if ("time_start" %in% names(x)) substr(as.character(time_start), 1, 4) else year)]
    x
  }
  init <- with_year(init)
  final <- with_year(final)
  keys <- c(by, ".year")
  y1 <- unique(init[, keys, with = FALSE])
  y2 <- unique(final[, keys, with = FALSE])
  both <- merge(y1, y2, by = keys)

  groups <- unique(rbind(y1[, by, with = FALSE], y2[, by, with = FALSE]))
  data.table::setorderv(groups, by)
  years_of <- function(y, g) y[g, on = by, nomatch = NULL]$.year
  periods <- groups[,
    {
      g <- data.table::as.data.table(.BY)
      a <- years_of(y1, g)
      b <- years_of(y2, g)
      list(
        v1 = cwp_year_ranges(a), v2 = cwp_year_ranges(b),
        common = cwp_year_ranges(intersect(a, b)),
        o1 = cwp_year_ranges(setdiff(a, b)), o2 = cwp_year_ranges(setdiff(b, a))
      )
    },
    by = by
  ]
  data.table::setnames(
    periods, c("v1", "v2", "common", "o1", "o2"),
    c(
      paste("Years", titre_1), paste("Years", titre_2), "Common years",
      paste("Only in", titre_1), paste("Only in", titre_2)
    )
  )

  total <- function(x, name) {
    x <- x[!both, on = keys]
    x[, list(measurement_value = sum(as.numeric(measurement_value), na.rm = TRUE), number_lines = .N),
      by = c(by, ".year", "measurement_unit")
    ][, dataset := name]
  }
  uncommon <- rbind(total(init, titre_1), total(final, titre_2))
  data.table::setnames(uncommon, ".year", "year")
  uncommon[, .order := match(dataset, c(titre_1, titre_2))] # first dataset first
  data.table::setorderv(uncommon, c(".order", by, "year"))
  uncommon[, .order := NULL]

  common <- function(x) x[both, on = keys, nomatch = NULL][, .year := NULL][]
  list(init = common(init), final = common(final), periods = periods[], uncommon = uncommon[])
}

#' Years as compact ranges: c(1950:1953, 1960) gives "1950-1953, 1960"
#' @noRd
cwp_year_ranges <- function(y) {
  y <- sort(unique(y))
  if (!length(y)) {
    return("")
  }
  groups <- cumsum(c(1, diff(y) != 1))
  paste(vapply(split(y, groups), function(g) {
    if (length(g) == 1) as.character(g) else paste0(g[1], "-", g[length(g)])
  }, character(1)), collapse = ", ")
}
