#' Aggregate a CWP dataset to a coarser time and space resolution
#'
#' Sums a dataset in CWP format by year (or month) and by 5 degree square (the
#' 1 degree squares are grouped into their 5 degree square), keeping all its
#' other dimensions. The result is much smaller than the dataset and keeps
#' its totals: it can be stored, and compared with the aggregate of another
#' version of the dataset (see [cwp_split_common_periods()] and
#' [comprehensive_cwp_dataframe_analysis()]).
#'
#' @param data A data frame in CWP format: `time_start` (or `year`),
#'   `measurement_value` and dimension columns.
#' @param time_resolution `"year"` or `"month"`: period of `time_start` in the
#'   result.
#' @param spatial_resolution `5` to group the 1 degree squares into their 5
#'   degree square, `NULL` to keep the geographic identifiers as they are.
#'   Codes that are not 1 degree squares are always kept.
#' @param dimensions Dimension columns to keep (those absent from `data` are
#'   ignored).
#' @return A data.table with `time_start` (first day of the period, as a
#'   string), the dimensions, `measurement_value` (sum) and `n_rows` (number
#'   of rows of `data` summed).
#' @examples
#' d <- data.frame(
#'   source_authority = "IOTC", species = "YFT", gear_type = "01.1",
#'   time_start = c("2020-01-01", "2020-02-01"),
#'   geographic_identifier = c("5100050", "5101051"),
#'   measurement_unit = "t", measurement_value = c(1, 2)
#' )
#' cwp_aggregate(d)
#' @export
cwp_aggregate <- function(data, time_resolution = c("year", "month"), spatial_resolution = 5,
                          dimensions = c(
                            "source_authority", "species", "gear_type", "fishing_fleet",
                            "fishing_mode", "geographic_identifier", "measurement_type",
                            "measurement_unit"
                          )) {
  time_resolution <- match.arg(time_resolution)
  d <- data.table::as.data.table(data)
  start <- if ("time_start" %in% names(d)) as.character(d$time_start) else paste0(d$year, "-01-01")
  period <- if (time_resolution == "year") paste0(substr(start, 1, 4), "-01-01") else paste0(substr(start, 1, 7), "-01")
  dims <- intersect(dimensions, names(d))
  out <- d[, dims, with = FALSE]
  out[, time_start := period]
  out[, measurement_value := as.numeric(d$measurement_value)]
  if (!is.null(spatial_resolution) && "geographic_identifier" %in% dims) {
    if (!identical(as.numeric(spatial_resolution), 5)) stop("spatial_resolution must be 5 or NULL")
    out[, geographic_identifier := cwp_grid_to_5deg(geographic_identifier)]
  }
  out[, list(measurement_value = sum(measurement_value, na.rm = TRUE), n_rows = .N),
    by = c("time_start", dims)
  ]
}

#' 5 degree square of CWP 1 degree square codes
#'
#' A CWP 1 degree square code (7 digits starting with 5: size, quadrant,
#' latitude on 2 digits, longitude on 3 digits) is replaced by the code of the
#' 5 degree square that contains it. Other codes are returned unchanged.
#'
#' @param code Character vector of CWP grid codes.
#' @return Character vector of the same length.
#' @examples
#' cwp_grid_to_5deg(c("5212124", "6230150", "IOTC_WEST"))
#' @export
cwp_grid_to_5deg <- function(code) {
  code <- as.character(code)
  one <- !is.na(code) & nchar(code) == 7 & substr(code, 1, 1) == "5"
  if (any(one)) {
    lat <- as.integer(substr(code[one], 3, 4))
    lon <- as.integer(substr(code[one], 5, 7))
    code[one] <- paste0(
      "6", substr(code[one], 2, 2),
      sprintf("%02d", lat - lat %% 5), sprintf("%03d", lon - lon %% 5)
    )
  }
  code
}
