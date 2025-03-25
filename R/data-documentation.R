#' Example subset of catch dataset (initial)
#'
#' A small sample of initial catch data, structured according to CWP standards.
#'
#' @format A tibble with 200 rows and 14 variables:
#' \describe{
#'   \item{source_authority}{Source of the data (e.g. RFMO)}
#'   \item{gear_type}{CWP gear type code}
#'   \item{fishing_fleet}{Fishing fleet country or code}
#'   \item{fishing_mode}{Fishing mode (e.g., UNK for unknown)}
#'   \item{time_start}{Start date of the operation}
#'   \item{time_end}{End date of the operation}
#'   \item{geographic_identifier}{CWP geographic identifier}
#'   \item{measurement_unit}{Unit of measurement (e.g., t, no, etc.)}
#'   \item{measurement_value}{Measurement value (e.g., catch in tons)}
#'   \item{GRIDTYPE}{Spatial resolution label}
#'   \item{Gear}{Gear label}
#'   \item{fishing_mode_label}{Fishing mode label}
#'   \item{fishing_fleet_label}{Fishing fleet label}
#'   \item{species}{Species code (e.g., YFT, SKJ, etc.)}
#' }
"parameter_init_catch"

#' Example subset of catch dataset (final)
#'
#' A small sample of final catch data, structured according to CWP standards.
#'
#' @format Same structure as \code{parameter_init_catch}.
"parameter_final_catch"

#' Example subset of effort dataset (initial)
#'
#' A small sample of initial effort data, structured according to CWP standards.
#'
#' @format A tibble with 200 rows and 13 variables:
#' \describe{
#'   \item{source_authority}{Source of the data (e.g. RFMO)}
#'   \item{gear_type}{CWP gear type code}
#'   \item{fishing_fleet}{Fishing fleet country or code}
#'   \item{fishing_mode}{Fishing mode (e.g., UNK for unknown)}
#'   \item{time_start}{Start date of the operation}
#'   \item{time_end}{End date of the operation}
#'   \item{geographic_identifier}{CWP geographic identifier}
#'   \item{measurement_unit}{Unit of measurement (e.g., HRSRH)}
#'   \item{measurement_value}{Measurement value (e.g., hours of effort)}
#'   \item{GRIDTYPE}{Spatial resolution label}
#'   \item{Gear}{Gear label}
#'   \item{fishing_mode_label}{Fishing mode label}
#'   \item{fishing_fleet_label}{Fishing fleet label}
#' }
"parameter_init_effort"

#' Example subset of effort dataset (final)
#'
#' A small sample of final effort data, structured according to CWP standards.
#'
#' @format Same structure as \code{parameter_init_effort}.
"parameter_final_effort"
