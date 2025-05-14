#' Enrich a CWP dataset with missing labels and geometries
#'
#' This function enriches a dataset (data.frame or sf) by:
#' - filling in missing descriptive labels (species, gear type, fleet, units, etc.),
#' - attaching or validating geometries,
#' using SQL queries, local fallback files, or online downloads.
#'
#' @param data         A \code{data.frame} or \code{sf} object following CWP standards.
#' @param connectionDB Optional. A DBI-compatible database connection.
#' @param save_prefix  Optional. A prefix for saving output files (\code{.qs} and \code{.csv}).
#'
#' @return A \code{list} with two elements:
#'   \describe{
#'     \item{\code{with_geom}}{An \code{sf} object enriched with geometry.}
#'     \item{\code{without_geom}}{A \code{data.frame} enriched without geometry.}
#'   }
#'
#' @details
#' Steps performed:
#' \itemize{
#'   \item Load required codelists (species, measurements, gear, fleet, etc.) via SQL or fallback.
#'   \item Normalize units (e.g. “Tons” → “t”, “Number of fish” → “no”).
#'   \item Left-join to add each \code{*_label} column alongside its code.
#'   \item Reorder columns: base columns with their labels immediately after.
#'   \item Convert to \code{sf} (CRS EPSG:4326).
#'   \item Optionally save to disk if \code{save_prefix} is provided.
#' }
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   species = c("COD", "SAL"),
#'   measurement_unit = c("Tons", "Number of fish"),
#'   geographic_identifier = c("0001", "0002"),
#'   fishing_mode = "LL",
#'   fishing_fleet = "FleetA",
#'   measurement_type = "KT",
#'   measurement = "catch",
#'   measurement_processing_level = "raised",
#'   gear_type = "01.1"
#' )
#' result <- enrich_dataset_if_needed(df)
#' str(result)
#' }
#'
#' @seealso \code{\link[sf]{st_as_sf}}, \code{\link[DBI]{dbGetQuery}}
#'
#' @import dplyr
#' @import sf
#' @import qs
#' @import data.table
#'
#' @importFrom DBI dbGetQuery dbIsValid
#' @importFrom utils download.file unzip
#' @importFrom readr read_csv
#' @importFrom stringr str_detect
#' @importFrom janitor clean_names
#' @importFrom here here
#' @importFrom dplyr mutate case_when rename left_join select everything
#' @importFrom sf st_read st_as_sf st_crs
#' @importFrom data.table fwrite
#'
#' @export
enrich_dataset_if_needed <- function(data, connectionDB = NULL, save_prefix = NULL) {
  library(dplyr); library(sf); library(readr); library(stringr)
  library(janitor); library(here); library(qs); library(data.table); library(DBI)

  # Fallback‐query helpers
  try_db_query <- function(con, query, fallback_file, fallback_read_fun, download_url = NULL) {
    if (!is.null(con) && dbIsValid(con)) {
      ok <- try(dbGetQuery(con, "SELECT 1"), silent = TRUE)
      if (!inherits(ok, "try-error")) {
        message("DB ok, running: ", query)
        return(dbGetQuery(con, query))
      }
    }
    message("No valid DB—falling back to: ", fallback_file)
    if (!file.exists(fallback_file)) {
      if (!is.null(download_url)) {
        message("Downloading from ", download_url)
        utils::download.file(download_url, fallback_file, mode = "wb")
      } else {
        stop("Missing fallback file: ", fallback_file)
      }
    }
    fallback_read_fun(fallback_file)
  }

  try_st_read <- function(con, query, fallback_file, download_url = NULL) {
    if (!is.null(con) && dbIsValid(con)) {
      ok <- try(dbGetQuery(con, "SELECT 1"), silent = TRUE)
      if (!inherits(ok, "try-error")) {
        message("DB ok, st_read: ", query)
        return(sf::st_read(con, query = query))
      }
    }
    if (!file.exists(fallback_file)) {
      if (!is.null(download_url)) {
        message("Downloading and unzipping ", download_url)
        utils::download.file(download_url, fallback_file, mode = "wb")
        utils::unzip(fallback_file, exdir = dirname(fallback_file))
      } else {
        stop("Missing fallback shapefile: ", fallback_file)
      }
    }
    sf::st_read(fallback_file)
  }

  # standardize units
  data <- data %>%
    mutate(measurement_unit = case_when(
      measurement_unit == "Tons"             ~ "t",
      measurement_unit == "Number of fish"   ~ "no",
      TRUE                                   ~ measurement_unit
    ))

  # if WKT col present
  if ("geom_wkt" %in% colnames(data)) {
    data <- rename(data, geom = geom_wkt)
  }

  con <- tryCatch(connectionDB, error = function(e) NULL)

  # Load all codelists via system.file()
  pkg <- "yourPackageName"
  species_group <- try_db_query(
    con,
    "SELECT taxa_order, code FROM species.species_asfis",
    system.file("extdata", "cl_asfis_species.csv", package = pkg),
    function(f) read_csv(f) %>% clean_names() %>% select(species_group = taxa_order, species = code),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv"
  )

  cl_measurement_processing_level <- try_db_query(
    con,
    "SELECT * FROM measurement_processing_level.measurement_processing_level",
    system.file("extdata", "cl_measurement_processing_level.csv", package = pkg),
    function(f) read_csv(f) %>% clean_names() %>% select(code, label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement_processing_level.csv"
  )

  cl_measurement <- try_db_query(
    con,
    "SELECT * FROM measurement.measurement",
    system.file("extdata", "cl_measurement.csv", package = pkg),
    function(f) read_csv(f) %>% clean_names() %>% select(code, label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement.csv"
  )

  cl_fishing_mode <- try_db_query(
    con,
    "SELECT * FROM fishing_mode.fishing_mode",
    system.file("extdata", "cl_fishing_mode.csv", package = pkg),
    function(f) read_csv(f) %>% clean_names() %>% select(code, label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_mode.csv"
  )

  cl_cwp_gear_level2 <- try_db_query(
    con,
    "SELECT * FROM gear_type.isscfg_revision_1",
    system.file("extdata", "cl_isscfg_pilot_gear.csv", package = pkg),
    function(f) read_csv(f) %>% select(Code = code, Gear = label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_isscfg_pilot_gear.csv"
  )

  fishing_fleet_label <- try_db_query(
    con,
    "SELECT * FROM fishing_fleet.fishingfleet_firms",
    system.file("extdata", "cl_fishingfleet_firms.csv", package = pkg),
    function(f) read_csv(f) %>% clean_names() %>% select(code, label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_fleet.csv"
  )

  # measurement_type: still bind two CSVs shipped in inst/extdata
  catch_file  <- system.file("extdata", "cl_catch_concepts.csv", package = pkg)
  effort_file <- system.file("extdata", "cl_measurement_types_effort.csv", package = pkg)
  measurement_type_df <- bind_rows(
    read_csv(catch_file)  %>% clean_names() %>% select(code, label),
    read_csv(effort_file) %>% clean_names() %>% select(code, label)
  )

  # measurement-unit labels (global & specific) — no change

  # CWP grid (WKT) from extdata
  cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = pkg)
  if (!file.exists(cwp_grid_file)) {
    stop("cl_areal_grid.csv not found in inst/extdata — run data-raw/download_codelists.R")
  }
  shapefile.fix <- st_read(cwp_grid_file) %>%
    st_as_sf(wkt = "geom_wkt", crs = 4326) %>%
    rename(cwp_code = CWP_CODE, geom = geom_wkt)

  # Start enrichment
  enriched_data <- data %>%
    # species
    left_join(species_group, by = "species") %>%
    # gear
    left_join(cl_cwp_gear_level2, by = c("gear_type" = "Code")) %>%
    rename(gear_type_label = Gear) %>%
    # fleet
    left_join(fishing_fleet_label %>% select(code, fishing_fleet_label = label),
              by = c("fishing_fleet" = "code")) %>%
    # measurement unit
    {
      global  <- measurement_unit_label %>% filter(source_authority == "ALL")
      specific <- measurement_unit_label %>% filter(source_authority != "ALL")
      . %>%
        left_join(specific %>% select(code, source_authority, meas_unit_lbl_sp = label),
                  by = c("measurement_unit" = "code", "source_authority")) %>%
        left_join(global  %>% select(code, meas_unit_lbl_gl = label),
                  by = "measurement_unit") %>%
        mutate(measurement_unit_label = coalesce(meas_unit_lbl_sp, meas_unit_lbl_gl)) %>%
        select(-meas_unit_lbl_sp, -meas_unit_lbl_gl)
    } %>%
    # measurement type, measurement, processing level, fishing mode
    left_join(measurement_type_df %>% rename(measurement_type_label = label),
              by = c("measurement_type" = "code")) %>%
    left_join(cl_measurement %>% rename(measurement_label = label),
              by = c("measurement" = "code")) %>%
    left_join(cl_measurement_processing_level %>% rename(measurement_processing_level_label = label),
              by = c("measurement_processing_level" = "code")) %>%
    left_join(cl_fishing_mode %>% rename(fishing_mode_label = label),
              by = c("fishing_mode" = "code")) %>%
    # add gridtype
    left_join(shapefile.fix %>% select(geographic_identifier = cwp_code, gridtype = GRIDTYPE),
              by = "geographic_identifier") %>%
    # reorder so that each code is followed by its *_label
    { df ->
        cols <- names(df)
        base <- grep("_label$", cols, invert = TRUE, value = TRUE)
        neword <- unlist(lapply(base, function(x) c(x, paste0(x, "_label"))))
        neword <- neword[neword %in% cols]
        df[, unique(c(neword, setdiff(cols, neword)))]
    }() %>%
    st_as_sf(crs = 4326)

  # save if requested
  if (!is.null(save_prefix)) {
    qsave(enriched_data, paste0(save_prefix, "_with_geom.qs"))
    df_no_geom <- as.data.frame(enriched_data)
    df_no_geom$geom <- NULL
    fwrite(df_no_geom, paste0(save_prefix, "_without_geom.csv"))
  }

  without_geom <- as.data.frame(enriched_data)
  without_geom$geom <- NULL

  list(with_geom = enriched_data, without_geom = without_geom)
}

