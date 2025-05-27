#' Enrich a CWP Dataset with Missing Labels and Geometries
#'
#' @title Enrich CWP Dataset
#' @description
#' Completes a CWP-compliant dataset (data.frame or sf) by:
#' \itemize{
#'   \item Filling in missing descriptive labels (species, gear type, fleet, units, etc.);
#'   \item Attaching or validating spatial geometries.
#' }
#' Labels and geometries are sourced via SQL queries when \code{connectionDB} is provided,
#' with fallbacks to local files or online downloads otherwise.
#'
#' @param data         A \code{data.frame} or \code{sf} object following CWP conventions.
#' @param connectionDB Optional. A DBI-compatible database connection for querying codelists.
#' @param save_prefix  Optional. Filename prefix for saving outputs (\code{.qs} and \code{.csv}).
#' @param shp_raw  Optional. To prevent reading it every time that can be time consuming, we can provide it directly
#'
#' @return A \code{list} with two elements:
#' \describe{
#'   \item{\code{with_geom}}{An \code{sf} object enriched with geometries (CRS EPSG:4326).}
#'   \item{\code{without_geom}}{A \code{data.frame} enriched without geometries.}
#' }
#'
#' @details
#' Steps performed:
#' \enumerate{
#'   \item Load required codelists (species, measurements, gear, fleet, etc.) via SQL or fallback.
#'   \item Normalize measurement units (e.g. tons number of fish.
#'   \item Left-join to add each \code{*_label} column alongside its code.
#'   \item Reorder columns so that each code is immediately followed by its label.
#'   \item Convert to \code{sf} with CRS EPSG:4326.
#'   \item Optionally save to disk if \code{save_prefix} is provided.
#' }
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   species                     = c("YFT", "SKJ"),
#'   measurement_unit            = c("Tons", "Number of fish"),
#'   geographic_identifier       = c("0001", "0002"),
#'   fishing_mode                = "UNK",
#'   fishing_fleet               = "NEI",
#'   measurement_type            = "NC",
#'   measurement                  = "catch",
#'   measurement_processing_level = "raised",
#'   gear_type                   = "99.9"
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
#' @importFrom DBI        dbGetQuery dbIsValid
#' @importFrom utils      download.file unzip
#' @importFrom readr      read_csv
#' @importFrom stringr    str_detect
#' @importFrom janitor     clean_names
#' @importFrom here        here
#' @importFrom dplyr       mutate case_when rename left_join select everything
#' @importFrom sf          st_read st_as_sf st_crs
#' @importFrom data.table fwrite
#'
#' @export
enrich_dataset_if_needed <- function(data, connectionDB = NULL, save_prefix = NULL, shp_raw = NULL) {
  library(dplyr); library(sf); library(readr); library(stringr)
  library(janitor); library(here); library(qs); library(data.table); library(DBI)

  # Fallback query helpers
  try_db_query <- function(con, query, fallback_file, fallback_read_fun, download_url = NULL) {
    if (!is.null(con) && dbIsValid(con)) {
      ok <- try(dbGetQuery(con, "SELECT 1"), silent = TRUE)
      if (!inherits(ok, "try-error")) {
        message("DB ok, running: ", query)
        return(dbGetQuery(con, query))
      }
    }
    message("No valid DB falling back to: ", fallback_file)
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
    dplyr::mutate(measurement_unit = case_when(
      measurement_unit == "Tons"             ~ "t",
      measurement_unit == "Number of fish"   ~ "no",
      TRUE                                   ~ measurement_unit
    ))

  # if WKT col present
  if ("geom_wkt" %in% colnames(data)) {
    data <- dplyr::rename(data, geom = geom_wkt)
  }
  data$geom <- NULL
  con <- tryCatch(connectionDB, error = function(e) NULL)

  # Load all codelists via system.file()
  species_group <- try_db_query(
    con,
    "SELECT taxa_order, code FROM species.species_asfis",
    system.file("extdata", "cl_asfis_species.csv", package = "CWP.dataset"),
    function(f) read_csv(f) %>% janitor::clean_names() %>% dplyr::select(species_label = label, species_group = taxa_order, species = code),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv"
  )

  cl_measurement_processing_level <- try_db_query(
    con,
    "SELECT * FROM measurement_processing_level.measurement_processing_level",
    system.file("extdata", "cl_measurement_processing_level.csv", package = "CWP.dataset"),
    function(f) read_csv(f) %>% janitor::clean_names() %>% dplyr::select(code, measurement_processing_level_label = label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement_processing_level.csv"
  )

  cl_measurement <- try_db_query(
    con,
    "SELECT * FROM measurement.measurement",
    system.file("extdata", "cl_measurement.csv", package = "CWP.dataset"),
    function(f) read_csv(f) %>% janitor::clean_names() %>% dplyr::select(code, measurement_label = label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement.csv"
  )

  cl_fishing_mode <- try_db_query(
    con,
    "SELECT * FROM fishing_mode.fishing_mode",
    system.file("extdata", "cl_fishing_mode.csv", package = "CWP.dataset"),
    function(f) read_csv(f) %>% janitor::clean_names() %>% dplyr::select(code, fishing_mode_label = label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_mode.csv"
  )

  cl_cwp_gear_level2 <- try_db_query(
    con,
    "SELECT * FROM gear_type.isscfg_revision_1",
    system.file("extdata", "cl_isscfg_pilot_gear.csv", package = "CWP.dataset"),
    function(f) read_csv(f) %>% dplyr::select(code, gear_type_label = label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_isscfg_pilot_gear.csv"
  )

  fishing_fleet_label <- try_db_query(
    con,
    "SELECT * FROM fishing_fleet.fishingfleet_firms",
    system.file("extdata", "cl_fishingfleet_firms.csv", package = "CWP.dataset"),
    function(f) read_csv(f) %>% janitor::clean_names() %>% dplyr::select(code, fishing_fleet_label = label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_fleet.csv"
  )

  # measurement_type: still bind two CSVs shipped in inst/extdata
  catch_file  <- system.file("extdata", "cl_catch_concepts.csv", package = "CWP.dataset")
  effort_file <- system.file("extdata", "cl_measurement_types_effort.csv", package = "CWP.dataset")
  measurement_type_df <- dplyr::bind_rows(
    read_csv(catch_file)  %>% clean_names() %>% dplyr::select(code, label),
    read_csv(effort_file) %>% clean_names() %>% dplyr::select(code, label)
  ) %>% dplyr::rename(measurement_type_label = label)

  # measurement-unit labels (global & specific) no change

  # CWP grid (WKT) from extdata
  if(is.null(shp_raw)){
    cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
    if (!file.exists(cwp_grid_file)) {
      stop("cl_areal_grid.csv not found in inst/extdata - run data-raw/download_codelists.R")
    }
    shp_raw <- sf::st_read(cwp_grid_file, show_col_types = FALSE)
  }
  shapefile.fix <- sf::st_as_sf(shp_raw, wkt = "geom_wkt", crs = 4326)
  shapefile.fix <- dplyr::rename(shapefile.fix,
                                 cwp_code = CWP_CODE,
                                 geom     = geom_wkt)
  # Step 2: Join species and gear data
  if("species"%in%colnames(data)){

  enriched_data <- dplyr::left_join(data,
                                    species_group,
                                    by = "species",
                                    suffix = c("", ".y"))%>%dplyr::select(-ends_with(".y"))
  }

  if("gear_type"%in%colnames(enriched_data)){

  enriched_data <- dplyr::left_join(enriched_data,
                                    cl_cwp_gear_level2,
                                    by = c("gear_type" = "code"),
                                    suffix = c("", ".y")) %>%dplyr::select(-ends_with(".y"))
  }
  # Step 3: Join fleet data
  if("fishing_fleet"%in%colnames(enriched_data)){

  enriched_data <- dplyr::left_join(
    enriched_data,
    fishing_fleet_label,
    by = c("fishing_fleet" = "code"),
    suffix = c("", ".y")
  )%>%dplyr::select(-ends_with(".y"))
}
  # Step 4: Join measurement type, measurement, processing level, and mode labels
  if("measurement_type"%in%colnames(enriched_data)){

  enriched_data <- dplyr::left_join(
    enriched_data,
    measurement_type_df,
    by = c("measurement_type" = "code"),
    suffix = c("", ".y")
  )%>%dplyr::select(-ends_with(".y"))

  }

  if("measurement"%in%colnames(enriched_data)){

  enriched_data <- dplyr::left_join(
    enriched_data,
    cl_measurement,
    by = c("measurement" = "code"),
    suffix = c("", ".y")
  )%>%dplyr::select(-ends_with(".y"))
  }

  if("measurement_processing_level"%in%colnames(enriched_data)){

  enriched_data <- dplyr::left_join(
    enriched_data,
    cl_measurement_processing_level,
    by = c("measurement_processing_level" = "code"),
    suffix = c("", ".y")
  )%>%dplyr::select(-ends_with(".y"))
  }

  if("fishing_mode"%in%colnames(enriched_data)){

  enriched_data <- dplyr::left_join(
    enriched_data,
    cl_fishing_mode,
    by = c("fishing_mode" = "code"),
    suffix = c("", ".y")
  )%>%dplyr::select(-ends_with(".y"))
}
  # Step 5: Join gridtype from shapefile.fix
  if("geographic_identifier"%in%colnames(enriched_data)){

  enriched_data <- dplyr::left_join(
    enriched_data,
    dplyr::select(shapefile.fix,
                  geographic_identifier = cwp_code,
                  gridtype            = GRIDTYPE),
    by = "geographic_identifier",
    suffix = c("", ".y")
  )%>%dplyr::select(-ends_with(".y"))
}
  # Step 6: Reorder columns so each code is followed by its label
  cols      <- base::names(enriched_data)
  base_cols <- base::grep("_label$", cols,
                          invert = TRUE,
                          value  = TRUE)
  new_order <- base::unlist(
    base::lapply(base_cols, function(x) c(x, paste0(x, "_label")))
  )
  new_order <- new_order[new_order %in% cols]
  final_order <- base::unique(c(new_order,
                                base::setdiff(cols, new_order)))
  enriched_data <- enriched_data[final_order]

  # Step 7: Convert to sf if not already
  if (!inherits(enriched_data, "sf")) {
    enriched_data <- sf::st_as_sf(enriched_data, crs = 4326)
  }

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
