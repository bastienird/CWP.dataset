#!/usr/bin/env Rscript

urls <- c(
  cl_asfis_species                = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv",
  cl_measurement_processing_level = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement_processing_level.csv",
  cl_measurement                  = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement.csv",
  cl_fishing_mode                 = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_mode.csv",
  cl_isscfg_pilot_gear            = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_isscfg_pilot_gear.csv",
  cl_fishingfleet_firms           = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_fishing_fleet.csv",
  cl_catch_concepts               = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_catch_concepts.csv",
  cl_measurement_types_effort     = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/fdi/cl_measurement_types_effort.csv",
  cl_areal_grid                   = "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_areal_grid.csv"
)

dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)

# Downloading each csv
for (nm in names(urls)) {
  dest <- file.path("inst/extdata", paste0(nm, ".csv"))
  message("Downloading ", nm)
  download.file(urls[nm], dest, mode = "wb")
}

library(readr)
download_codelists <- lapply(names(urls), function(nm) {
  path <- file.path("inst/extdata", paste0(nm, ".csv"))
  read_csv(path, show_col_types = FALSE)
})
names(download_codelists) <- names(urls)

usethis::use_data(download_codelists, overwrite = TRUE)
