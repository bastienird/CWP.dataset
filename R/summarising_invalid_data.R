#' Summarizes invalid data in the provided dataset
#'
#' This function identifies, aggregates, and processes invalid data within a geoflow entity from rawdata in geoflow-tunaatlas workflow by analyzing various
#' entities and their respective tRFMOs. It checks for missing data, incorrect values, and geographic inconsistencies.
#' The function can optionally upload results to a database and Google Drive.
#'
#' @param main_dir The main working directory containing the dataset and necessary files.
#' @param connectionDB A database connection object used for querying relevant tables.
#' @param upload_drive Logical, whether to upload results to Google Drive (default: FALSE).
#' @param upload_DB Logical, whether to upload processed data to a database (default: TRUE).
#'
#' @return Writes multiple summary CSV files and optional database tables, returning no explicit value.
#' @export
#' @importFrom qs qsave qread
#' @importFrom googledrive drive_upload as_id
#' @importFrom futile.logger flog.info
summarising_invalid_data = function(main_dir, connectionDB, upload_drive = FALSE, upload_DB = TRUE){
  ancient_wd <- getwd()
  setwd(main_dir)
  dir.create("Recap_on_pre_harmo")
  path = file.path(getwd())#, "Recap_on_pre_harmo")

  library(sf)
  library(dplyr)
  library(readr)
  library(janitor)
  library(DBI)
  library(here)

  # Definition des chemins
  dir.create(here("data"), showWarnings = FALSE)

  # Fonctions de fallback
  try_db_query <- function(con, query, fallback_file, fallback_read_fun, download_url = NULL) {
    if (!is.null(con) && DBI::dbIsValid(con)) {
      res <- try(DBI::dbGetQuery(con, "SELECT 1"), silent = TRUE)
      if (!inherits(res, "try-error")) {
        message("Connexion a la base reussie, recuperation via SQL : ", query)
        return(DBI::dbGetQuery(con, query))
      }
    }

    message("Pas de connexion valide. ")
    if (!file.exists(fallback_file)) {
      if (!is.null(download_url)) {
        message("Telechargement depuis ", download_url)
        download.file(download_url, fallback_file, mode = "wb")
      } else {
        stop("Fichier requis manquant : ", fallback_file)
      }
    }
    message("Chargement depuis le fichier local : ", fallback_file)
    return(fallback_read_fun(fallback_file))
  }

  try_st_read <- function(con, query, fallback_file, download_url = NULL) {
    if (!is.null(con) && DBI::dbIsValid(con)) {
      res <- try(DBI::dbGetQuery(con, "SELECT 1"), silent = TRUE)
      if (!inherits(res, "try-error")) {
        message("Connexion a la base reussie, lecture via st_read : ", query)
        return(st_read(con, query = query))
      }
    }

    if (!file.exists(fallback_file)) {
      if (!is.null(download_url)) {
        message("Telechargement de ", download_url)
        download.file(download_url, fallback_file, mode = "wb")
        if (grepl("\\.zip$", fallback_file)) {
          unzip(fallback_file, exdir = dirname(fallback_file))
        }
      } else {
        stop("Fichier requis manquant : ", fallback_file)
      }
    }
    message("Lecture du fichier local shapefile : ", fallback_file)
    return(st_read(fallback_file))
  }

  # con <- config$software$output$dbi
  con <- tryCatch(connectionDB, error = function(e) NULL)

  # Recuperation des donnees
  species_group <- try_db_query(
    con,
    "SELECT taxa_order, code FROM species.species_asfis",
    here("data/cl_asfis_species.csv"),
    function(f) read_csv(f) %>% janitor::clean_names() %>% select(species_group = taxa_order, species = code),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv"
  )

  cl_cwp_gear_level2 <- try_db_query(
    con,
    "SELECT * FROM gear_type.isscfg_revision_1",
    here("data/cl_isscfg_pilot_gear.csv"),
    function(f) read_csv(f) %>% select(Code = code, Gear = label),
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/firms/gta/cl_isscfg_pilot_gear.csv"
  )

  # Lecture CWP grid via CSV si pas de DB
  cwp_grid_file <- here("data/cl_areal_grid.csv")
  if (!file.exists(cwp_grid_file)) {
    message("Fichier cl_areal_grid.csv manquant. Telechargement en cours...")
    zip_url <- "https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip"
    zip_path <- here("data/cwp_grid.zip")
    download.file(zip_url, zip_path, mode = "wb")
    unzip(zip_path, exdir = here("data"))
  }
  cwp_grid <- st_read(cwp_grid_file)
  cwp_grid <- st_as_sf(
    cwp_grid,
    wkt = "geom_wkt",   # la colonne contenant les geometries
    crs = 4326          # ou un autre CRS si tu en connais un specifique
  ) %>%
    dplyr::rename(cwp_code = CWP_CODE) %>%
    dplyr::rename(geom = geom_wkt)

  try_get_continent_layer <- function(con = NULL, fallback_file = "UN_CONTINENT2.qs") {
    # 1. Try to read from database
    if (!is.null(con) && DBI::dbIsValid(con)) {
      message("Attempting to read continent layer from database...")
      res <- try(sf::st_read(con, query = "SELECT * FROM public.continent"), silent = TRUE)
      if (!inherits(res, "try-error")) {
        message("Continent layer successfully read from database.")
        sf::st_crs(res) <- 4326
        qs::qsave(res, fallback_file)
        return(res)
      }
    }

    # 2. Try to load from local .qs file
    if (file.exists(fallback_file)) {
      message("Loading continent layer from local file: ", fallback_file)
      continent <- qs::qread(fallback_file)
      sf::st_crs(continent) <- 4326
      return(continent)
    }

    # 3. Download from FAO GeoServer using ows4R
    message("Fetching continent layer from FAO GeoServer...")
    WFS <- ows4R::WFSClient$new(
      url = "https://www.fao.org/fishery/geoserver/fifao/wfs",
      serviceVersion = "1.0.0",
      logger = "INFO"
    )
    continent <- WFS$getFeatures("fifao:UN_CONTINENT2")
    sf::st_crs(continent) <- 4326
    qs::qsave(continent, fallback_file)
    return(continent)
  }
  continent <- try_get_continent_layer(connectionDB)


  shape_without_geom  <- cwp_grid %>% as_tibble() %>%dplyr::select(-geom)
  shapefile.fix <- cwp_grid
  rm(cwp_grid)
  require(CWP.dataset)
  # PART 1: Identify entities and their respective tRFMOs
  entity_dirs <- list.dirs("entities", full.names = TRUE, recursive = FALSE)
  # Function to determine tRFMO from entity name
  determine_tRFMO <- function(entity_name) {
    trfmo_labels <- c("iattc", "wcpfc", "ccsbt", "iotc", "iccat") # the labels to look for
    matched_tRFMO <- character(0) # empty character vector to store matches

    for (trfmo in trfmo_labels) {
      if (grepl(trfmo, entity_name, ignore.case = TRUE)) {
        matched_tRFMO <- toupper(trfmo) # If a match is found, set it to the corresponding tRFMO in uppercase
        break
      }
    }

    if (length(matched_tRFMO) == 0) {
      matched_tRFMO <- "Unknown" # If no matches are found, set tRFMO as "Unknown"
    }

    return(matched_tRFMO)
  }

  entities_trfmo <- lapply(entity_dirs, function(dir) {
    entity_name <- basename(dir)
    trfmo <- determine_tRFMO(entity_name) # Call the function to determine the tRFMO

    return(data.frame(Entity = entity_name, tRFMO = trfmo))
  })

  # Combine all entities into a single data frame
  entities_df <- do.call(rbind, entities_trfmo)


  flog.info("patrt1")
  # PART 2: Checking for specific .csv files

  target_files <- c("negative_values.csv", "not_conform_conversion_factors.csv", "removed_irregular_areas.csv",
                    "areas_in_land.csv", "outside_juridiction.csv")



  entity_file_existence <- list()

  for (entity_dir in entity_dirs) {
    entity_name <- basename(entity_dir)
    current_files <- list.files(paste0(entity_dir, "/data/"), full.names = TRUE)
    file_existence <- setNames(lapply(target_files, function(x) x %in% basename(current_files)), target_files)
    entity_file_existence[[entity_name]] <- file_existence

  }

  results_df <- do.call(rbind, lapply(entity_file_existence, function(x) {
    data <- as.data.frame(t(unlist(x)))
    rownames(data) <- NULL
    return(data)
  }))

  # Add entity names as a column in the data frame
  results_df$Entity <- rownames(results_df)

  # Ensure that 'Entity' columns are factors and have the same levels in both data frames
  entities_df$Entity <- as.factor(entities_df$Entity)
  results_df$Entity <- as.factor(results_df$Entity)

  combined_results <- merge(entities_df, results_df, by = "Entity")

  # Group data by tRFMO
  grouped_results <- combined_results %>%
    # group_by(tRFMO) %>%
    # summarise(across(where(is.logical), sum)) %>%
    distinct()
  readr::write_csv(grouped_results, file.path(entity_dir, "data", "grouped_results_invalid_data.csv"))

  not_mapped_data_list <- list()

  for (entity_dir in entity_dirs) {
    entity_name <- basename(entity_dir)

    # Define the path to the 'not_mapped_total.csv' file for the current entity
    not_mapped_file_path <- file.path(entity_dir, "data", "not_mapped_total.csv")

    # Check if the file exists
    if (file.exists(not_mapped_file_path)) {
      # Read the .csv file and store the data with the entity's name
      not_mapped_data <- readr::read_csv(not_mapped_file_path)
      if(nrow(not_mapped_data)!= 0){
        not_mapped_data$Entity <- entity_name  # add an 'Entity' column to keep track of the entity
        not_mapped_data_list[[entity_name]] <- not_mapped_data
      }
    }
  }

  # Bind all collected data.frames into one
  all_not_mapped_data <- bind_rows(not_mapped_data_list) %>% distinct()

  readr::write_csv(all_not_mapped_data, file.path(path, "all_not_mapped_data.csv"))

  recap_mapping_data_list <- list()

  for (entity_dir in entity_dirs) {
    entity_name <- basename(entity_dir)

    # Define the path to the 'recap_mapping.csv' file for the current entity
    recap_mapping_file_path <- file.path(entity_dir, "data", "recap_mapping.csv")

    # Check if the file exists
    if (file.exists(recap_mapping_file_path)) {
      # Read the .csv file and store the data with the entity's name
      recap_mapping_data <- readr::read_csv(recap_mapping_file_path)
      recap_mapping_data$Entity <- entity_name  # add an 'Entity' column to keep track of the entity
      recap_mapping_data_list[[entity_name]] <- recap_mapping_data
    }
  }

  # Bind all collected data.frames into one
  all_recap_mapping_data <- bind_rows(recap_mapping_data_list) %>% distinct()

  readr::write_csv(all_recap_mapping_data, file.path(path, "all_recap_mapping.csv"))
  flog.info("patrt2")

  # PART 3: Generate a summary CSV for all entity
  `%notin%` <- Negate(`%in%`)
  all_data <- list()
  for (entity_dir in entity_dirs) {
    entity_name <- basename(entity_dir)
    entity_data <- combined_results[combined_results$Entity == entity_name, ]


    problematic_files <- target_files[as.logical(entity_data[3:ncol(entity_data)])]
    # If 'problematic_files' contains NA values, this line will remove them.
    problematic_files <- na.omit(problematic_files)
    problematic_data <- lapply(problematic_files, function(file) {
      data_path <- file.path(entity_dir, "data", file)
      data_list <- readr::read_csv(data_path)
      if("Gear.x" %in% colnames(data_list)){
        # Removing columns that end in '.y' and renaming '.x' columns
        data_list <- data_list %>%
          select(-matches("\\.y$")) %>%  # This removes columns ending in '.y'
          rename_with(~ gsub("\\.x$", "", .x), matches("\\.x$"))  # This renames columns, removing '.x'


      } else if ("gridtype" %notin% colnames(data_list)){
        data_list <- data_list %>% dplyr::mutate(geographic_identifier = as.character(geographic_identifier))
        data_list <- tidying_GTA_data_for_comparison(dataframe = data_list,
                                                     shape = shape_without_geom,
                                                     species_group_dataframe = species_group,
                                                     cl_cwp_gear_level2_dataframe = cl_cwp_gear_level2)
      }
      readr::write_csv(data_list, file = data_path)


      return(data_list)
    })
    flog.info("patrt4")
    if (length(problematic_files) > 0) {
      # Combine all problematic data into one data frame with an additional column specifying the input file
      combined_problematic_data <- do.call(rbind, lapply(1:length(problematic_data), function(i) {
        data_frame <- as.data.frame(problematic_data[[i]])
        data_frame$issue <- problematic_files[i]
        return(data_frame)
      }))

      all_data[[entity_name]] <- combined_problematic_data
      # Combine all dataframes into one


      # Write the combined data frame to a CSV file
      write_csv(combined_problematic_data, file.path(entity_dir, paste0(entity_name, "_summary_invalid_data.csv")),
                progress = show_progress())

    }
  }
  if(length(all_data) !=0){
    combined_data <- bind_rows(all_data, .id = "entity_name")
    combined_data <- combined_data  %>%
      dplyr::rename(dataset  = entity_name) %>%
      dplyr::mutate(issue = gsub(".csv","", issue)) %>%
      dplyr::rename(codesource_area = geographic_identifier)
    combined_data$time_start <- as.Date(combined_data$time_start)
    combined_data$time_end <- as.Date(combined_data$time_end)
    combined_data$year <- as.integer(format(combined_data$time_end, "%Y"))
    combined_data$month <- as.integer(format(combined_data$time_end, "%m"))
    combined_data$quarter <- as.integer(substr(quarters(combined_data$time_end), 2, 2))
    # Save the combined data as a .qs file
    qs::qsave(combined_data,"All_invalid_data.qs")
    if(upload_DB){
      dbExecute(connectionDB, "DROP MATERIALIZED VIEW IF EXISTS public.issueddata CASCADE;")
      dbWriteTable(connectionDB, "temp_tableissueddata", combined_data, temporary = TRUE, row.names = FALSE, append = FALSE)
      dbExecute(connectionDB, "
    CREATE MATERIALIZED VIEW public.issueddata AS
    SELECT * FROM temp_tableissueddata;
  ")
      dbExecute(connectionDB, "REFRESH MATERIALIZED VIEW public.issueddata;")
      # dbExecute(connectionDB, "DROP TABLE IF EXISTS temp_tableissueddata CASCADE;")
    }
  }
  # Directory for the R Markdown template
  base::options(knitr.duplicate.label = "allow")


  # Parameters for child Rmd
  # load(here(".RData"))


  parameters_child <- list(
    parameter_colnames_to_keep = c("fishing_fleet", "gear_type", "geographic_identifier",
                                   "fishing_mode", "species", "measurement_unit", "measurement_value",
                                   "Gear", "species_group", "gridtype"),
    shapefile.fix = shapefile.fix,
    outputonly = FALSE,
    print_map = TRUE,
    parameter_time_dimension = c("time_start"),
    unique_analyse = TRUE, child_header = "",continent = continent,
    child = TRUE, parameter_final = NULL
  )

  child_env_base <- new.env(parent = environment())
  list2env(parameters_child, env = child_env_base)
  source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/Functions_markdown.R", local = child_env_base)

  child_env <- list2env(as.list(child_env_base), parent = child_env_base)
  for (entity_dir in entity_dirs) {
    entity_name <- basename(entity_dir)
    entity_data <- combined_results[combined_results$Entity == entity_name, ]

    # Identify problematic files
    problematic_files <- target_files[as.logical(entity_data[3:ncol(entity_data)])]
    problematic_files <- na.omit(problematic_files)
    problematic_files <- setdiff(problematic_files, "not_mapped_total.csv")

    if (length(problematic_files) > 0) {
      output_file_name <- paste0(entity_name, "_report.html") # name of the output file
      render_env <- new.env(parent = child_env)
      process_rds_file <- function(file_path, parameter_short, parameters_child_global, fig.path, shapefile.fix, continent, coverage = TRUE) {
        # Creer un chemin pour les figures
        file_name <- tools::file_path_sans_ext(basename(file_path))

        # Ajouter des lignes specifiques en fonction du fichier
        Addlines <- switch(
          file_name,
          "outside_juridiction" = paste0(
            "# Outside juridiction area\n\n",
            "The data displayed by tRFMOs is supposed to concern the spatial area of the juridiction of the tRFMO. However, some data is displayed outside.\n"
          ),
          "areas_in_land" = paste0(
            "# Overview of data located on land\n\n",
            "Only the squares where the integrity of the area is located on land are considered in the analysis.\n The analysis has been perform using the cwp grid dataset that can be found : https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip",
            "\n\n More details : https://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/main-water-areas/fr/#c737133"
          ),
          "removed_irregular_areas" = paste0(
            "# Area not in CWP grid\n\n",
            "Some squares do not correspond to the CWP grid standards.\n"
          ),
          "not_conform_conversion_factors" = paste0(
            "# Not conform conversion factors\n\n",
            "Some data provided in Number of fish and tons are not plausible.\n"
          ),
          "negative_values" = paste0(
            "# Negative or null values in provided data\n\n",
            "Some data are provided with a measurement_value inferior or equal to 0.\n"
          ),
          "not_mapped_total" = paste0(
            "# Not mapped data\n\n",
            "Some data provided does not correspond to any mapping.\n"
          ),
          # Valeur par defaut si le fichier n'a pas de description
          paste0("# Unknown issue\n\nNo specific description available for this dataset.\n")
        )
        file_path <- readr::read_csv(file_path) %>% dplyr::mutate(geographic_identifier = as.numeric(geographic_identifier))
        # Generer l'environnement pour ce fichier
        child_env_result <- comprehensive_cwp_dataframe_analysis(
          parameter_init = file_path,
          parameter_final = NULL,
          parameter_fact = "catch",
          plotting_type = "plot",
          parameter_colnames_to_keep = c(
            # "source_authority",
            "species", "gear_type", "fishing_fleet",
            "fishing_mode", "geographic_identifier", "measurement_unit",
            "measurement_value", "gridtype", "species_group", "Gear"
          ),
          shapefile_fix = shapefile.fix,
          continent = continent,
          coverage = coverage,
          parameter_resolution_filter = NULL,
          parameter_filtering = NULL,
          parameter_titre_dataset_1 = file_name,
          unique_analyse = TRUE
        )

        # Ajouter les parametres supplementaires a l'environnement
        child_env_result$step_title_t_f <- FALSE
        child_env_result$treatment <- FALSE
        child_env_result$parameter_titre_dataset_1 <- file_name
        child_env_result$child_header <- ""
        child_env_result$Add_lines <- Addlines

        gc()

        flog.info(paste("Analysis completed for:", file_name))
        return(child_env_result)
      }

      target_files_path <- paste0(file.path("entities",entity_name,"data",target_files))

      target_files_path <- target_files_path[file.exists(target_files_path)]

      all_list <- lapply(target_files_path, process_rds_file,
                         parameter_short = FALSE,
                         fig.path = render_env$fig.path,
                         shapefile.fix = shapefile.fix,
                         continent = continent,
                         coverage = TRUE)

      all_list <- all_list[!sapply(all_list, is.null)]
      render_env$directory <- entity_dir
      render_env$dataset <- entity_name
      render_env$all_list <- all_list
      # Use the function (make sure to use the correct local paths)
      # rmarkdown::render("summary_invalid_data_test_index.Rmd",
      #                   output_dir = entity_dir, output_file =output_file_name ,
      #                   envir = render_env
      # )
      summary_invalid_data <- read_csv(file.path(entity_dir, paste0(entity_name, "_summary_invalid_data.csv")))
      render_env$summary_invalid_data <- summary_invalid_data
      qs::qsave(render_env, file.path(entity_dir, paste0(entity_name, "render_env.qs")))

      Report_on_raw_data <- system.file("rmd", "Report_on_raw_data.Rmd", package = "CWP.dataset")
      rmarkdown::render(input = Report_on_raw_data, envir = render_env, output_format = "bookdown::html_document2",
                        output_dir =entity_dir, output_file = entity_name)

      message(sprintf("Rendering: %s", entity_name, e$message))


      # rmarkdown::render(input = "Report_on_raw_data_iccat.Rmd", envir = render_env, output_format = "bookdown::html_document2",
      #                   output_dir ="~/firms-gta/geoflow-tunaatlas/jobs/20250117135138_raw_data_georef/entities/catch_iccat_level0", output_file = "catch_iccat_level0_detailed.html")
      # rmarkdown::render(input = "Report_on_raw_data.Rmd", envir = render_env, output_format = "pdf_document",
      #                   output_dir =entity_dir, output_file = entity_name)
      # rmarkdown::render(input = "Report_on_raw_data.Rmd", envir = render_env, output_format = "bookdown::markdown_document2",
      #                   output_dir =entity_dir, output_file = entity_name)
      rm(render_env, envir = environment())
    }
    # Recap_on_pre_harmo <- system.file("rmd", "Recap_on_pre_harmo.Rmd", package = "CWP.dataset")
    # rmarkdown::render(Recap_on_pre_harmo,
    #                   output_dir = path,
    #                   envir = environment()
    # )

    # rmarkdown::render(Recap_on_pre_harmo,
    #                   output_dir = path,
    #                   envir = environment(), output_format = "pdf_document2")
  }
  folder_datasets_id <- "1s8sCv6j_3-zHR1MsOqhrqZrGKhGY3W_Y"

  all_files <- list.files(getwd(), pattern = "\\.html$", full.names = TRUE, recursive = TRUE)

  if(upload_drive){
    sapply(all_files, function(file) {
      destination_file <- file.path(getwd(),"Recap_on_pre_harmo", basename(file))
      file.copy(file, destination_file)
      path_to_dataset_new <- file.path(file)
      drive_upload(path_to_dataset_new, as_id(folder_datasets_id), overwrite = TRUE)

    })
    #
    path_Recap <- file.path(getwd(),"Recap_on_pre_harmo.html")
    drive_upload(path_Recap, as_id(folder_datasets_id), overwrite = TRUE)
    read_last_csv <- function(path) {
      csv_files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
      if (length(csv_files) == 0) return(NULL)
      last_csv <- csv_files[order(file.info(csv_files)$mtime, decreasing = TRUE)[1]]
      read_csv(last_csv)
    }

    # Liste des tRFMOs, n'inclut pas iattc car pas de binding
    tRFMOs <- c("ccsbt", "wcpfc")
    list_csv <- c()

    combined_data_list <- lapply(tRFMOs, function(trfmo) {
      trfmo_paths <- list.dirs(file.path(path, "entities"), recursive = FALSE)
      trfmo_paths <- trfmo_paths[!grepl("nominal", trfmo_paths)]
      if(length(trfmo_paths) != 0){
        trfmo_paths <- trfmo_paths[grep(trfmo, trfmo_paths)]

        trfmo_data <- lapply(file.path(trfmo_paths, "data"), read_last_csv)
        trfmo_data <- do.call(rbind, trfmo_data)

        # Enregistrement du fichier combine
        name <- paste0(path, "/", trfmo, "_combined_data.csv")
        write_csv(trfmo_data, name)
        return(name)
      } else {return(NULL)}
    })

    drive_upload_safe <- function(data_path) {
      tryCatch({
        drive_upload(data_path, as_id("1fXgxn-spBydGrFLtsrayVMLrQ2LOCkeg"), overwrite = TRUE)
      }, error = function(e) {
        message(sprintf("Failed to upload %s: %s", data_path, e$message))
        return(NULL)  # Returning NULL or any other indication of failure
      })
    }

    # Apply the safe upload function to each path in your list
    result_list <- lapply(combined_data_list, drive_upload_safe)

  }

  setwd(ancient_wd)

}
