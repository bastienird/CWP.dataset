#' Summarising_step
#'
#' This function performs various summarizing steps on data related to species and gear types, retrieving data from a database,
#' processing it, and rendering output reports.
#'
#' @param main_dir Character. The main directory containing the entities. (jobs/entities)
#' @param connectionDB Object. The database connection.
#' @param config List. Configuration list containing metadata and options for processing.
#' @param source_authoritylist Vector. Vector of source_authority to filter on, "all" being all of them.
#' @param savestep Logical TRUE/FALSE, should the .qs result of this be saved ?
#' @param nameoutput Character, name of the .qs if saved
#' @param usesave Logical Should we use the nameoutput .qs instead of rerunning everything ?
#' @return NULL. The function has side effects, such as writing files and rendering reports.
#' @param sizepdf Character string. La taille peut prendre les valeurs suivantes :
#'   \itemize{
#'     \item `"long"` (par défaut) : Long with coverage.
#'     \item `"middle"` : Long without coverage
#'     \item `"short"` : Only first characteristics, first differences and main table of steps
#'   }
#'
#' @examples
#' \dontrun{
#' connectionDB <- DBI::dbConnect(RSQLite::SQLite(), ":memory:") # Connexion temporaire
#' config <- list() # Simule une configuration
#' summarising_step(main_dir = "chemin/vers/dossier", connectionDB, config)
#' }
#' @import dplyr
#' @import sf
#' @importFrom futile.logger flog.info flog.warn flog.error
#' @importFrom qs qread qsave
#' @export
summarising_step <- function(main_dir, connectionDB, config, source_authoritylist = c("all","IOTC","WCPFC", "IATTC", "ICCAT", "CCSBT" ), sizepdf = "long",
                             savestep = FALSE, nameoutput = NULL, usesave = FALSE) {

  if(sizepdf == "long"){
    coverage = TRUE
  } else if(sizepdf %in% c("middle", "short")){
    coverage = FALSE
  } else {
    stop('Please provide a correct sizepdf')
  }

  futile.logger::flog.info(paste0("Size pdf is:", sizepdf))


  ancient_wd <- getwd()
  futile.logger::flog.info("Starting Summarising_step function")

  species_group <- st_read(connectionDB, query = "SELECT taxa_order, code FROM species.species_asfis") %>%
    janitor::clean_names() %>%
    dplyr::select(species_group = taxa_order, species = code)
  futile.logger::flog.info("Loaded species_group data")

  if(!file.exists("data/cl_fishing_mode.csv")){
    url <- "https://raw.githubusercontent.com/fdiwg/fdi-codelists/31756d4c0baf44c6d7d851e93c14c1e6917f7276/global/firms/gta/cl_fishing_mode.csv"
    destination <- "data/cl_fishing_mode.csv"

    utils::download.file(url, destination, method = "curl")
  }
  setwd(ancient_wd)
  cl_fishing_mode <- readr::read_csv("data/cl_fishing_mode.csv")

  species_label <- st_read(connectionDB, query = "SELECT * FROM species.species_asfis") %>%
    janitor::clean_names()
  fishing_fleet_label <- st_read(connectionDB, query = "SELECT * FROM fishing_fleet.fishingfleet_firms") %>%
    janitor::clean_names()

  cl_cwp_gear_level2 <- st_read(connectionDB, query = "SELECT * FROM gear_type.isscfg_revision_1") %>%
    dplyr::select(Code = code, Gear = label)

  futile.logger::flog.info("Loaded cl_cwp_gear_level2 data")

  cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
  if (!file.exists(cwp_grid_file)) {
    stop("cl_areal_grid.csv not found in inst/extdata — run data-raw/download_codelists.R")
  }
  shapefile.fix <- st_read(cwp_grid_file) %>%
    st_as_sf(wkt = "geom_wkt", crs = 4326) %>%
    rename(cwp_code = CWP_CODE, geom = geom_wkt)

  continent <- tryCatch({
    st_read(connectionDB, query = "SELECT * FROM public.continent")
  }, error = function(e) {
    futile.logger::flog.error("An error occurred while reading continent data: %s", e$message)
    NULL
  })

  if (is.null(continent)) {
    futile.logger::flog.warn("Continent data not found in the database. Fetching from WFS service.")
    url <- "https://www.fao.org/fishery/geoserver/wfs"
    serviceVersion <- "1.0.0"
    logger <- "INFO"
    WFS <- WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")
    continent <- WFS$getFeatures("fifao:UN_CONTINENT2")
    futile.logger::flog.info("Fetched continent data from WFS service")
  }

  shape_without_geom <- shapefile.fix %>%
    as_tibble() %>%
    dplyr::select(-geom)
  futile.logger::flog.info("Processed shapefile.fix data")

  entity_dirs <- list.dirs(file.path(main_dir, "entities"), full.names = TRUE, recursive = FALSE)
  # entity_dirs <- entity_dirs[2]
  child_env <- new.env(parent = new.env())
  gc()
  futile.logger::flog.info("Initialized child environment")

  i <- 1
  futile.logger::flog.info("Sourced all required functions")

  for (entity_dir in entity_dirs) {

    futile.logger::flog.info("Processing entity directory: %s", entity_dir)

    entity <- config$metadata$content$entities[[i]]
    i <- i + 1
    action <- entity$data$actions[[1]]
    opts <- action$options

    if (opts$fact == "effort") {
      futile.logger::flog.warn("Effort dataset not displayed for now")
      parameter_colnames_to_keep_fact = c("source_authority", "fishing_mode", "geographic_identifier","fishing_fleet", "gear_type",
                                          "measurement_unit", "measurement_value", "gridtype","species_group")
      topnumberfact = 3
    } else {
      parameter_colnames_to_keep_fact = c("source_authority", "species_label", "gear_type_label", "fishing_fleet_label",
                                          "fishing_mode_label", "geographic_identifier",
                                          "measurement_unit_label", "measurement_value", "gridtype", "measurement_processing_level_label")
      topnumberfact = 6

    }
      entity_name <- basename(entity_dir)
      setwd(here::here(entity_dir))
      # copy_project_files(original_repo_path = here::here("Analysis_markdown"), new_repo_path = getwd())

      sub_list_dir_2 <- list.files("Markdown", recursive = TRUE, pattern = "data.qs", full.names = TRUE)
      details <- file.info(sub_list_dir_2)
      details <- details[with(details, order(as.POSIXct(mtime))), ]
      sub_list_dir_2 <- rownames(details)
      futile.logger::flog.info("Processed sub_list_dir_2")

      for (file in sub_list_dir_2) {
        `%notin%` <- Negate(`%in%`)
        if (!file.exists(gsub(pattern = basename(file), replacement = "ancient.qs", file))) {
          data <- qs::qread(file)
          file.copy(from = file, to = gsub(pattern = basename(file), replacement = "ancient.qs", file))

          if ("gridtype" %notin% colnames(data)) {
            data <- CWP.dataset::enrich_dataset_if_needed(data)
            qs::qsave(data, file = file)
            futile.logger::flog.info("Processed and saved data for file: %s", file)
          }
        } else {
          futile.logger::flog.info("Retrieving processed data: %s", file)

      }
      }
      parameter_resolution_filter <- opts$resolution_filter
      parameter_filtering <- opts$parameter_filtering
      for (s in 1:length(source_authoritylist)){
        if(source_authoritylist[s] == "all"){
          parameter_filtering = opts$parameter_filtering
        } else {
      parameter_filtering$source_authority <- source_authoritylist[s]
        }
        if(usesave){
        if(file.exists(paste0(sizepdf, paste0(source_authoritylist[s],"renderenv.qs")))){

          render_env <- qs::qread(paste0(sizepdf,paste0(source_authoritylist[s],"renderenv.qs")))

        } else if(sizepdf=="short" && file.exists(paste0("long", paste0(source_authoritylist[s],"renderenv.qs")))){

          render_env <- qs::qread(paste0(sizepdf,paste0(source_authoritylist[s],"renderenv.qs")))
          assign("all_list", NULL, envir = render_env)
        }
        }else {

      parameters_child_global <- list(
        fig.path = paste0("tableau_recap_global_action/figures/"),
        parameter_filtering = parameter_filtering,
        parameter_resolution_filter = parameter_resolution_filter
      )

      output_file_name <- paste0(entity_name, "_report.html")

      render_env <- list2env(as.list(child_env), parent = child_env)
      list2env(parameters_child_global, envir = render_env)
      child_env_last_result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
        parameter_init = sub_list_dir_2[length(sub_list_dir_2)],
        parameter_final = NULL,
        fig.path = parameters_child_global$fig.path,
        parameter_fact = opts$fact,
        parameter_colnames_to_keep = parameter_colnames_to_keep_fact,
        coverage = TRUE,
        shapefile_fix = shapefile.fix,
        continent = continent,
        parameter_resolution_filter = parameters_child_global$parameter_resolution_filter,
        parameter_filtering = parameters_child_global$parameter_filtering,
        parameter_titre_dataset_1 = entity$identifiers[["id"]],
        unique_analyse = TRUE
      )

      filename <- paste0("Report_on_", entity$identifiers[["id"]])
      new_path <- file.path(render_env$fig.path, filename)
      dir.create(new_path, recursive = TRUE)
      child_env_last_result$fig.path <- new_path
      child_env_last_result$step_title_t_f <- FALSE
      # child_env_last_result$parameter_short <- FALSE
      child_env_last_result$child_header <- "#"
      # child_env_last_result$unique_analyse <- TRUE
      # child_env_last_result$parameter_titre_dataset_1 <- entity$identifiers[["id"]]
      # child_env_last_result$parameter_titre_dataset_2 <- NULL

      child_env_first_to_last_result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
        parameter_init = sub_list_dir_2[1],
        parameter_final = sub_list_dir_2[length(sub_list_dir_2)],
        fig.path = parameters_child_global$fig.path,
        parameter_fact = opts$fact,
        parameter_colnames_to_keep = parameter_colnames_to_keep_fact,
        shapefile_fix = shapefile.fix,
        continent = continent,
        coverage = TRUE,
        parameter_resolution_filter = parameters_child_global$parameter_resolution_filter,
        parameter_filtering = parameters_child_global$parameter_filtering,
        parameter_titre_dataset_1 = "FirmsLevel0",
        parameter_titre_dataset_2 = entity$identifiers[["id"]],
        unique_analyse = FALSE
      )

      new_path <- file.path(parameters_child_global$fig.path, paste0("/Comparison/initfinal_", basename(sub_list_dir_2[1]), "_", basename(sub_list_dir_2[length(sub_list_dir_2)])))
      dir.create(new_path, recursive = TRUE)
      child_env_first_to_last_result$fig.path <- new_path
      child_env_first_to_last_result$step_title_t_f <- FALSE
      # child_env_first_to_last_result$parameter_short <- FALSE
      # child_env_first_to_last_result$unique_analyse <- FALSE
      child_env_first_to_last_result$parameter_titre_dataset_1 <- "Initial_data"
      child_env_first_to_last_result$parameter_titre_dataset_2 <- entity$identifiers[["id"]]
      child_env_first_to_last_result$child_header <- "#"

      sub_list_dir_3 <- gsub("/data.qs", "", sub_list_dir_2)
      render_env$sub_list_dir_3 <- sub_list_dir_3
      if(opts$fact == "effort"){
        process_fisheries_data_list <- process_fisheries_effort_data(sub_list_dir_3,  parameter_filtering)
        } else {
      process_fisheries_data_list <- process_fisheries_data(sub_list_dir_3, parameter_fact = "catch", parameter_filtering)
        }
      futile.logger::flog.info("Processed process_fisheries_data_list")

      render_env$process_fisheries_data_list <- process_fisheries_data_list

      futile.logger::flog.info("Adding to render_env")



      if(sizepdf %in% c("long", "middle")){

      final_step <- length(sub_list_dir_3) - 1
      all_list <- lapply(1:final_step, CWP.dataset::function_multiple_comparison, parameter_short = FALSE, sub_list_dir = sub_list_dir_3,
                         shapefile.fix = shapefile.fix,
                         continent = continent,
                         parameters_child_global = parameters_child_global, fig.path = render_env$fig.path, coverage = coverage)

      futile.logger::flog.info("all_list processed")

      all_list <- all_list[!is.na(all_list)]

      render_env$all_list <- all_list

      } else{

        rm(all_list, envir = render_env)
        assign("all_list", NULL, envir = render_env)

      }

      render_env$child_env_first_to_last_result <- child_env_first_to_last_result
      render_env$child_env_last_result <- child_env_last_result
      gc()

      render_env$plotting_type <- "view"
      render_env$fig.path <- new_path

      if(savestep){
        qs::qsave(render_env, file = paste0(sizepdf, paste0(source_authoritylist[s],"renderenv.qs")))
      }
        }

        if(is.null(nameoutput)){
          nameoutput <- paste0(sizepdf, paste0(source_authoritylist[s],"recappdf"))
        }

      set_flextable_defaults(fonts_ignore=TRUE)
      base::options(knitr.duplicate.label = "allow")
      bookdown_path <- CWP.dataset::generate_bookdown_yml()

      if(sizepdf != "short"){
      futile.logger::flog.info("gitbook")
          bookdown::render_book(
            input = bookdown_path,
            envir = render_env,
            output_format = "bookdown::gitbook",
            output_dir = nameoutput
          )


      gc()
      }
      futile.logger::flog.info("pdfdocument")
      bookdown::render_book(".", envir = render_env,
                            output_format = "bookdown::pdf_document2",
                            output_dir = nameoutput)
      unlink("_bookdown.yml")
      nameoutput <- NULL
      rm(child_env_last_result, envir = render_env)
      rm(child_env_first_to_last_result, envir = render_env)
      rm(render_env)
      gc()

      # drive_upload("tableau_recap_global_action_effort.html", as_id(folder_datasets_id), overwrite = TRUE)
      futile.logger::flog.info("Rendered and uploaded report for entity: %s", entity_dir)
      }

    sprintf("entity: %s is done", entity_dir)

    }
  try(setwd(ancient_wd))
  futile.logger::flog.info("Finished Summarising_step function")
  # return(render_env)
}
