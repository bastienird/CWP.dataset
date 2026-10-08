#' Summarising_step
#'
#' This function performs various summarizing steps on data related to species and gear types, retrieving data from a database,
#' processing it, and rendering output reports.
#'
#' @param main_dir Character. The main directory containing the entities. (jobs/entities)
#' @param connectionDB Object. The database connection.
#' @param config List. Configuration list containing metadata and options for processing.
#' @param source_authoritylist Vector. Vector of source_authority to filter on, "all" being all of them.
#' @param savestep Logical TRUE/FALSE, should the result of this be saved (.rds) ?
#' @param nameoutput Character, name of the output directory of the report
#' @param usesave Logical Should the results saved by a previous run be used instead of rerunning everything ?
#' @param sizepdf Character string. La taille peut prendre les valeurs suivantes :
#'   \itemize{
#'     \item `"long"` (par défaut) : Long with coverage.
#'     \item `"middle"` : Long without coverage
#'     \item `"short"` : Only first characteristics, first differences and main table of steps
#'   }
#' @param parameter_colnames_to_keep_fact Vector: what column to display
#' @return NULL. The function has side effects, such as writing files and rendering reports.
#' @param render_pdf Logical. Should the PDF report be rendered in addition to the HTML one?
#'   Default `FALSE`: only the HTML report is produced, which needs no LaTeX installation. With
#'   `TRUE`, the PDF is rendered if `lualatex` is available (for instance through TinyTeX);
#'   otherwise a warning is logged and only the HTML report is produced.
#' @param fast_and_heavy Logical TRUE/FALSE, if FALSE, each result is saved to its own .rds file and read back by the chapter that needs it, which uses less memory
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
#' @export
summarising_step <- function(main_dir, connectionDB, config, source_authoritylist = c("all","IOTC","WCPFC", "IATTC", "ICCAT", "CCSBT" ), sizepdf = "long",
                             savestep = FALSE, nameoutput = NULL, usesave = FALSE, fast_and_heavy = TRUE, parameter_colnames_to_keep_fact = NULL,
                             render_pdf = FALSE) {

  if(sizepdf == "long"){
    coverage = TRUE
  } else if(sizepdf %in% c("middle", "short")){
    coverage = FALSE
  } else {
    stop('Please provide a correct sizepdf, "short", "middle" or "long"')
  }

  # The HTML report only needs pandoc and is always rendered. The PDF needs a
  # LaTeX installation: it is rendered only when asked for and when the engine
  # of the report (lualatex, see inst/rmd/index.Rmd) is available.
  pdf_render_available <- FALSE
  if (isTRUE(render_pdf)) {
    pdf_render_available <- nzchar(Sys.which("lualatex"))
    if (!pdf_render_available) {
      futile.logger::flog.warn(
        "PDF demande mais lualatex introuvable (TinyTeX non installe ?) : seul le HTML/gitbook sera genere."
      )
    }
  }

  futile.logger::flog.info(paste0("Size pdf is:", sizepdf))

  ancient_wd <- getwd()
  futile.logger::flog.info("Starting Summarising_step function")

  entity_dirs <- list.dirs(file.path(main_dir, "entities"), full.names = TRUE, recursive = FALSE)
  # entity_dirs <- entity_dirs[2]
  child_env <- new.env(parent = new.env())
  gc()
  futile.logger::flog.info("Initialized child environment")

  i <- 1
  futile.logger::flog.info("Sourced all required functions")
  cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
  if (!file.exists(cwp_grid_file)) {
    stop("cl_areal_grid.csv not found in inst/extdata - run data-raw/download_codelists.R")
  }
  shp_raw <- sf::st_read(cwp_grid_file, show_col_types = FALSE)
  shapefile.fix <- sf::st_as_sf(shp_raw, wkt = "geom_wkt", crs = 4326)
  shapefile.fix <- dplyr::rename(shapefile.fix,
                                 cwp_code = CWP_CODE,
                                 geom     = geom_wkt)
  futile.logger::flog.info("New function")

  continent <- cwp_default_continent()

  if (is.null(continent)) {

    futile.logger::flog.warn("Continent data not found in the database. Fetching from WFS service.")

    url <- "https://www.fao.org/fishery/geoserver/wfs"

    serviceVersion <- "1.0.0"

    logger <- "INFO"

    WFS <- WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0", logger = "INFO")

    continent <- WFS$getFeatures("fifao:UN_CONTINENT2")

    futile.logger::flog.info("Fetched continent data from WFS service")

  }

  for (entity_dir in entity_dirs) {

    futile.logger::flog.info("Processing entity directory: %s", entity_dir)

    # Match the entity on its identifier rather than on its position: the
    # alphabetical order of the directories is not necessarily the config order.
    entities <- config$metadata$content$entities
    entity_ids <- vapply(entities, function(e) {
      id <- tryCatch(as.character(e$identifiers[["id"]]), error = function(err) NA_character_)
      if (length(id) == 1) id else NA_character_
    }, character(1))
    entity_index <- match(basename(entity_dir), entity_ids)
    if (is.na(entity_index)) {
      futile.logger::flog.warn("No entity with id '%s' in config, falling back to position %s",
                               basename(entity_dir), i)
      entity_index <- i
    }
    entity <- entities[[entity_index]]
    i <- i + 1
    action <- entity$data$actions[[1]]
    opts <- action$options

    if(is.null(parameter_colnames_to_keep_fact)){
    if (opts$fact == "effort") {
      futile.logger::flog.warn("Effort dataset not displayed for now")
      parameter_colnames_to_keep_fact = c("source_authority", "fishing_mode_label", "geographic_identifier","fishing_fleet_label","gear_type_label",
                                          "measurement_unit", "measurement_value", "gridtype","species_group", "species_label", "measurement_type")
    } else {
      parameter_colnames_to_keep_fact = c("source_authority", "fishing_fleet_label",
                                          "fishing_mode_label", "geographic_identifier",
                                          "measurement_unit", "measurement_value", "gridtype",
                                           "species_label", "gear_type_label")

    }
    }
    entity_name <- basename(entity_dir)
    setwd(here::here(entity_dir))
    # copy_project_files(original_repo_path = here::here("Analysis_markdown"), new_repo_path = getwd())

    step_dirs <- cwp_list_step_dirs("Markdown")
    futile.logger::flog.info("Listed %s step directories", length(step_dirs))

    for (step_dir in step_dirs) {
      `%notin%` <- Negate(`%in%`)
      # An "ancient" copy means the dataset of this step is already enriched
      if (length(list.files(step_dir, pattern = "^ancient\\.(parquet|rds|qs)$")) == 0) {
        file <- cwp_step_data_path(step_dir)
        data <- read_data(file)
        # copy.date keeps the original date, which gives the order of the steps
        file.copy(from = file, to = file.path(step_dir, paste0("ancient.", tools::file_ext(file))),
                  copy.date = TRUE)
        data <- CWP.dataset::enrich_dataset_if_needed(data, shp_raw = shp_raw, with_geom = FALSE)$without_geom
        data <- data%>%dplyr::mutate(measurement_unit = dplyr::case_when(measurement_unit %in% c("MT","t","MTNO", "Tons")~ "Tons",
                                                                         measurement_unit %in% c("NO", "NOMT","no", "Number of fish")~"Number of fish", TRUE ~ as.character(measurement_unit)))

        cwp_write_step_data(data, step_dir)
        rm(data)
        futile.logger::flog.info("Processed and saved data for step: %s", step_dir)
      } else {
        futile.logger::flog.info("Retrieving processed data: %s", step_dir)
      }
    }
    # Path of the dataset of each step, in processing order
    sub_list_dir_2 <- vapply(step_dirs, cwp_step_data_path, character(1), USE.NAMES = FALSE)
    parameter_resolution_filter <- opts$resolution_filter
    parameter_filtering <- opts$parameter_filtering
    for (s in 1:length(source_authoritylist)){
      if(source_authoritylist[s] == "all"){
        parameter_filtering = opts$parameter_filtering
      } else {
        parameter_filtering$source_authority <- source_authoritylist[s]
      }

      prefix <- paste0(sizepdf, source_authoritylist[s])

      if(usesave & file.exists(paste0(prefix, "renderenv.rds")) | (sizepdf=="short" && file.exists(paste0("long", paste0(source_authoritylist[s],"renderenv.rds"))))){ # if the size pdf is short but the .qs for long exists we can use it
        if(file.exists(paste0(prefix, "renderenv.rds"))){

          render_env <- cwp_read_object(paste0(sizepdf,paste0(source_authoritylist[s],"renderenv.rds")))

        } else if(sizepdf=="short" && file.exists(paste0("long", paste0(source_authoritylist[s],"renderenv.rds")))){

          render_env <- cwp_read_object(paste0("long", source_authoritylist[s], "renderenv.rds"))
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

        if(usesave && file.exists("process_fisheries_data_list.rds")){

          futile.logger::flog.info("Using saved data for process_fisheries_data_list.rds")

        }

        if(!fast_and_heavy && usesave && file.exists(paste0(prefix,"path_to_qs_final.rds"))){

          futile.logger::flog.info("Using saved data for path_to_qs_final.rds")
          child_env_last_result <- NULL

        } else {

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
            parameter_geographical_dimension_groupping = "gridtype",
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
          child_env_last_result$parameter_titre_dataset_1 <- entity$identifiers[["id"]]
          # child_env_last_result$parameter_titre_dataset_2 <- NULL
          if(!fast_and_heavy){
            cwp_save_object(child_env_last_result, paste0(prefix, "path_to_qs_final.rds"))
          }
        }

        if(!fast_and_heavy && usesave && file.exists(paste0(prefix,"path_to_qs_summary.rds"))){

          futile.logger::flog.info("Using saved data for path_to_qs_summary.rds")
          child_env_first_to_last_result <- NULL
          new_path <- file.path(parameters_child_global$fig.path, paste0("/Comparison/initfinal_", basename(sub_list_dir_2[1]), "_", basename(sub_list_dir_2[length(sub_list_dir_2)])))
        } else {

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
            parameter_titre_dataset_1 =  "Initial_data",
            parameter_titre_dataset_2 = entity$identifiers[["id"]],
            parameter_geographical_dimension_groupping = "gridtype",
            unique_analyse = FALSE
          )

          new_path <- file.path(parameters_child_global$fig.path, paste0("/Comparison/initfinal_",  "Initial_data", "_", entity$identifiers[["id"]]))
          dir.create(new_path, recursive = TRUE)
          child_env_first_to_last_result$fig.path <- new_path
          child_env_first_to_last_result$step_title_t_f <- FALSE
          # child_env_first_to_last_result$parameter_short <- FALSE
          # child_env_first_to_last_result$unique_analyse <- FALSE
          child_env_first_to_last_result$parameter_titre_dataset_1 <- "Initial_data"
          child_env_first_to_last_result$parameter_titre_dataset_2 <- entity$identifiers[["id"]]
          child_env_first_to_last_result$child_header <- "#"

          if(!fast_and_heavy){
            cwp_save_object(child_env_first_to_last_result, paste0(prefix,"path_to_qs_summary.rds"))
          }

        }

        sub_list_dir_3 <- dirname(sub_list_dir_2)
        render_env$sub_list_dir_3 <- sub_list_dir_3

        if(!fast_and_heavy && usesave && file.exists(paste0(prefix,"process_fisheries_data_list.rds"))){

          futile.logger::flog.info("Using saved data for process_fisheries_data_list.rds")
          process_fisheries_data_list <- NULL

        } else {

          if(opts$fact == "effort"){
            process_fisheries_data_list <- CWP.dataset::process_fisheries_effort_data(sub_list_dir_3,  parameter_filtering)
          } else {
            process_fisheries_data_list <- CWP.dataset::process_fisheries_data(sub_list_dir_3, parameter_fact = "catch", parameter_filtering)
          }
          if(!fast_and_heavy){

            cwp_save_object(process_fisheries_data_list, paste0(prefix,"process_fisheries_data_list.rds"))
          }
        }
        futile.logger::flog.info("Processed process_fisheries_data_list")

        render_env$process_fisheries_data_list <- process_fisheries_data_list

        futile.logger::flog.info("Adding to render_env")

        if(sizepdf %in% c("long", "middle")){

          final_step <- length(sub_list_dir_3) - 1
          fast_and_heavy_t_f <- fast_and_heavy

          run_comparisons <- function(final_step,
                                      fast_and_heavy = TRUE,
                                      sub_list_dir_3,
                                      shapefile.fix,
                                      continent,
                                      parameters_child_global,
                                      fig.path,
                                      coverage) {

            seq_i <- seq_len(final_step)

            if (fast_and_heavy) {
              all_list <- lapply(seq_i, function(i) {
                # 1) calcul du résultat
                res_i <- CWP.dataset::function_multiple_comparison(
                  i,
                  parameter_short         = FALSE,
                  sub_list_dir            = sub_list_dir_3,
                  shapefile.fix           = shapefile.fix,
                  continent               = continent,
                  parameters_child_global = parameters_child_global,
                  fig.path                = fig.path,
                  coverage                = coverage
                )

                # 2) si ce n’est pas déjà une liste, on l’emballe
                if (!is.list(res_i)) {
                  res_i <- list(value = res_i)
                }

                # 3) si des noms manquent, on les génère
                nms <- names(res_i)
                if (is.null(nms) || any(nms == "")) {
                  nms <- nms %||% rep("", length(res_i))  # %||% = si NULL, remplace par ""
                  empty <- which(nms == "")
                  nms[empty] <- paste0("item", empty)
                  names(res_i) <- nms
                }

                res_i
              })
              return(all_list)
            } else {
              all_paths <- lapply(seq_i, function(i) {

                out_file <- file.path(fig.path,
                                      sprintf("comparison_step_%02d.rds", i))

                if(usesave && file.exists(out_file)){

                  futile.logger::flog.info("comparison_step_%02d.rds already exists, using the cached data", i)

                } else {
                  res_i <- CWP.dataset::function_multiple_comparison(
                    i,
                    parameter_short         = FALSE,
                    sub_list_dir            = sub_list_dir_3,
                    shapefile.fix           = shapefile.fix,
                    continent               = continent,
                    parameters_child_global = parameters_child_global,
                    fig.path                = fig.path,
                    coverage                = coverage
                  )


                  cwp_save_object(res_i, file = out_file)
                  rm(res_i); gc()
                }
                out_file
              })
              return(all_paths)
            }
          }


          all_list <- run_comparisons(
            final_step = final_step,
            fast_and_heavy = fast_and_heavy_t_f,
            sub_list_dir_3           = sub_list_dir_3,
            shapefile.fix            = shapefile.fix,
            continent                = continent,
            parameters_child_global  = parameters_child_global,
            fig.path                 = prefix,
            coverage                 = coverage
          )


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
        render_env$parameter_titre_dataset_1 <- entity$identifiers[["id"]]


        if(fast_and_heavy){
          if(savestep){
            cwp_save_object(render_env, file = paste0(prefix, "renderenv.rds"))
          }
        } else {
          render_env$child_env_first_to_last_result <- NULL
          render_env$child_env_last_result <- NULL
          render_env$process_fisheries_data_list <- NULL
          render_env$path_to_qs_summary <- paste0(prefix, "path_to_qs_summary.rds")
          render_env$path_to_process_fisheries_data_list <- paste0(prefix, "process_fisheries_data_list.rds")
          render_env$path_to_qs_final <- paste0(prefix, "path_to_qs_final.rds")

          # 1) créer un env « propre » sans parent
          minimal_env <- new.env(parent = emptyenv())

          process_paths <- function(x, start) {
            if (is.character(x)&& grepl("[./]", x)) {
              # transforme chaque élément en chemin absolu
              return(fs::path_abs(x, start = start))
            }
            if (is.list(x)) {
              # rappelle process_paths sur chaque sous-élément
              return(lapply(x, process_paths, start = start))
            }
            # si ce n'est ni caractère ni liste, on ignore
            return(NULL)
          }

          # 2) y copier uniquement les bindings de render_env qui vous intéressent
          for (nm in ls(render_env, all.names = TRUE)) {
            val <- render_env[[nm]]
            # si c'est un chemin ou une liste de chemins
            if (is.character(val) || is.list(val)) {
              processed <- process_paths(val, start = getwd())
              # only keep it if there's something non-NULL
              if (!is.null(processed)) {
                minimal_env[[nm]] <- processed
              }
            }
            # sinon, on n'ajoute pas cet objet
          }

          minimal_env$tmap_mode <- "view"
          minimal_env$parameter_titre_dataset_1 <- entity$identifiers[["id"]]
          # 3) sauvegarder le minimal_env à la place de render_env
          cwp_save_object(minimal_env, file = paste0(prefix, "renderenvpath.rds"))

          gc()
        }
      }

      if(is.null(nameoutput)){
        nameoutput <- paste0(prefix,"recappdf")
      }

      set_flextable_defaults(fonts_ignore=TRUE)
      base::options(knitr.duplicate.label = "allow")
      bookdown_path <- CWP.dataset::generate_bookdown_yml(new_session = !fast_and_heavy)
        if(fast_and_heavy){
          futile.logger::flog.info("gitbook")
          bookdown::render_book(
            input = bookdown_path,
            envir = render_env,
            output_format = "bookdown::gitbook",
            output_dir = nameoutput
          )
        } else {

        CWP.dataset::build_book(master_qs_rel = paste0(prefix, "renderenvpath.rds"),
                     output_format = "bookdown::gitbook",
                     output_dir = nameoutput)
        }


        gc()
      if (pdf_render_available) {
        futile.logger::flog.info("pdfdocument")
        tryCatch({
          if (fast_and_heavy) {
            bookdown::render_book(
              ".",
              envir = render_env,
              output_format = "bookdown::pdf_document2",
              output_dir = nameoutput
            )
            gc()
          } else {
            CWP.dataset::build_book(
              master_qs_rel = paste0(prefix, "renderenvpath.rds"),
              output_format = "bookdown::pdf_document2",
              output_dir = nameoutput
            )
          }
        }, error = function(e) {
          futile.logger::flog.warn(
            "Echec du rendu PDF pour %s : %s",
            entity_dir,
            conditionMessage(e)
          )
        })
      }

      unlink("_bookdown.yml")
      nameoutput <- NULL
      rm(child_env_last_result, envir = render_env)
      rm(child_env_first_to_last_result, envir = render_env)
      rm(render_env)

      futile.logger::flog.info("Rendered and uploaded report for entity: %s", entity_dir)
    }

    futile.logger::flog.info("entity: %s is done", entity_dir)

  }
  try(setwd(ancient_wd))
  futile.logger::flog.info("Finished Summarising_step function")
  # return(render_env)
}
