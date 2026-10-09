# A tiny job, as written by a processing chain: one entity with two steps,
# built from the example datasets of the package. Nothing is stored in the
# repository: the job is created in a temporary directory with the same
# function a real chain uses, function_recap_each_step().

#' @param steps Named list of datasets, in processing order.
#' @param entity_id Identifier of the entity.
#' @return A list with the job directory (`main_dir`), the entity directory
#'   and a `config` object shaped like the one geoflow gives to
#'   summarising_step().
create_mini_job <- function(steps, entity_id = "mini_catch_dataset", fact = "catch") {
  main_dir <- tempfile("mini_job_")
  entity_dir <- file.path(main_dir, "entities", entity_id)
  dir.create(entity_dir, recursive = TRUE)

  old_wd <- setwd(entity_dir)
  on.exit(setwd(old_wd), add = TRUE)
  # function_recap_each_step() accumulates its texts in two global variables
  on.exit(suppressWarnings(rm(list = c("options_written_total", "explanation_total"),
                              envir = .GlobalEnv)), add = TRUE)

  start <- Sys.time() - 3600
  for (i in seq_along(steps)) {
    step_name <- names(steps)[i]
    function_recap_each_step(
      step_name, steps[[i]],
      explanation = paste("Explanation of", step_name),
      functions = "none"
    )
    # The order of the steps is the order of their files in time
    files <- list.files(file.path("Markdown", step_name), full.names = TRUE)
    Sys.setFileTime(files, start + 60 * i)
  }

  config <- list(metadata = list(content = list(entities = list(
    list(
      identifiers = list(id = entity_id),
      data = list(actions = list(list(options = list(
        fact = fact,
        resolution_filter = NULL,
        parameter_filtering = list(species = NULL, fishing_fleet = NULL)
      ))))
    )
  ))))

  list(main_dir = main_dir, entity_dir = entity_dir, config = config)
}
