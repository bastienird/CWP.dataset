#' Function to Record Step Details and Save Results
#'
#' This function records the details of each processing step, including explanations, function names, and options. It saves the results as RDS files and text files in a specified directory.
#'
#' @param step_name A character string specifying the name of the step.
#' @param rds_data A data frame containing the data to be saved as an RDS file.
#' @param explanation A character string providing an explanation of the step.
#' @param functions A character string listing the functions used in the step.
#' @param option_list A list of options used in the step.
#' @param entity A geoflow entity
#'
#' @return None
#' @examples
#' \dontrun{
#' function_recap_each_step("step1", data, "This step does X", "function1, function2", list(option1 = "value1"))
#' }
#' @export
#' @importFrom qs qsave
#' @author
#' Bastien Grasset

function_recap_each_step <- function(step_name, rds_data, explanation = "No explanation provided to this step", functions = "No function used in this step", option_list = NULL, entity = NULL) {
  # Initialize global variables if needed
  if (!exists("options_written_total", envir = .GlobalEnv)) {
    assign("options_written_total", "", envir = .GlobalEnv)
  }
  if (!exists("explanation_total", envir = .GlobalEnv)) {
    assign("explanation_total", "", envir = .GlobalEnv)
  }

  # Create directories
  dir.create("Markdown", showWarnings = FALSE)
  step_dir <- file.path("Markdown", step_name)
  dir.create(step_dir, showWarnings = FALSE)

  # Convert to data.table
  dt <- data.table::as.data.table(rds_data)

  # Ensure measurement_value is numeric
  dt[, measurement_value := as.numeric(measurement_value)]

  # Aggregate duplicates
  group_cols <- setdiff(names(dt), "measurement_value")
  dt <- dt[, .(measurement_value = sum(measurement_value, na.rm = TRUE)), by = group_cols]

  # Global sums
  sum_t <- dt[measurement_unit %in% c("t", "MTNO", "MT"), sum(measurement_value, na.rm = TRUE)]
  sum_no <- dt[measurement_unit %in% c("no", "NOMT", "NO"), sum(measurement_value, na.rm = TRUE)]
  lines <- nrow(dt)

  data.table::fwrite(
    data.table::data.table(sum_t = sum_t, sum_no = sum_no, lines = lines),
    file.path(step_dir, "sums.csv")
  )

  # Sums by source_authority
  if ("source_authority" %in% names(dt)) {
    sums_source_auth <- dt[
      ,
      .(
        sum_t = sum(measurement_value[measurement_unit %in% c("t", "MTNO", "MT")], na.rm = TRUE),
        sum_no = sum(measurement_value[measurement_unit %in% c("no", "NOMT", "NO")], na.rm = TRUE),
        lines = .N
      ),
      by = source_authority
    ]

    data.table::fwrite(
      sums_source_auth,
      file.path(step_dir, "sums_source_auth.csv")
    )
  }

  # Sums by species
  if ("species" %in% names(dt)) {
    sums_species <- dt[
      ,
      .(
        sum_t = sum(measurement_value[measurement_unit %in% c("t", "MTNO", "MT")], na.rm = TRUE),
        sum_no = sum(measurement_value[measurement_unit %in% c("no", "NOMT", "NO")], na.rm = TRUE),
        lines = .N
      ),
      by = species
    ]

    data.table::fwrite(
      sums_species,
      file.path(step_dir, "sums_species.csv")
    )
  }

  # Handle options
  if (!is.null(option_list) && length(option_list) != 0) {
    options_substi <- as.list(substitute(option_list))[-1]
    options_written <- paste(
      vapply(
        seq_along(options_substi),
        function(i) paste0(options_substi[[i]], " = ", option_list[[i]]),
        character(1)
      ),
      collapse = " , \n "
    )
  } else {
    options_written <- "NONE"
  }

  # Update globals
  options_written_total <- get("options_written_total", envir = .GlobalEnv)
  explanation_total <- get("explanation_total", envir = .GlobalEnv)

  options_written_total <- paste0(options_written_total, options_written)
  explanation_total <- paste0(explanation_total, explanation)

  assign("options_written_total", options_written_total, envir = .GlobalEnv)
  assign("explanation_total", explanation_total, envir = .GlobalEnv)

  # Save files
  qs::qsave(as.data.frame(dt), file.path(step_dir, "data.qs"))
  write(explanation, file.path(step_dir, "explanation.txt"))
  write(explanation_total, file.path(step_dir, "explanation_total.txt"))
  write(functions, file.path(step_dir, "functions.txt"))
  write(options_written_total, "options_total.txt")
  write(options_written_total, file.path(step_dir, "options_total.txt"))
  write(options_written, file.path(step_dir, "options_written.txt"))

  # Update provenance if entity is provided
  if (!is.null(entity)) {
    if (entity$provenance$statement != "The following processes are applied to the dataset:") {
      entity$provenance$setStatement("The following processes are applied to the dataset:")
      entity$provenance$processes <- NULL
    }
    rationale <- geoflow_process$new()
    rationale$rationale <- explanation
    entity$provenance$addProcess(rationale)
  }

  invisible(dt)
}
