#' Write a script that opens the explorer on the steps of a report
#'
#' The script is saved with the job, so that a reader of the report can
#' explore the same datasets with filters, in RStudio (button "Source") or with
#' `source()`. The paths of the steps are written relative to the script and
#' resolved when it runs, so the job folder can be moved or opened on another
#' machine (an RStudio Server for instance).
#'
#' The first and the last steps are explored; the intermediate ones are listed
#' as comments, to be uncommented when needed, because every step loaded uses
#' memory.
#'
#' @param step_dirs Directories of the steps, in processing order.
#' @param file Path of the script to write.
#' @param selected Values to select in the filters when the explorer opens
#'   (see `cwp_explore()`). `NULL` elements and the value `"all"` are ignored.
#' @param title Name of the report, written in the header of the script.
#' @return The path of the script, invisibly.
#' @keywords internal
#' @noRd
cwp_write_explorer_script <- function(step_dirs, file = "explorer.R", selected = list(),
                                      title = NULL) {
  if (length(step_dirs) == 0) stop("No step to explore.")
  script_dir <- dirname(fs::path_abs(file))
  relative <- as.character(fs::path_rel(fs::path_abs(step_dirs), start = script_dir))
  step_names <- basename(step_dirs)

  # One line per step; the intermediate ones are commented out
  last <- length(step_dirs)
  step_lines <- vapply(seq_along(step_dirs), function(i) {
    line <- sprintf("  %s = %s%s", deparse(step_names[i]), deparse(relative[i]),
                    if (i == last) "" else ",")
    if (i == 1 || i == last) line else paste0("  # ", sub("^  ", "", line))
  }, character(1))
  if (last > 2) {
    step_lines <- append(step_lines,
                         "  # Intermediate steps: uncomment to explore them too (they use memory)",
                         after = 1)
  }

  selected <- Filter(function(x) length(x) > 0 && !identical(as.character(x), "all"), selected)
  selected_arg <- if (length(selected) == 0) "" else {
    paste0(",\n  list(selected = ", paste(deparse(selected, width.cutoff = 500L), collapse = " "), ")")
  }

  lines <- c(
    paste0("# Explore the datasets of the report", if (!is.null(title)) paste0(" \"", title, "\""),
           " in detail"),
    "#",
    "# In RStudio: open this file and click \"Source\".",
    "# Otherwise: source(\"<path of this file>\").",
    "# Needs the shiny package, and DT for the tables: install.packages(c(\"shiny\", \"DT\"))",
    "",
    "# Folder of this script: the paths of the steps are relative to it",
    "script_dir <- tryCatch({",
    "  sourced <- sys.frame(1)$ofile",
    "  if (is.null(sourced)) getwd() else dirname(normalizePath(sourced))",
    "}, error = function(e) getwd())",
    "",
    "steps <- c(",
    step_lines,
    ")",
    "# Otherwise, the folder where the script was written",
    paste0("if (!all(dir.exists(file.path(script_dir, steps)))) script_dir <- ", deparse(script_dir)),
    "paths <- stats::setNames(as.list(file.path(script_dir, steps)), names(steps))",
    "",
    paste0("app <- do.call(CWP.dataset::cwp_explore, c(\n  paths", selected_arg, "\n))"),
    "shiny::runApp(app)"
  )
  writeLines(lines, file)
  invisible(file)
}
