# Step datasets: the dataset saved after each processing step, one per
# directory under "Markdown/". They are tables, so they are stored as Parquet:
# an open and stable format, compact, and readable outside R. Steps written by
# earlier versions of the package ("data.qs") are still read.

# File names a step dataset can have, by order of preference
cwp_step_data_files <- c("data.parquet", "data.qs")

#' Path of the dataset of a processing step
#'
#' @param step_dir Directory of the step.
#' @return The path of `data.parquet` if it exists, otherwise of `data.qs`.
#'   An error if the directory holds neither.
#' @keywords internal
#' @noRd
cwp_step_data_path <- function(step_dir) {
  candidates <- file.path(step_dir, cwp_step_data_files)
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    stop("No step dataset (", paste(cwp_step_data_files, collapse = " or "), ") in: ", step_dir)
  }
  found[1]
}

#' Read the dataset of a processing step
#'
#' @param step_dir Directory of the step.
#' @return A data.frame.
#' @keywords internal
#' @noRd
cwp_read_step_data <- function(step_dir) {
  read_data(cwp_step_data_path(step_dir))
}

#' Write the dataset of a processing step as Parquet
#'
#' If the table cannot be written as Parquet (a column type the format does not
#' support), it is saved as `data.qs` instead, with a warning, so that a
#' processing chain is never interrupted by the storage format.
#'
#' @param data A data.frame (or data.table).
#' @param step_dir Directory of the step.
#' @return The path of the file written, invisibly.
#' @keywords internal
#' @noRd
cwp_write_step_data <- function(data, step_dir) {
  data <- as.data.frame(data)
  parquet_path <- file.path(step_dir, "data.parquet")
  qs_path <- file.path(step_dir, "data.qs")

  written <- tryCatch({
    nanoparquet::write_parquet(data, parquet_path)
    parquet_path
  }, error = function(e) {
    warning("Step dataset could not be written as Parquet (", conditionMessage(e),
            "); saved as data.qs instead: ", step_dir)
    if (file.exists(parquet_path)) file.remove(parquet_path)
    qs::qsave(data, qs_path)
    qs_path
  })
  invisible(written)
}

#' List the step directories in processing order
#'
#' Steps are ordered by the date of their oldest dataset file. The untouched
#' copy kept when a dataset is enriched (`ancient.*`) counts, so the order
#' stays the processing order after the datasets have been rewritten.
#'
#' @param root Directory containing the step directories.
#' @return A character vector of step directories, oldest first.
#' @keywords internal
#' @noRd
cwp_list_step_dirs <- function(root = "Markdown") {
  files <- list.files(root, recursive = TRUE, full.names = TRUE,
                      pattern = "^(data|ancient)\\.(parquet|qs)$")
  # A directory only counts if it holds a dataset, not just a copy
  files <- files[file.exists(file.path(dirname(files), cwp_step_data_files[1])) |
                   file.exists(file.path(dirname(files), cwp_step_data_files[2]))]
  if (length(files) == 0) return(character())

  oldest <- tapply(as.numeric(file.info(files)$mtime), dirname(files), min)
  names(sort(oldest))
}
