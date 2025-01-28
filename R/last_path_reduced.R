#' Reduce Last Path for Georeferenced Datasets
#'
#' @description This function modifies the last path by removing specific substrings for clarity.
#' @param x A character string representing the file path.
#' @return A character string representing the modified last path.
#' @export
last_path_reduced <- function(x) {
  gsub("georef_dataset", "", last_path(x))
}
