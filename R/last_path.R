#' Extract Last Part of a File Path
#'
#' @description This function returns the last part of a file path after removing a specific suffix.
#' @param x A character string representing the file path.
#' @return A character string representing the last part of the file path.
#' @export
last_path <- function(x){
  x <- gsub("/rds.rds", "", x)
  substr(x, max(gregexpr("/", x)[[1]]) + 1, nchar(x))
}
