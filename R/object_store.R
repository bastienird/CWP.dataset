#' Save and read R objects on disk
#'
#' The intermediate results of a report (lists of results, environments) are R
#' objects, not tables: they are stored as `.rds`, the native format of R, which
#' needs no package and stays readable by any version of R. Tables go to
#' Parquet instead (see [read_data()]).
#'
#' `cwp_read_object()` also reads `.qs` files written by earlier versions of the
#' package, provided the {qs} package is installed.
#'
#' @param x Any R object.
#' @param file Path of the file. `cwp_save_object()` expects a `.rds` path.
#' @param compress Logical. Compress the file? Default `FALSE`: writing and
#'   reading are faster, which matters more than size for intermediate results.
#' @return `cwp_save_object()` returns `file`, invisibly. `cwp_read_object()`
#'   returns the object.
#' @examples
#' file <- tempfile(fileext = ".rds")
#' cwp_save_object(list(a = 1), file)
#' cwp_read_object(file)
#' @export
cwp_save_object <- function(x, file, compress = FALSE) {
  saveRDS(x, file = file, compress = compress)
  invisible(file)
}

#' @rdname cwp_save_object
#' @export
cwp_read_object <- function(file) {
  if (grepl("\\.qs$", file)) {
    return(cwp_read_legacy_qs(file))
  }
  readRDS(file)
}

#' Read a `.qs` file written by an earlier version of the package
#'
#' @param file Path of the `.qs` file.
#' @return The object.
#' @keywords internal
#' @noRd
cwp_read_legacy_qs <- function(file) {
  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("'", file, "' was written with the {qs} package by an earlier version of CWP.dataset. ",
         "Install {qs} to read it, or generate the file again.")
  }
  # qs is no longer on CRAN, so it cannot be declared as a dependency of the
  # package: its reader is looked up at run time instead of with qs::qread()
  read_qs <- getExportedValue("qs", "qread")
  read_qs(file)
}
