#' Stop with a clear message when an optional package is missing
#'
#' Packages only needed by an optional feature are listed in `Suggests`, not in
#' `Imports`, so that installing CWP.dataset does not install them. A function
#' that needs one of them calls this first.
#'
#' @param package Name of the package.
#' @param reason What the package is needed for, to complete the message.
#' @return `TRUE`, invisibly, when the package is installed.
#' @keywords internal
#' @noRd
cwp_require_package <- function(package, reason) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("The '", package, "' package is needed ", reason,
         ". Install it with install.packages(\"", package, "\").", call. = FALSE)
  }
  invisible(TRUE)
}
