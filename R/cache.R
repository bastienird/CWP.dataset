# Session-level cache for reference data shipped with the package (codelists,
# CWP grid). Reading and parsing them on every call is expensive, and they do
# not change during a session.
.cwp_cache <- new.env(parent = emptyenv())

#' Get a value from the package cache, computing it on first use
#'
#' @param key Character. Cache key.
#' @param expr Expression evaluated (once) to fill the cache.
#' @return The cached value.
#' @keywords internal
#' @noRd
cwp_cached <- function(key, expr) {
  if (!exists(key, envir = .cwp_cache, inherits = FALSE)) {
    assign(key, expr, envir = .cwp_cache)
  }
  get(key, envir = .cwp_cache, inherits = FALSE)
}

#' Empty the package cache (codelists and CWP grid)
#'
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
cwp_clear_cache <- function() {
  rm(list = ls(.cwp_cache, all.names = TRUE), envir = .cwp_cache)
  invisible(NULL)
}
