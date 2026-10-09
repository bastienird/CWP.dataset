#' Remove large objects from a function's environment when it exits
#'
#' A ggplot object keeps references to the environment of the function that
#' built it (plot environment, `aes()` expressions, internal ggplot2 objects),
#' and a function environment also keeps the arguments that were never
#' evaluated, which point to the caller's environment. Saving such a plot
#' therefore saves everything those environments still hold: typically the full
#' datasets, although the plot only needs a small aggregated table.
#'
#' These references cannot be removed reliably from the plot, so the datasets
#' are dropped from the environments instead. Use it as
#' `on.exit(cwp_forget(c("init", "final"), environment()), add = TRUE)` at the
#' top of any function that receives or reads a full dataset and returns plots.
#'
#' @param names Character vector of object names. Names that do not exist are ignored.
#' @param envir The environment to clean, usually `environment()`.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
cwp_forget <- function(names, envir) {
  names <- intersect(names, ls(envir, all.names = TRUE))
  if (length(names) > 0) {
    rm(list = names, envir = envir)
  }
  invisible(NULL)
}
