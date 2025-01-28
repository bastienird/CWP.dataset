#' Check if Variable is NULL or Does Not Exist
#'
#' @description This function checks if a variable is either NULL or does not exist in the environment.
#' @param x The variable to check.
#' @return Logical value indicating whether the variable is NULL or does not exist.
#' @export
is_null_or_not_exist <- function(x) {
  var_name <- deparse(substitute(x))
  if (!exists(var_name, envir = parent.frame()) ||
      is.null(get(var_name, envir = parent.frame()))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
