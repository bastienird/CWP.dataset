#' Check if an Object is a ggplot
#'
#' @description This function checks if an object is a ggplot object.
#' @param obj The object to check.
#' @return Logical value indicating whether the object is a ggplot.
#' @export
is_ggplot <- function(obj) {
  inherits(obj, "gg") || inherits(obj, "ggplot")
}
