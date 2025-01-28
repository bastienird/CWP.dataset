#' Create a Title with Optional Child Header
#'
#' @description This function formats a title with optional child headers for output.
#' @param x A character string representing the title.
#' @param child_headerinside An optional character string for the child header.
#' @return A formatted character string.
#' @export
cat_title <- function(x, child_headerinside = "") {
  if(is.null(child_headerinside)){
    child_headerinside <- ""
  }
  if(child_headerinside == "-#"){
    x <- sub("#", "", x)
    child_headerinside = ""
  }

  output = paste0(child_headerinside, x, " \n")
  return(output)
}
