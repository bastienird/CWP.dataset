#' Generate a `_bookdown.yml` file dynamically
#'
#' This function creates a `_bookdown.yml` file in the specified directory
#' to define the structure and configuration for a Bookdown project.
#' It allows specifying the output directory, merging behavior, and the list
#' of `.Rmd` files required for the Bookdown compilation.
#'
#' @param destination Character. The directory where the `_bookdown.yml` file
#'   will be created. Default is `"bookdown_run"`.
#'
#' @return No return value. The function writes `_bookdown.yml` in the
#'   specified destination directory.
#'
#' @examples
#' # Generate a `_bookdown.yml` in the default directory
#' generate_bookdown_yml()
#'
#' # Generate a `_bookdown.yml` in a custom directory
#' generate_bookdown_yml("custom_bookdown_dir")
#'
#' @export
generate_bookdown_yml <- function(destination = system.file("rmd", package = "CWP.dataset")) {
  yml_content <- c(
    'book_filename: "tableau_recapbookdowntest"',
    'output_dir: "_book"',
    'delete_merged_file: true',
    'new_session: false',
    'rmd_files:',
    paste0('  - "', destination, '/index.Rmd"'),
    paste0('  - "', destination, '/first_and_first_to_last_and_process.Rmd"'),
    paste0('  - "', destination, '/all_child_process.Rmd"'),
    paste0('  - "', destination, '/Annexenew.Rmd"'),
    paste0('template: "', system.file("rmd/template.tex", package = "CWP.dataset"), '"')
  )


  writeLines(yml_content, file.path(getwd(), "_bookdown.yml"))
  return(file.path(getwd(), "_bookdown.yml"))
}
