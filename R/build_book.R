#' Build a bookdown project in isolated sessions
#'
#' This function automates the process of building a Bookdown project with `new_session = TRUE`.
#' It copies and injects setup and chapter files, loads a master .qs environment,
#' and restores the working directory after completion.
#'
#' @param master_qs_rel Path to the master .qs file, relative to the initial working directory.
#' @param orig_setup_rmd Path to the Setup_markdown.Rmd file (typically via `system.file()`).
#' @param src_paths Character vector of source Rmd paths to include in the book.
#' @param book_filename Filename (without extension) for the output book.
#' @param output_dir Directory where the rendered book will be placed.
#' @param template Path to the LaTeX template (.tex) to use for PDF output.
#' @param output_format Output format string for `render_book`, e.g. "bookdown::pdf_book" or "bookdown::html_book".
#' @param new_session Logical; if TRUE, each chapter is rendered in a new R session.
#' @param delete_merged_file Logical; passed to Bookdown to delete intermediate merged files.
#' @param root Root directory of the project where Bookdown should run (defaults to here::here()).
#'
#' @import bookdown yaml fs qs here knitr
#' @export
#'
#' @examples
#' \dontrun{
#' build_book(
#'   master_qs_rel   = "everything_for_bookdown_fixed.qs",
#'   orig_setup_rmd  = system.file("rmd/Setup_markdown.Rmd", package = "CWP.dataset"),
#'   src_paths       = c(
#'     system.file("rmd/index.Rmd", package = "CWP.dataset"),
#'     system.file("rmd/first_and_first_to_last_and_process.Rmd", package = "CWP.dataset"),
#'     system.file("rmd/all_child_process.Rmd", package = "CWP.dataset"),
#'     system.file("rmd/Annexenew.Rmd", package = "CWP.dataset")
#'   ),
#'   book_filename      = "tableau_recapbookdowntest",
#'   output_dir         = "_book",
#'   template           = system.file("rmd/template.tex", package = "CWP.dataset"),
#'   output_format      = "bookdown::pdf_document2",
#'   new_session        = TRUE,
#'   delete_merged_file = TRUE
#' )
#' }
build_book <- function(master_qs_rel,
                       orig_setup_rmd = system.file("rmd/Setup_markdown.Rmd", package = "CWP.dataset"),
                       src_paths = c(
                         system.file("rmd/index.Rmd", package = "CWP.dataset"),
                         system.file("rmd/first_and_first_to_last_and_process.Rmd", package = "CWP.dataset"),
                         system.file("rmd/all_child_process.Rmd", package = "CWP.dataset"),
                         system.file("rmd/Annexenew.Rmd", package = "CWP.dataset")
                       ),
                       book_filename = "tableau_recapbookdowntest",
                       output_dir = "_book",
                       template = system.file("rmd/template.tex", package = "CWP.dataset"),
                       output_format = "bookdown::pdf_document2",
                       new_session = TRUE,
                       delete_merged_file = TRUE,
                       root = here::here()) {
  # Store original working directory and compute absolute master Qs path
  orig_wd <- getwd()
  master_qs_abs <- fs::path_abs(master_qs_rel, start = orig_wd)

  # Read and fix the master environment paths
  absolute_path_env <- qs::qread(master_qs_abs)
  fixed_env <- new.env(parent = emptyenv())
  for (nm in ls(absolute_path_env, all.names = TRUE)) {
    val <- get(nm, envir = absolute_path_env)
    if (is.character(val) && grepl("[./]", val)) {
      val <- fs::path_abs(val, start = orig_wd)
    }
    assign(nm, val, envir = fixed_env)
  }
  fixed_qs <- "everything_for_bookdown_fixed.qs"
  if (file.exists(fixed_qs)) unlink(fixed_qs)
  qs::qsave(fixed_env, fixed_qs)
  master_qs_abs <- fs::path_abs(fixed_qs, start = orig_wd)

  # Prepare Bookdown configuration
  dest_files <- basename(src_paths)
  book_cfg <- list(
    book_filename      = book_filename,
    output_dir         = output_dir,
    delete_merged_file = delete_merged_file,
    new_session        = new_session,
    rmd_files          = dest_files,
    template           = template
  )

  # Set working directory to project root
  setwd(root)

  # Write _bookdown.yml
  yaml::write_yaml(book_cfg, "_bookdown.yml")

  # Copy setup and source Rmd files
  fs::file_copy(orig_setup_rmd, "Setup_markdown.Rmd", overwrite = TRUE)
  fs::file_copy(src_paths, dest_files, overwrite = TRUE)

  # Inject load and setup chunks into each Rmd
  for (dest in dest_files) {
    lines <- readLines(dest, warn = FALSE)
    chunk_load_master <- c(
      "```{r load_master, include=FALSE}",
      sprintf("master_env <- qs::qread('%s')", master_qs_abs),
      "master_list <- as.list(master_env, all.names = TRUE)",
      "rm(master_env)",
      "knitr::opts_chunk$set(duplicate.label = 'allow')",
      "list2env(master_list, envir = .GlobalEnv)",
      "rm(master_list)",
      "```",""
    )
    chunk_setup <- c(
      "```{r setup, include=FALSE, child='Setup_markdown.Rmd'}", "" ,"```",""
    )
    writeLines(c(chunk_load_master, chunk_setup, lines), con = dest)
  }
  # Build the book
  bookdown::render_book(
    input         = dest_files,
    config_file   = "_bookdown.yml",
    output_dir = paste0(orig_wd, "/_book"),
    output_format = output_format
  )

  # Cleanup temporary files
  fs::file_delete(dest_files)
  fs::file_delete("Setup_markdown.Rmd")
  fs::file_delete("_bookdown.yml")
  # Restore original working directory
  setwd(orig_wd)
  fs::file_delete(fixed_qs)

  message("✅ Build completed successfully.")
}
