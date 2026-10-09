# Build a bookdown project in isolated sessions

This function automates the process of building a Bookdown project with
`new_session = TRUE`. It copies and injects setup and chapter files,
loads a master environment saved with
[`cwp_save_object()`](https://bastienird.github.io/CWP.dataset/reference/cwp_save_object.md),
and restores the working directory after completion.

## Usage

``` r
build_book(
  master_qs_rel,
  orig_setup_rmd = system.file("rmd/Setup_markdown.Rmd", package = "CWP.dataset"),
  src_paths = c(system.file("rmd/index.Rmd", package = "CWP.dataset"),
    system.file("rmd/first_and_first_to_last_and_process.Rmd", package = "CWP.dataset"),
    system.file("rmd/all_child_process.Rmd", package = "CWP.dataset"),
    system.file("rmd/Annexenew.Rmd", package = "CWP.dataset")),
  book_filename = "tableau_recapbookdowntest",
  output_dir = "_book",
  template = system.file("rmd/template.tex", package = "CWP.dataset"),
  output_format = "bookdown::pdf_document2",
  new_session = TRUE,
  delete_merged_file = TRUE,
  root = here::here()
)
```

## Arguments

- master_qs_rel:

  Path to the master environment file (.rds), relative to the initial
  working directory.

- orig_setup_rmd:

  Path to the Setup_markdown.Rmd file (typically via
  [`system.file()`](https://rdrr.io/r/base/system.file.html)).

- src_paths:

  Character vector of source Rmd paths to include in the book.

- book_filename:

  Filename (without extension) for the output book.

- output_dir:

  Directory where the rendered book will be placed.

- template:

  Path to the LaTeX template (.tex) to use for PDF output.

- output_format:

  Output format string for `render_book`, e.g. "bookdown::pdf_book" or
  "bookdown::html_book".

- new_session:

  Logical; if TRUE, each chapter is rendered in a new R session.

- delete_merged_file:

  Logical; passed to Bookdown to delete intermediate merged files.

- root:

  Root directory of the project where Bookdown should run (defaults to
  here::here()).

## Examples

``` r
if (FALSE) { # \dontrun{
build_book(
  master_qs_rel   = "everything_for_bookdown_fixed.rds",
  orig_setup_rmd  = system.file("rmd/Setup_markdown.Rmd", package = "CWP.dataset"),
  src_paths       = c(
    system.file("rmd/index.Rmd", package = "CWP.dataset"),
    system.file("rmd/first_and_first_to_last_and_process.Rmd", package = "CWP.dataset"),
    system.file("rmd/all_child_process.Rmd", package = "CWP.dataset"),
    system.file("rmd/Annexenew.Rmd", package = "CWP.dataset")
  ),
  book_filename      = "tableau_recapbookdowntest",
  output_dir         = "_book",
  template           = system.file("rmd/template.tex", package = "CWP.dataset"),
  output_format      = "bookdown::pdf_document2",
  new_session        = TRUE,
  delete_merged_file = TRUE
)
} # }
```
