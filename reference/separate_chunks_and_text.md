# Separate R Code Chunks and Text from an RMarkdown (.Rmd) File and Optionally Save Them

This function reads an RMarkdown file and separates the code chunks from
the text. It returns a list with two elements: `chunks` (R code chunks)
and `text` (non-code content). Optionally, it saves the chunks to an
`.R` file and the text to a `.txt` file.

## Usage

``` r
separate_chunks_and_text(rmd_file, save_files = FALSE)
```

## Arguments

- rmd_file:

  A character string specifying the path to the .Rmd file.

- save_files:

  Logical. If TRUE, saves the chunks to an `.R` file and the text to a
  `.txt` file. Default is FALSE.

## Value

A list containing two elements:

- chunks:

  A character vector with the content of all R code chunks.

- text:

  A character vector with the content outside of R code chunks.
