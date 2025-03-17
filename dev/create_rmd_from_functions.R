create_rmd_from_r_script <- function(input_script, output_rmd) {
  # Read the R script
  script_lines <- readLines(input_script)

  # Initialize a list to store the Rmd content
  rmd_content <- list()

  # Variables to track function name and content
  in_function <- FALSE
  function_name <- NULL
  function_content <- NULL

  for (line in script_lines) {
    # Detect function start
    if (grepl("<- function", line)) {
      if (in_function) {
        # If already in a function, close the previous function and add chunks
        rmd_content <- c(rmd_content, create_rmd_chunks(function_name, function_content))
      }
      # Start a new function
      function_name <- sub(".*([a-zA-Z0-9_]+)\\s*<-\\s*function.*", "\\1", line)
      function_content <- line
      in_function <- TRUE
    } else if (in_function) {
      # Append function content
      function_content <- paste(function_content, line, sep = "\n")
      # Check for function end
      if (grepl("\\}", line)) {
        # End of the function
        rmd_content <- c(rmd_content, create_rmd_chunks(function_name, function_content))
        in_function <- FALSE
        function_name <- NULL
        function_content <- NULL
      }
    }
  }

  # Handle any remaining function
  if (in_function) {
    rmd_content <- c(rmd_content, create_rmd_chunks(function_name, function_content))
  }

  # Write the Rmd content to the output file
  writeLines(rmd_content, output_rmd)
}

create_rmd_chunks <- function(function_name, function_content) {
  # Create the chunks for RMarkdown
  rmd_chunks <- c(
    paste0("```{r function-", function_name, "}\n", function_content, "\n```"),
    paste0("\n```{r examples-", function_name, "}\n", function_name, "(inputs)\n```"),
    paste0("\n```{r tests-", function_name, "}\ntest_that(\"", function_name, " works properly and show error if needed\", {\n  NULL\n})\n```")
  )

  return(rmd_chunks)
}

# Example usage:
# create_rmd_from_r_script("input_script.R", "output_document.Rmd")
create_rmd_from_r_script("~/CWP.dataset/test.R", "output_document.Rmd")
