# WARNING - Generated by {fusen} from dev/flat_first.Rmd: do not edit by hand

#' Save Plots with Subfigures in a Knitr Environment
#'
#' @description This function saves plots and creates subfigures for rendering in RMarkdown.
#' @param plot The plot object to save.
#' @param title A character string for the plot title.
#' @param folder The folder where the plot will be saved.
#' @param fig.pathinside The path for saving the figure.
#' @return None
#' @export
knitting_plots_subfigures <- function(plot, title, folder = "Unknown_folder", fig.pathinside = fig.path) {
  # Check if the function is being run in a knitr environment
  in_knitr <- !is.null(knitr::opts_knit$get("out.format"))

  # Save the ggplot object in the current environment with a unique name
  if(is_ggplot(plot)) {
    save_image(title = title, plott = plot, folder = folder, fig.pathinside = fig.pathinside)
    if(in_knitr) {
      # This will run if inside a knitr/RMarkdown environment

      # Adjust title for use in fig.cap
      assign("title_adj", gsub("_", "-", title), envir = environment())
      assign("plot_obj", plot, envir = environment())

      # Create the R chunk as a string referencing the ggplot object by its name
      knitr::knit_child(text = c(
        '```{r evolvaluedimdiff, fig.cap=`title_adj`, fig.align = "center", out.width = "100%", results= "asis"}',
        '',
        '',
        'plot(plot_obj)',
        '',
        '```'
      ), envir = environment(), quiet = TRUE)
    } else {
      # This will run if outside a knitr/RMarkdown environment (e.g., in a plain R script)
      print(plot)
    }
  } else if (inherits(plot, "tmap")) {
    if(in_knitr) {
      # This will run if inside a knitr/RMarkdown environment

      # Adjust title for use in fig.cap
      assign("title_adj", gsub("_", "-", title), envir = environment())
      assign("plot_obj", plot, envir = environment())

      # Create the R chunk as a string referencing the ggplot object by its name
      knitr::knit_child(text = c(
        '```{r evolvaluedimdiff, fig.cap=`title_adj`, fig.align = "center", out.width = "100%", results= "asis"}',
        '',
        '',
        'plot_obj',
        '',
        '```'
      ), envir = environment(), quiet = TRUE)
    } else {
      # This will run if outside a knitr/RMarkdown environment (e.g., in a plain R script)
      plot
    }
  } else {
    stop("Not a ggplot or tmap object")
  }
}

#' Render Subfigures from a List of Plots
#'
#' @description This function creates a grid of subfigures from a list of plots and titles.
#' @param plots_list A list of plot objects.
#' @param titles_list A list of character strings for plot titles.
#' @param general_title A character string for the general title of the plots.
#' @return None
#' @export
render_subfigures <- function(plots_list, titles_list, general_title) {
  # Check if the function is being run in a knitr environment
  in_knitr <- !is.null(knitr::opts_knit$get("out.format"))
  # Check if the lists are of the same length
  if (length(plots_list) != length(titles_list)) {
    stop("The lengths of plots_list and titles_list are not the same.")
  }
  # Check if all plots are ggplots
  if (!all(sapply(plots_list, inherits, "gg"))) {
    stop("Not all items in plots_list are ggplot objects.")
  }
  if (in_knitr) {
    # Create subcaptions
    subcaps <- lapply(titles_list, function(title) paste0("(", title, ")"))
    # Start creating the dynamic R chunk as a string
    chunk_header <- sprintf(
      '```{r subfigures, fig.cap="%s", fig.subcap=c(%s), fig.ncol=2, out.width="50%%", fig.align="center"}',
      general_title,
      paste0('"', subcaps, '"', collapse=", ")
    )
    # Assign each plot in plots_list to a new variable in the environment
    for (i in seq_along(plots_list)) {
      assign(paste0("plot_", i), plots_list[[i]], envir = environment())
    }
    # Create a string for each plot command, referring to the correct plot objects
    plot_commands <- lapply(seq_along(plots_list), function(i) {
      paste0("print(plot_", i, ")")
    })
    # Combine all elements into a single character string
    chunk_text <- paste(chunk_header, paste(plot_commands, collapse = '\n'), '```', sep = '\n')
    # Render the chunk
    result <- knitr::knit_child(text = chunk_text, envir = environment(), quiet = TRUE)
    cat(result)
  } else {
    # If not in a knitr environment, just print the plots
    for (plot in plots_list) {
      print(plot)
    }
  }
}
