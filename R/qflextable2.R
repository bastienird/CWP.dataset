#' Create and Save a Flextable
#'
#' @description This function creates a flextable from a data frame, optionally saving it as an image.
#' @param x A data frame or flextable to create the table from.
#' @param captionn An optional character string for the table caption.
#' @param autonumm An optional automatic numbering parameter.
#' @param pgwidth A numeric value for the width of the table.
#' @param columns_to_color Optional columns to apply color coding.
#' @param save_folder Optional folder to save the flextable.
#' @param fig.pathinside A character string for the path to save figures.
#' @param grouped_data Optional grouped data for formatting.
#' @param interactive_plot Logical indicating if the output should be interactive.
#' @return A flextable object or a DT datatable if interactive_plot is TRUE.
#' @export
qflextable2 <- function(x, captionn = NULL, autonumm = autonum, pgwidth = 6, columns_to_color = NULL, save_folder = NULL, fig.pathinside = "Figures", grouped_data = NULL, interactive_plot = FALSE, find_and_print = FALSE) {
  captionn <- eval(captionn)

  if (all(class(x) == "flextable")) {
    flextabley <- x
  } else {
    if (!(all(class(x) == "data.frame"))) {
      x <- as.data.frame(x %>% dplyr::ungroup())
    }
    if (!is.null(save_folder)) {
      if (!dir.exists(file.path(fig.pathinside, save_folder))) {
        dir.create(file.path(fig.pathinside, save_folder), recursive = TRUE)
      }
      save_path_data <- file.path(fig.pathinside, save_folder, paste0(make.names(captionn), ".csv" ))  # Adjust the file name as needed
      fwrite(x, file = save_path_data)
    }

    y <- x %>%
      dplyr::ungroup() %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::mutate_if(is.character, ~ str_replace_all(., ",", ", \n" )) %>%
      dplyr::mutate_if(is.character, ~ str_replace_all(., "_", "-" )) %>%
      dplyr::mutate_if(is.numeric, function(.) { round(., 2) })

    if(!is.null(grouped_data)){
      y <- as_grouped_data(y, groups = grouped_data)
    }

    if (!is.null(columns_to_color)) {
      colormatrix <- ifelse(y %>% dplyr::select(all_of(columns_to_color)) < 0, "red",
                            ifelse(y %>% dplyr::select(all_of(columns_to_color)) > 0, "green", "white"))
      flextabley <- flextable::flextable(y)

      flextabley <- flextabley %>% highlight(j = all_of(columns_to_color), color = colormatrix)


    } else {    flextabley <- flextable::flextable(y)}

  }

  if (!is.null(captionn)) {
    flextable_captionned <- flextable::set_caption(flextabley, caption = captionn, style = "Table Caption")
  } else {
    flextable_captionned <- flextabley
  }



  ft_out <- flextable_captionned %>% autofit()
  ft_out <- width(ft_out, width = dim(ft_out)$widths * pgwidth / flextable_dim(ft_out)$widths)

  if (!is.null(save_folder)) {
    save_path_flextable <- file.path(fig.pathinside, save_folder, paste0(make.names(captionn), ".png"))  # Adjust the file name as needed
    save_as_image(ft_out, path = save_path_flextable)
  }
  if (interactive_plot) {
    # Create an interactive plot using DT
    return(DT::datatable(y))
  }
  ft_out

}
