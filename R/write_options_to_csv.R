#' Write options to CSV and return structured output
#'
#' This function writes a CSV file containing all the options of the data column of the entity of geoflow for GTA workflows
#' Additionally, it returns a structured list containing the options renamed as a named list.
#'
#' @param opts A named list where each element is a vector of options corresponding to an entity.
#'
#' @return A list containing:
#'   \item{options}{A named list where each key is `options_<name>` and the value is a concatenated string of options.}
#'   \item{table}{A data frame with two columns: "Options" (names of the options) and "Position" (concatenated values).}
#'
#' @export
write_options_to_csv <- function(opts) {
  list_options <- data.frame(Options = character(),
                             Position = character(),
                             stringsAsFactors = FALSE)

  options_list <- list()

  for (i in names(opts)) {
    if (i != "") {
      option_value <- paste(opts[[i]], collapse = ' ; ')
      options_list[[paste0("options_", i)]] <- option_value

      option_data <- data.frame(Options = i, Position = option_value, stringsAsFactors = FALSE)
      list_options <- rbind(list_options, option_data)
    }
  }

  if (nrow(list_options) > 0) {
    write.csv(list_options, "list_options.csv", row.names = FALSE)
  }

  return(list(options = options_list))
}
