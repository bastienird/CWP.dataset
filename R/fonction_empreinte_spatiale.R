#' Spatial Footprint Function
#'
#' This function generates a spatial representation of measurement values from two datasets,
#' allowing for comparison between initial and final datasets using a provided shapefile.
#'
#' @param variable_affichee A character string indicating the measurement unit to be displayed.
#' @param initial_dataset A data.table containing the initial measurement data (default: init).
#' @param final_dataset A data.table containing the final measurement data (default: final).
#' @param titre_1 A character string for the title of the first dataset (default: "Dataset 1").
#' @param titre_2 A character string for the title of the second dataset (default: "Dataset 2").
#' @param shapefile.fix A spatial object (sf) for the polygons that defines the geographical areas.
#' @param plotting_type A character string indicating the type of plot ("plot" or "view").
#' @param continent An optional spatial object for adding continent borders to the plot.
#'
#' @return A plot object representing the spatial footprint of the measurement values.
#' @export
fonction_empreinte_spatiale <- function(variable_affichee, initial_dataset = init, final_dataset = final,
                                        titre_1 = "Dataset 1", titre_2 = "Dataset 2",
                                        shapefile.fix = NULL, plotting_type = "plot", continent = NULL) {

  if(is.null(shapefile.fix)){
    stop("Please provide a shape for the polygons")
  }

  selection <- function(x) {
    x[, .(geographic_identifier = as.character(geographic_identifier),
          measurement_value,
          GRIDTYPE,
          measurement_unit)]
  }

  Initial_dataframe <- selection(as.data.table(initial_dataset))
  Final_dataframe <- selection(as.data.table(final_dataset))

  Initial_dataframe[, source := "Initial_dataframe"]
  Final_dataframe[, source := "Final_dataframe"]

  geo_data <- rbind(Initial_dataframe, Final_dataframe, use.names = TRUE, fill = TRUE)
  geo_data[, source := fifelse(source == "Initial_dataframe", titre_1,
                               fifelse(source == "Final_dataframe", titre_2, "Error"))]

  inner_join_data <- geo_data[, .(measurement_value = sum(measurement_value, na.rm = TRUE)),
                         by = .(geographic_identifier, measurement_unit, source, GRIDTYPE)][
                           measurement_value != 0]

  inner_join_data <- sf::st_as_sf(inner_join(inner_join_data,
                                shapefile.fix %>% select(code, geom),
                                by = c("geographic_identifier" = "code")))

  if (nrow(inner_join_data%>% dplyr::filter(measurement_unit == variable_affichee)) != 0) {
      # inner_join_data <- inner_join_data %>%
      #   dplyr::mutate(Group = paste0(GRIDTYPE, "_", source))

      if (plotting_type == "view") {
image <- tmap::tm_shape(inner_join_data %>% dplyr::filter(measurement_unit == variable_affichee)) +
  tmap::tm_fill("measurement_value", palette = "RdYlGn", style = "cont", n = 8, id = "name", midpoint = 0, fill.free = TRUE) +
  tmap::tm_layout(legend.outside = FALSE) +
  tmap::tm_facets(by = c("GRIDTYPE", "source"))

      } else {
image <- tmap::tm_shape(inner_join_data %>% dplyr::filter(measurement_unit == variable_affichee)) +
  tmap::tm_fill("measurement_value", palette = "RdYlGn", style = "cont", n = 8, id = "name", midpoint = 0, fill.free = TRUE) +
  tmap::tm_layout(legend.outside = FALSE) +
  tmap::tm_facets(by = c("GRIDTYPE", "source")) +  # Suppression de `free.scales = TRUE`
  tmap::tm_shape(continent) +
  tmap::tm_borders()

      }

    return(image)
  }
}
