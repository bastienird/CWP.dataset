compare_temporal_differences_dygraphs <- function(parameter_time_dimension, init, final, titre_1, titre_2, unique_analyse = FALSE) {
  require(dplyr)
  require(dygraphs)
  require(xts)

  # Combine and group data across time dimensions
  Groupped_all_time <- data.frame()
  for (i in parameter_time_dimension) {
    temporaire <- fonction_groupement(i, init, final)
    Groupped_all_time <- rbind(Groupped_all_time, temporaire)
  }

  # Generate dygraphs for each measurement unit and time dimension
  timediffplot <- lapply(parameter_time_dimension, function(filtering_unit) {
    measurement_units <- unique(Groupped_all_time$measurement_unit[Groupped_all_time$Dimension == filtering_unit])

    # Create a list of dygraphs for each measurement unit
    lapply(measurement_units, function(unit) {
      create_dygraph_for_unit(Groupped_all_time, filtering_unit, unit)
    })
  })

  titles <- paste0("Difference in percent of value for the dimension ", parameter_time_dimension, " for ", titre_1, " and ", titre_2, " dataset")

  return(list(plots = timediffplot, titles = titles))
}
