#' @title Geographic Differences
#' @description This function analyzes geographic data differences between two datasets.
#' @param init Initial dataset
#' @param final Final dataset
#' @param shapefile_fix Shapefile to be used
#' @param parameter_geographical_dimension Parameter for geographical dimension
#' @param parameter_geographical_dimension_groupping Parameter for geographical dimension grouping
#' @param continent Shapefile of the continent
#' @param plotting_type Type of plotting ("plot" or other)
#' @param titre_1 Title for the first dataset
#' @param titre_2 Title for the second dataset
#' @param outputonly Boolean to specify if output should be saved only
#' @param map_engine Character. `"tiles"` (default) draws the grid cells as ggplot2 tiles, which is
#'   much faster than one polygon per cell but gives a static map. `"tmap"` keeps the previous
#'   tmap rendering (interactive in HTML when `tmap_mode("view")`). The default can be set for a
#'   whole session with `options(CWP.dataset.map_engine = "tmap")`. If the cells cannot be drawn
#'   as tiles, tmap is used.
#' @return A list containing the geographic differences and a saved image
#' @export
#' @import dplyr
#' @import tmap
#' @importFrom tmap tm_shape tm_fill tm_facets tm_layout tm_borders
#' @importFrom ggplot2 ggsave
#' @importFrom sf st_as_sf
geographic_diff <- function(init, final, shapefile_fix, parameter_geographical_dimension,
                            parameter_geographical_dimension_groupping, continent, plotting_type,
                            titre_1, titre_2, outputonly,
                            map_engine = getOption("CWP.dataset.map_engine", "tiles")) {

  impact_levels <- c("Appearing data", "Gain (more than double)", "Gain",
                     "No differences", "Loss", "All data lost")

  geographic_dimension <- CWP.dataset::fonction_groupement(c(parameter_geographical_dimension, parameter_geographical_dimension_groupping),
                                                           init = init , final = final) %>%
    dplyr::filter(value_sum_1 != 0 | value_sum_2 != 0) %>%
    dplyr::mutate(`Impact on the data` = dplyr::case_when(`Difference (in %)` == Inf ~ "Appearing data",
                                                          Inf > `Difference (in %)`  & `Difference (in %)` >= 100 ~ "Gain (more than double)",
                                                          100 > `Difference (in %)`  & `Difference (in %)` > 0 ~ "Gain",
                                                          `Difference (in %)` == 0 ~ "No differences",
                                                          0 > `Difference (in %)`  & -100 < `Difference (in %)` ~ "Loss",
                                                          `Difference (in %)` == -100 ~  "All data lost")) %>%
    dplyr::mutate(`Impact on the data` = factor(`Impact on the data`, levels = impact_levels))

  title = paste0("Spatial differences between ", titre_1, " and ", titre_2, " dataset")
  impact_palette <- rev(RColorBrewer::brewer.pal(6, "PiYG"))

  if (identical(map_engine, "tiles")) {
    image <- tryCatch({
      tiles <- cwp_grid_tiles(shapefile_fix, geographic_dimension$Precision)
      plot_data <- if (is.null(tiles)) NULL else {
        merge(data.table::as.data.table(geographic_dimension), tiles,
              by.x = "Precision", by.y = "code")
      }
      if (is.null(plot_data) || nrow(plot_data) == 0) NULL else {
        tile_palette <- impact_palette
        names(tile_palette) <- impact_levels
        cwp_tile_map(
          plot_data,
          fill = "Impact on the data",
          facet_rows = "measurement_unit",
          facet_cols = parameter_geographical_dimension_groupping,
          fill_scale = ggplot2::scale_fill_manual(values = tile_palette, drop = FALSE,
                                                  na.value = "grey80"),
          continent = continent
        )
      }
    }, error = function(e) {
      warning("Tile map failed (", conditionMessage(e), "), falling back to tmap.")
      NULL
    })
    if (!is.null(image)) {
      return(list(title = title, plott = image))
    }
  }

  breaks <- dplyr::inner_join(shapefile_fix %>% dplyr::select(cwp_code, geom), geographic_dimension, by = c("cwp_code"="Precision")) %>%
    dplyr::ungroup()

  image <- tm_shape(breaks) +
    tmap::tm_polygons(
      fill = "Impact on the data",
      palette = impact_palette,
      border.col = NA,
      lwd        = 0
    ) +
    tmap::tm_layout(legend.outside = TRUE) +
    tmap::tm_facets_grid(rows = "measurement_unit", columns = parameter_geographical_dimension_groupping)
  image <- image+tmap::tm_shape(continent) + tmap::tm_borders()

  return(list(title = title, plott = image))
}
