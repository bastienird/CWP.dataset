#' Tidy GTA Data for Comparison
#'
#' This function processes a dataset by joining it with spatial and categorical data,
#' standardizing measurement units, and preparing it for analysis.
#'
#' @param dataframe A data frame or a file path (character) to an RDS file containing the dataset.
#' @param shape An optional spatial data frame with a `gridtype` and `cwp_code` column for geographic matching.
#' @param species_group_dataframe An optional data frame mapping species to groups.
#' @param cl_cwp_gear_level2_dataframe An optional data frame mapping gear types to level 2 categories.
#'
#' @return A cleaned and processed data frame with standardized geographic identifiers, measurement units,
#'         and additional categorical information from external data frames.
#'
#' @details
#' - If `dataframe` is a file path, it is loaded using `readRDS()`.
#' - If `shape` is provided and `geographic_identifier` exists in `dataframe`, a spatial join is performed.
#' - If `species_group_dataframe` is provided and `species` exists in `dataframe`, a species join is performed.
#' - If `cl_cwp_gear_level2_dataframe` is provided and `gear_type` exists in `dataframe`, a gear-type join is performed.
#' - Measurement units are standardized to "Tons" or "Number of fish".
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' df <- data.frame(
#'   geographic_identifier = c("5100000", "5100001"),
#'   species = c("YFT", "BET"),
#'   gear_type = c("03.1.0", "01.1.0"),
#'   measurement_unit = c("MT", "NO"),
#'   measurement_value = c(100, 200)
#' )
#'
#' shape_data <- data.frame(gridtype = c("1deg_x_1deg", "5deg_x_5deg"), cwp_code = c("5100000", "5100001"))
#' species_mapping <- data.frame(species = c("YFT", "BET"), species_group = c("Yellowfin Tuna", "Bigeye Tuna"))
#' gear_mapping <- data.frame(Code = c("03.1.0", "01.1.0"), GearLevel2 = c("Purse Seine", "Longline"))
#'
#' # Run function
#' cleaned_df <- tidying_GTA_data_for_comparison(df, shape_data, species_mapping, gear_mapping)
#' }
#'
#' @import dplyr
#' @export
tidying_GTA_data_for_comparison <- function(dataframe, shape = NULL,
                                            species_group_dataframe = NULL,
                                            cl_cwp_gear_level2_dataframe = NULL) {
  if(is_string(dataframe)){
    dataframe <- readRDS(dataframe)
  }

  if("geographic_identifier"%in%colnames(dataframe) & !is.null(shape)){
    dataframe <- dataframe%>%  dplyr::left_join(shape%>%
                                                  dplyr::select(gridtype, cwp_code), by = c("geographic_identifier"="cwp_code"))
    print_map <- TRUE
  } else {print_map <- FALSE}

  if("gridtype"%in%colnames(dataframe)){
    dataframe <- dataframe%>%dplyr::mutate(gridtype = as.character(gridtype))
  }
  if(!is.null(species_group_dataframe) && ("species" %in% colnames(dataframe))){
    dataframe <- dataframe %>% dplyr::left_join(species_group_dataframe%>% dplyr::distinct(), by = c("species"))
  }
  if("gear_type" %in%colnames(dataframe) & !is.null(cl_cwp_gear_level2_dataframe) ){
    dataframe <- dataframe %>% dplyr::left_join(cl_cwp_gear_level2_dataframe, by = c("gear_type" = "Code"))
  }

  dataframe <- dataframe%>%dplyr::mutate(measurement_unit = dplyr::case_when(measurement_unit %in% c("MT","t","MTNO", "Tons")~ "Tons",
  measurement_unit %in% c("NO", "NOMT","no", "Number of fish")~"Number of fish", TRUE ~ measurement_unit))

  # dataframe <- dataframe %>% dplyr::mutate(measurement_value = sum(measurement_value))

  return(dataframe = dataframe)
}
