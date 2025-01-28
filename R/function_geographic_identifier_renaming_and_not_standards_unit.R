#' Rename Geographic Identifiers and Handle Standard Units
#'
#' @description This function renames geographic identifiers in a data frame and manages non-standard units.
#' @param dataframe_to_filter A data frame to filter and rename columns.
#' @param geo_dim The geographic dimension to rename.
#' @param parameter_fact A parameter indicating the measurement context.
#' @param parameter_UNK_for_not_standards_unit Logical indicating if non-standard units should be labeled as 'UNK'.
#' @param geo_dim_group The grouping geographic dimension to rename.
#' @return A modified data frame with renamed columns.
#' @export
function_geographic_identifier_renaming_and_not_standards_unit <- function(dataframe_to_filter, geo_dim , parameter_fact, parameter_UNK_for_not_standards_unit = TRUE, geo_dim_group){
  if(  parameter_UNK_for_not_standards_unit & parameter_fact == "effort"){
    dataframe_to_filter <- dataframe_to_filter %>% dplyr::mutate(measurement_unit = ifelse(measurement_unit%in%c("HOOKS","FDAYS"), measurement_unit, "UNK" ))}

  if(geo_dim != "geographic_identifier" && "geographic_identifier"%notin%colnames(dataframe_to_filter)){
    dataframe_to_filter <- dataframe_to_filter %>% dplyr::rename("geographic_identifier" := {{geo_dim}})
  }
  if(geo_dim_group != "GRIDTYPE" && "GRIDTYPE"%notin%colnames(dataframe_to_filter)){
    dataframe_to_filter <- dataframe_to_filter %>% dplyr::rename("GRIDTYPE" := {{geo_dim_group}})
  }
  dataframe_to_filter
}
