#' Tidying Data by Keeping Specific Columns
#'
#' @description This function tidies a data frame by selecting specified columns and converting time columns to character.
#' @param dataframe A data frame to tidy.
#' @param parameter_colnames_to_keep_dataframe A character vector of column names to keep.
#' @param time_dimension A character vector of time dimension column names.
#' @return A tidied data frame.
#' @export
tidying_data <- function(dataframe, parameter_colnames_to_keep_dataframe, time_dimension){
  dataframe <- dataframe %>% ungroup()
  dataframe <- dataframe %>% dplyr::select(any_of(parameter_colnames_to_keep_dataframe))
  dataframe <- dataframe %>% mutate_at(all_of(time_dimension), as.character)
  return(dataframe)

}
