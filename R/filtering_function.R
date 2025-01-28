#' Filter Data Frame Based on Provided Parameters
#'
#' @description This function filters a data frame based on specified filtering parameters.
#' @param dataframe_to_filter A data frame to be filtered.
#' @param parameter_filtering A list of parameters to use for filtering.
#' @return A filtered data frame.
#' @export
filtering_function <- function(dataframe_to_filter, parameter_filtering) {

  matchingList <- parameter_filtering %>% purrr::keep( ~ !is.null(.) )
  if(length(matchingList)!= 0){

    colnames_to_filter <- colnames(dataframe_to_filter %>% dplyr::select(names(matchingList)))

    names(matchingList) <- colnames_to_filter

    matchingList <- lapply(matchingList, function(x){ #handling single filter
      if(length(x) == 1){
        x <- c(x, x) }else {
          x
        }

    }
    )
    dataframe_to_filter <- dataframe_to_filter%>% dplyr::filter(!! rlang::parse_expr(str_c(colnames_to_filter, matchingList, sep = '%in%', collapse="&")))}
  return(dataframe_to_filter)
}
