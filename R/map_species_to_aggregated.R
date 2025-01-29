map_species_to_aggregated <- function(df) {
  # Ensure the input dataframe has the necessary columns
  if (!all(c('species', 'source_authority') %in% colnames(df))) {
    stop("The input dataframe must contain 'species' and 'source_authority' columns.")
  }

  # Join the input dataframe with the lookup table to get the aggregated species data
  df <- df %>%
    left_join(species_lookup, by = c("species", "source_authority"))

  # If there are species that didn't match, fill the aggregated columns with "Unknown"
  df <- df %>%
    mutate(
      species_aggregated_code = ifelse(is.na(species_aggregated_code), species, species_aggregated_code),
      species_aggregated_name = ifelse(is.na(species_aggregated_name), species, species_aggregated_name)
    )

  return(df)
}
