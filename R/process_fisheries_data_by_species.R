process_fisheries_data_by_species <- function(sub_list_dir_2, parameter_fact, species_list, aggregatespecies = FALSE ) {

  # Liste pour stocker les résultats par espèce
  result_list <- list()

  # Charger les datasets une seule fois
  nominal_dataset <- if (dir.exists("Markdown")) {
    if(file.exists("data/global_nominal_catch_firms_level0.csv")){
      readr::read_csv("data/global_nominal_catch_firms_level0.csv")} else if(file.exists("data/global_nominal_catch_firms_level0.csv")){
        readr::read_csv("data/global_nominal_catch_firms_level0.csv")}
  } else {
    NULL
  }
  if(aggregatespecies){
    nominal_dataset <- map_species_to_aggregated(nominal_dataset)

  }

  # Boucler sur chaque espèce
  for (species_code in species_list) {

    parameter_filtering <- list(species = species_code)  # Mise à jour du filtre avec l'espèce

    # Initialiser un dataframe vide pour chaque espèce
    df <- NULL
    # Charger et filtrer les premières données pour l'espèce courante

    main <- qs::qread(paste0(sub_list_dir_2[1], "/data.qs"))

    if(aggregatespecies){
      main <- map_species_to_aggregated(main)

    }


    main <- filtering_function(main, parameter_filtering = parameter_filtering) %>%
      # dplyr::filter(source_authority == "IOTC") %>%
      dplyr::ungroup()
    tons_init <- sum((main %>% dplyr::filter(measurement_unit %in% c("MTNO", "MT", "t", "Tons")))$measurement_value)
    nofish_init <- sum((main %>% dplyr::filter(measurement_unit %in% c("NOMT", "NO", "no", "Number of fish")))$measurement_value)
    lines_init <- nrow(main)

    nominal_dataset_filtered <- filtering_function(nominal_dataset, parameter_filtering = parameter_filtering) %>%
      # dplyr::filter(source_authority == "IOTC") %>%
      dplyr::ungroup()

    nominal_total <- sum(nominal_dataset_filtered$measurement_value)

    for (i in sub_list_dir_2) {
      # Charger et filtrer les données pour l'itération courante
      main <- qs::qread(paste0(i, "/data.qs"))

      if(aggregatespecies){
        main <- map_species_to_aggregated(main)

      }

      main <- filtering_function(main, parameter_filtering = parameter_filtering)%>%
        # dplyr::filter(source_authority == "IOTC") %>%
        dplyr::ungroup()
      sum_t <- sum((main %>% dplyr::filter(measurement_unit %in% c("MTNO", "MT", "t", "Tons")))$measurement_value)
      sum_no <- sum((main %>% dplyr::filter(measurement_unit %in% c("NOMT", "NO", "no", "Number of fish")))$measurement_value)
      nrow <- nrow(main)

      georef_sup_nom_init <- compare_nominal_georef_corrected(nominal_dataset_filtered, main,
                                                              list(c("species", "year", "source_authority", "gear_type", "fishing_fleet")))$`species, year, source_authority, gear_type, fishing_fleet`$georef_sup_nominal %>%
        dplyr::distinct()

      georef_sup_nom_year_species <- compare_nominal_georef_corrected(nominal_dataset_filtered, main,
                                                                      list(c("species", "year", "source_authority")))$`species, year, source_authority`$georef_sup_nominal %>%
        dplyr::distinct()

      number_strata_source_auth_year_species <- nrow(georef_sup_nom_year_species)
      number_strata_source_auth_all <- nrow(georef_sup_nom_init)
      sum_diff_strata_source_auth_year_species <- sum(georef_sup_nom_year_species$Difference)
      sum_diff_strata_source_auth_all <- sum(georef_sup_nom_init$Difference)

      # Calcul des différences et des pourcentages
      Difference_percent <- round(-100 * ((tons_init - sum_t) / tons_init), 1)
      Difference_tons <- -(tons_init - sum_t)
      Difference_no <- -(nofish_init - sum_no)
      Difference_percent_lines <- round(-100 * ((lines_init - nrow) / lines_init), 1)
      Difference_percent_no <- round(-100 * ((nofish_init - sum_no) / nofish_init), 1)
      percentage_of_nominal <- round((sum_t * 100) / nominal_total, 1)

      # Calcul du facteur de conversion (kg)
      Conversion_factors_kg <- ifelse(Difference_no != 0 && Difference_tons != 0, (abs(Difference_tons) / abs(Difference_no)) * 1000, NA)

      # Ajouter les résultats dans un dataframe temporaire
      data_i <- data.frame(
        Step = tail(str_split(paste0(i), "/")[[1]], n = 1),
        Millions_of_tons = round(sum_t / 1000000, 3),
        Millions_of_fish = round(sum_no / 1000000, 3),
        Lines = nrow,
        Difference_in_percent_of_tons = Difference_percent,
        Difference_in_tons = Difference_tons,
        Difference_in_percent_of_fish = Difference_percent_no,
        Difference_in_number_of_fish = Difference_no,
        Difference_in_percent_of_lines = Difference_percent_lines,
        Percentage_of_nominal = percentage_of_nominal,
        Conversion_factors_kg = Conversion_factors_kg,
        Number_strata_sup_nom_all = number_strata_source_auth_all,
        Number_strata_sup_year_species = number_strata_source_auth_year_species,
        Summ_georef_sup_nom_year_species_auth = sum_diff_strata_source_auth_year_species,
        Summ_georef_sup_nom_all = sum_diff_strata_source_auth_all
      )

      # Ajouter à df pour cette espèce
      df <- rbind(df, data_i)

      # Mettre à jour les valeurs initiales pour l'itération suivante
      tons_init <- sum_t
      nofish_init <- sum_no
      lines_init <- nrow
    }

    # Nettoyer et formater les résultats
    df <- df %>%
      mutate(Step_number = row_number())

    # Stocker le résultat dans la liste par espèce
    result_list[[species_code]] <- df
  }

  # Retourner la liste des résultats pour chaque espèce
  return(result_list)
}

# IRD_data <- readr::read_csv("data/fact_conv_IRD.csv")
# specieslist <- unique(IRD_data$species)
# rm(IRD_data)
#
# setwd("~/firms-gta/geoflow-tunaatlas/jobs/20241002142921_global_datasets_level1_2/entities/global_catch_ird_level1")
# sub_list_dir_2 <- list.files("Markdown", recursive = TRUE, pattern = "data.qs", full.names = TRUE)
# details <- file.info(sub_list_dir_2)
# details <- details[with(details, order(as.POSIXct(mtime))), ]
# sub_list_dir_2 <- rownames(details)
#
# sub_list_dir_3 <- gsub("/data.qs", "", sub_list_dir_2)
# a <- process_fisheries_data_by_species(sub_list_dir_2 = sub_list_dir_3, parameter_fact = "catch", species_list = specieslist)
#
# create_combined_flextable <- function(a) {
#   # 1. Combiner tous les tableaux en un seul avec une colonne pour l'espèce
#   combined_df <- bind_rows(
#     lapply(names(a), function(species_code) {
#       df <- a[[species_code]]
#       df$Species <- species_code
#       return(df)
#     })
#   )
#
#   # 2. Ajouter une colonne indiquant si l'espèce a au moins un pourcentage supérieur à 100
#   combined_df <- combined_df %>%
#     group_by(Species) %>%
#     mutate(Above_100 = if_else(any(Percentage_of_nominal > 100), "Yes", "No")) %>%
#     ungroup()
#
#   # 3. Créer une flextable à partir de ce dataframe
#   flex_table <- flextable(combined_df) %>%
#     autofit() %>%
#     bold(j = "Above_100", bold = TRUE) %>%  # Mettre en gras la colonne "Above_100"
#     theme_vanilla()  # Utiliser un thème plus esthétique
#
#   return(flex_table)
# }
# #
# # flex_table <- create_combined_flextable(a)
# #
# create_combined_dataframe <- function(a) {
#   # 1. Combiner tous les tableaux en un seul avec une colonne pour l'espèce
#   combined_df <- bind_rows(
#     lapply(names(a), function(species_code) {
#       df <- a[[species_code]]
#       df$Species <- species_code
#       return(df)
#     })
#   )
#
#   # 2. Ajouter une colonne indiquant si l'espèce a au moins un pourcentage supérieur à 100
#   combined_df <- combined_df %>%
#     group_by(Species) %>%
#     mutate(Above_100 = if_else(any(Percentage_of_nominal > 100), "Yes", "No")) %>%
#     ungroup()
#
#   # 3. Trier les données pour mettre les espèces avec "Yes" en premier
#   combined_df <- combined_df %>%
#     arrange(desc(Above_100), Species, Step_number)
#
#   return(combined_df)
# }
# #
# # # Exemple d'utilisation avec le résultat 'a'
# # combined_df <- create_combined_dataframe(a)
# #
# # # Afficher le dataframe
# # View(combined_df)

