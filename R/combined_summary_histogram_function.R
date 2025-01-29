combined_summary_histogram_function <- function(init, parameter_titre_dataset_1 = "Init",
                                                final, parameter_titre_dataset_2 = "Final") {
  # Convertir en data.table
  setDT(init)
  summary_number_row_init <- init[, .(Number_different_stratas = .N), by = measurement_unit]
  summary_number_row_init[, data_source := parameter_titre_dataset_1]

  setDT(final)
  summary_number_row_final <- final[, .(Number_different_stratas = .N), by = measurement_unit]
  summary_number_row_final[, data_source := parameter_titre_dataset_2]

  # Combiner les résumés
  combined_summary <- rbind(summary_number_row_init, summary_number_row_final)
  combined_summary[, Percent := Number_different_stratas / sum(Number_different_stratas) * 100, by = data_source]

  # Calcul des totaux pour chaque dataset
  total_rows <- combined_summary[, .(Total_rows = sum(Number_different_stratas)), by = data_source]
  combined_summary <- merge(combined_summary, total_rows, by = "data_source")

  # Créer le graphique principal (histogramme)
  combined_summary_histogram <- ggplot(combined_summary,
                                       aes(x = factor(data_source),
                                           y = Percent, fill = measurement_unit)) +
    geom_bar(stat = "identity", position = "fill") +  # Barres empilées avec échelle à 100%
    scale_fill_manual(values = c("Tons" = "#4C72B0", "Number of fish" = "#55A868")) +  # Couleurs personnalisées
    scale_y_continuous(labels = scales::percent_format()) +
    geom_text(aes(label = paste0(round(Percent, 1), "%")),
              position = position_fill(vjust = 0.5), color = "black") + # Texte à l'intérieur des barres
    labs(title = "Distribution of number strata for each measurement_unit by dataset",
         x = "Dataset",
         y = "Percentage",
         fill = "Measurement unit") +
    theme_minimal()

  # # Créer un tableau comme ggplot avec ggtexttable
  # combined_table <- rbind(summary_number_row_init, summary_number_row_final)[, .(
  #   `Measurement Unit` = measurement_unit,
  #   `Dataset` = data_source,
  #   `Number of Strata` = Number_different_stratas
  # )]
  #
  # table_plot <- ggtexttable(
  #   combined_table,
  #   rows = NULL,
  #   theme = ttheme("minimal", base_size = 10)
  # )
  #
  # # Utiliser patchwork pour combiner le graphique et le tableau
  # library(patchwork)
  # final_plot <- combined_summary_histogram / table_plot + plot_layout(heights = c(3, 1))

  return(combined_summary_histogram)
}

