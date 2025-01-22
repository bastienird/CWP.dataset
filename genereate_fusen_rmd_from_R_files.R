genereate_fusen_rmd_from_R_files <- function(input, output_file) {
  # Vérifier si l'input est un répertoire
  if (dir.exists(input)) {
    # Lister tous les fichiers .R dans le répertoire
    r_files <- list.files(input, pattern = "\\.R$", full.names = TRUE)
  } else if (is.vector(input) && all(file.exists(input))) {
    # Si l'input est une liste de fichiers
    r_files <- input
  } else {
    stop("L'input doit être un répertoire ou une liste de fichiers existants.")
  }

  # Ouverture du fichier Rmd pour écriture
  file_conn <- file(output_file, open = "w")

  # Parcourir chaque fichier R de la liste
  for (r_file in r_files) {
    # Extraire le nom de la fonction sans l'extension
    function_name <- tools::file_path_sans_ext(basename(r_file))

    # Lire le contenu du fichier R
    function_content <- readLines(r_file)

    # Écrire le chunk pour la fonction
    writeLines(c(
      paste0("```{r function-", function_name, "}"),
      function_content,
      "```",
      ""
    ), file_conn)

    # Écrire le chunk vide pour les tests
    writeLines(c(
      paste0("```{r test-", function_name, "}"),
      "# Ajouter les tests ici",
      "```",
      ""
    ), file_conn)
  }

  # Fermer le fichier
  close(file_conn)

  message("Fichier Rmd généré avec succès : ", output_file)
}

# Exemple d'utilisation avec une liste de fichiers
r_files <- c(
  "bar_plot_default.R",
  "comparison-between-multiple-cwp-datasets.R",
  "compute_summary_of_differences.R",
  "fonction_groupement.R",
  "function_multiple_comparison.R",
  "isnulllist.R",
  "knitting_plots_subfigures.R",
  "my_fun.R",
  "read_data.R"
)

# Appeler la fonction pour générer un fichier Rmd avec une liste
generate_rmd_from_r_files(r_files, "output.Rmd")

# Exemple d'utilisation avec un répertoire
# generate_rmd_from_r_files("path/to/your/directory", "output.Rmd")
