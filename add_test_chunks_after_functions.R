add_test_chunks_after_functions <- function(file_path = "flat_additional.Rmd") {
  # Lire le fichier Rmd
  lines <- readLines(file_path, warn = FALSE)

  # Indices des lignes des chunks function-*
  function_indices <- grep("^```\\{r function-", lines)

  # Trouver les noms des fonctions
  function_names <- gsub("^```\\{r function-", "", lines[function_indices])
  function_names <- gsub("\\}.*$", "", function_names)

  # Indices des lignes des chunks test-*
  test_indices <- grep("^```\\{r test-", lines)

  # Trouver les noms des tests existants
  test_names <- gsub("^```\\{r test-", "", lines[test_indices])
  test_names <- gsub("\\}.*$", "", test_names)

  # Identifier les fonctions sans chunk test
  missing_tests <- setdiff(function_names, test_names)

  if (length(missing_tests) == 0) {
    message("✅ Tous les chunks de test existent déjà !")
    return(invisible(NULL))
  }

  # Créer une copie des lignes pour modification
  modified_lines <- lines

  # Décalage pour gérer l'insertion
  offset <- 0

  for (i in seq_along(function_indices)) {
    func_name <- function_names[i]

    # Vérifier si le test existe déjà
    if (func_name %in% test_names) next

    # Trouver où se termine le chunk de la fonction
    chunk_end <- function_indices[i] + 1  # On commence après la ligne d'ouverture
    while (chunk_end <= length(modified_lines) && !grepl("^```$", modified_lines[chunk_end])) {
      chunk_end <- chunk_end + 1
    }

    # Vérifier si on a trouvé la fin du chunk
    if (chunk_end > length(modified_lines)) next

    # Construire le chunk de test
    new_chunk <- paste0("\n```{r test-", func_name, "}\n# Ajouter les tests pour ", func_name, "\n```\n")

    # Calculer la nouvelle position avec l'offset
    insert_position <- chunk_end + offset

    # Insérer dans la liste modifiable après la fin du chunk
    modified_lines <- append(modified_lines, new_chunk, after = insert_position)

    # Mettre à jour l'offset à cause de l'insertion
    offset <- offset + 1
  }

  # Écrire dans le fichier
  writeLines(modified_lines, file_path)

  message("✅ Chunks de test ajoutés après chaque chunk function-* sans couper le code : ", paste(missing_tests, collapse = ", "))
}

# Exécuter la fonction sur le fichier cible
add_test_chunks_after_functions("~/CWP.dataset/dev/flat_first.Rmd")
