#' Compare Nominal and Georeferenced Data
#'
#' This function compares nominal and georeferenced datasets, aggregating values by specified strata,
#' identifying mismatches, and analyzing differences in measurement values.
#'
#' @param nominal A data.table containing nominal data with measurement values.
#' @param georef_mapped A data.table containing georeferenced data with measurement values.
#' @param list_strata A list of character vectors specifying the grouping strata for comparison.
#'                    Default: list(c("species", "year", "source_authority", "gear_type", "fishing_fleet", "geographic_identifier_nom")).
#'
#' @return A list containing multiple comparison results, including:
#' \itemize{
#'   \item `georef_no_nominal`: Strata present in georeferenced data but absent in nominal.
#'   \item `georef_no_nominal_with_value`: Strata missing in nominal with their georeferenced measurement values.
#'   \item `georef_tons_no_nominal`: Strata in tons missing from nominal data.
#'   \item `georef_sup_nominal`: Strata where georeferenced data exceeds nominal data.
#'   \item `tons_nei_nominal`: NEI strata in nominal data that could explain differences.
#'   \item `tons_nei_georef`: NEI strata in georeferenced data that could explain differences.
#'   \item `sum_georef_no_nominal`: Total measurement value of georeferenced data missing in nominal.
#'   \item `suffisant`: Boolean indicating whether georeferenced data is sufficient to match nominal data.
#'   \item `tons_aggregated_georef`: Measurement values of aggregated georeferenced strata.
#'   \item `sum_georef_sup_nom`: Total excess of georeferenced data over nominal data.
#' }
#'
#' @details
#' - Converts both `nominal` and `georef_mapped` to `data.table` format.
#' - Extracts `year` from `time_start` for both datasets.
#' - Filters georeferenced data to retain only measurements in tons.
#' - Aggregates measurement values by the provided strata.
#' - Compares common and distinct strata between the datasets.
#' - Evaluates the impact of `NEI` and aggregated species (`TUN`, `TUS`, `BIL`) on discrepancies.
#'
#' @examples
#' \dontrun{
#'
#' nominal <- data.table(
#'   species = c("YFT", "BET"),
#'   year = c("2020", "2020"),
#'   source_authority = c("RFMO_A", "RFMO_B"),
#'   gear_type = c("03.1.0", "01.1.0"),
#'   fishing_fleet = c("Fleet_1", "Fleet_2"),
#'   geographic_identifier_nom = c("5100000", "5100001"),
#'   measurement_unit = c("t", "t"),
#'   measurement_value = c(100, 200)
#' )
#'
#' georef_mapped <- data.table(
#'   species = c("YFT", "BET"),
#'   year = c("2020", "2020"),
#'   source_authority = c("RFMO_A", "RFMO_B"),
#'   gear_type = c("03.1.0", "01.1.0"),
#'   fishing_fleet = c("Fleet_1", "Fleet_2"),
#'   geographic_identifier_nom = c("5100000", "5100001"),
#'   measurement_unit = c("t", "t"),
#'   measurement_value = c(150, 180),
#'   time_start = as.Date(c("2020-01-01", "2020-01-01"))
#' )
#'
#' result <- compare_nominal_georef_corrected(nominal, georef_mapped)
#' }
#'
#' @import data.table
#' @importFrom lubridate year ymd
#' @importFrom dplyr rename
#' @export
compare_nominal_georef_corrected <- function(nominal, georef_mapped, list_strata = list(c("species", "year", "source_authority", "gear_type", "fishing_fleet", "geographic_identifier_nom"))) {
  # Convertir les data.frames en data.tables
  setDT(nominal)
  setDT(georef_mapped)

  # Créer la colonne "year" à partir de time_start
  georef_mapped[, year := as.character(year(ymd(time_start)))]
  nominal[, year := as.character(year(ymd(time_start)))]

  # Conserver uniquement les données en tonnes
  georef_mapped_tons <- georef_mapped[measurement_unit == "t"]

  # Initialise une liste pour stocker les résultats (un résultat pour chaque liste de dimensions à conserver pour faire la comparaison)
  results <- list()

  for (strata in list_strata) {
    # Nom pour la catégorie actuelle de strata
    name <- paste0(toString(strata))

    # Agréger les données pour le nominal et georef sur les colonnes spécifiées dans 'strata' (ex groupper les données par années, espèces, engins, pavillon)
    nominal_grouped <- nominal[, .(measurement_value_nominal = sum(measurement_value, na.rm = TRUE)), by = strata]
    georef_mapped_grouped <- georef_mapped[, .(measurement_value_georef = sum(measurement_value, na.rm = TRUE)), by = strata]
    georef_mapped_tons_grouped <- georef_mapped_tons[, .(measurement_value_georef_tons = sum(measurement_value, na.rm = TRUE)), by = strata]

    # # Retirer les valeurs des colonnes pour comparer uniquement les strates (si on veut garder que elles)
    nominal_grouped_without_value <- nominal_grouped[, .SD, .SDcols = strata]
    georef_grouped_without_value <- georef_mapped_grouped[, .SD, .SDcols = strata]
    georef_tons_grouped_without_value <- georef_mapped_tons_grouped[, .SD, .SDcols = strata]


    # # Assurer que les colonnes sont dans le même ordre pour la comparaison
    setcolorder(georef_grouped_without_value, names(nominal_grouped_without_value))
    setcolorder(georef_tons_grouped_without_value, names(nominal_grouped_without_value))

    # Trouver les strates présentes dans georef_mapped mais absentes de nominal
    georef_no_nominal <- fsetdiff(georef_grouped_without_value, nominal_grouped_without_value, all = FALSE)
    georef_no_nominal_with_value <- merge(georef_mapped_tons_grouped, georef_no_nominal, by = strata, all = FALSE)
    sum_georef_no_nominal_tons <- sum(georef_no_nominal_with_value$measurement_value_georef_tons ,na.rm = TRUE)


    # Comparer uniquement les données en tonnes
    georef_tons_no_nominal <- fsetdiff(georef_tons_grouped_without_value, nominal_grouped_without_value, all = FALSE)

    # Comparer les valeurs des strates communes entre nominal et georef_mapped pour les données en tonnes
    georef_sup_nominal <- merge(nominal_grouped, georef_mapped_tons_grouped, by = strata, all = FALSE)

    # Vérifier si les colonnes existent après le merge
    if ("measurement_value_georef_tons" %in% names(georef_sup_nominal) &&
        "measurement_value_nominal" %in% names(georef_sup_nominal)) {
      georef_sup_nominal[, Difference := measurement_value_georef_tons - measurement_value_nominal]
      georef_sup_nominal <- georef_sup_nominal[round(Difference, 3) > 1] # Supérieur strictement à 1, on s'affranchit des petits kouaks
    } else {
      georef_sup_nominal <- data.table()  # Retourne une table vide s'il n'y a pas de données
    }

    if ("fishing_fleet" %in% colnames(georef_sup_nominal)){
      tons_nei_georef <- georef_no_nominal_with_value[
        fishing_fleet == "NEI" ,
        sum(measurement_value_georef_tons)] + georef_sup_nominal[
          fishing_fleet == "NEI" ,
          sum(measurement_value_georef_tons)
        ]} else {
          tons_nei_georef <- 0
        }

    tons_aggregated_georef <- georef_no_nominal_with_value[
      species %in% c("TUN", "TUS" ,"BIL"),
      sum(measurement_value_georef_tons)
    ] + georef_sup_nominal[
      species %in% c("TUN", "TUS" ,"BIL"),
      sum(measurement_value_georef_tons)
    ]

    if ("fishing_fleet" %in% colnames(nominal_grouped)){
      tons_nei_nominal <- nominal_grouped[
        fishing_fleet == "NEI",
        sum(measurement_value_nominal)
      ]} else {tons_nei_nominal <- 0}


    sum_georef_sup_nom <- sum(georef_sup_nominal$Difference, na.rm = TRUE)

    suffisant <- ifelse(sum_georef_no_nominal_tons + sum_georef_sup_nom -(tons_aggregated_georef + tons_nei_georef) > 0, FALSE, TRUE)
    # Stocker les résultats
    results[[name]] <- list(
      georef_no_nominal = georef_no_nominal,           # Strates dans georef mais absentes dans nominal
      georef_no_nominal_with_value = georef_no_nominal_with_value %>% dplyr::rename(measurement_value = measurement_value_georef_tons),           # Strates dans georef mais absentes dans nominal avec la valeur totale
      georef_tons_no_nominal = georef_tons_no_nominal, # Strates en tonnes absentes dans nominal
      georef_sup_nominal = georef_sup_nominal,          # Strates où georef est supérieur à nominal
      tons_nei_nominal = tons_nei_nominal,          # Strates nei qui pourraient expliquer les différences
      tons_nei_georef = tons_nei_georef,          # Strates nei qui pourraient expliquer les différences
      sum_georef_no_nominal = sum_georef_no_nominal_tons,
      suffisant = suffisant,
      tons_aggregated_georef = tons_aggregated_georef,
      sum_georef_sup_nom = sum_georef_sup_nom
    )
  }

  return(results)
}
