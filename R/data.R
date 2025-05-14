#' download_codelists
#'
#' Liste des codelists FDIWG, chaque élément est un data frame contenant au moins
#' les colonnes `code` et `label` :
#'
#' \describe{
#'   \item{cl_asfis_species}{Codelist des espèces selon la classification ASFIS}
#'   \item{cl_measurement_processing_level}{Niveaux de traitement des mesures}
#'   \item{cl_measurement}{Types de mesures FDI}
#'   \item{cl_fishing_mode}{Modes de pêche (GTA FIRMS)}
#'   \item{cl_isscfg_pilot_gear}{Engins pilotes ISSCFG (GTA FIRMS)}
#'   \item{cl_fishingfleet_firms}{Flottes de pêche (GTA FIRMS)}
#'   \item{cl_catch_concepts}{Concepts relatifs aux captures (CWP FDIWG)}
#'   \item{cl_measurement_types_effort}{Types de mesures d’effort FDI}
#'   \item{cl_areal_grid}{Quadrillage spatial (CWP FDIWG)}
#' }
#'
"download_codelists"
