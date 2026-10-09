# download_codelists

Liste des codelists FDIWG, chaque élément est un data frame contenant au
moins les colonnes `code` et `label` :

## Usage

``` r
download_codelists
```

## Format

An object of class `list` of length 9.

## Details

- cl_asfis_species:

  Codelist des espèces selon la classification ASFIS

- cl_measurement_processing_level:

  Niveaux de traitement des mesures

- cl_measurement:

  Types de mesures FDI

- cl_fishing_mode:

  Modes de pêche (GTA FIRMS)

- cl_isscfg_pilot_gear:

  Engins pilotes ISSCFG (GTA FIRMS)

- cl_fishingfleet_firms:

  Flottes de pêche (GTA FIRMS)

- cl_catch_concepts:

  Concepts relatifs aux captures (CWP FDIWG)

- cl_measurement_types_effort:

  Types de mesures d’effort FDI

- cl_areal_grid:

  Quadrillage spatial (CWP FDIWG)
