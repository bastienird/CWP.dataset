# Summarising_step

This function performs various summarizing steps on data related to
species and gear types, retrieving data from a database, processing it,
and rendering output reports.

## Usage

``` r
summarising_step(
  main_dir,
  connectionDB,
  config,
  source_authoritylist = c("all", "IOTC", "WCPFC", "IATTC", "ICCAT", "CCSBT"),
  sizepdf = "long",
  savestep = FALSE,
  nameoutput = NULL,
  usesave = FALSE,
  fast_and_heavy = TRUE,
  parameter_colnames_to_keep_fact = NULL,
  render_pdf = FALSE,
  interactive = TRUE
)
```

## Arguments

- main_dir:

  Character. The main directory containing the entities. (jobs/entities)

- connectionDB:

  Object. The database connection.

- config:

  List. Configuration list containing metadata and options for
  processing.

- source_authoritylist:

  Vector. Vector of source_authority to filter on, "all" being all of
  them.

- sizepdf:

  Character string. La taille peut prendre les valeurs suivantes :

  - `"long"` (par défaut) : Long with coverage.

  - `"middle"` : Long without coverage

  - `"short"` : Only first characteristics, first differences and main
    table of steps

- savestep:

  Logical TRUE/FALSE, should the result of this be saved (.rds) ?

- nameoutput:

  Character, name of the output directory of the report

- usesave:

  Logical Should the results saved by a previous run be used instead of
  rerunning everything ?

- fast_and_heavy:

  Logical TRUE/FALSE, if FALSE, each result is saved to its own .rds
  file and read back by the chapter that needs it, which uses less
  memory

- parameter_colnames_to_keep_fact:

  Vector: what column to display

- render_pdf:

  Logical. Should the PDF report be rendered in addition to the HTML
  one? Default `FALSE`: only the HTML report is produced, which needs no
  LaTeX installation. With `TRUE`, the PDF is rendered if `lualatex` is
  available (for instance through TinyTeX); otherwise a warning is
  logged and only the HTML report is produced.

- interactive:

  Logical. Should the HTML report use the interactive version of the
  plots that have one (maps, with the leaflet package)? Default `TRUE`.
  The PDF report always uses the static plots. Without leaflet, the
  static maps are used.

## Value

NULL. The function has side effects, such as writing files and rendering
reports.

## Examples

``` r
if (FALSE) { # \dontrun{
connectionDB <- DBI::dbConnect(RSQLite::SQLite(), ":memory:") # Connexion temporaire
config <- list() # Simule une configuration
summarising_step(main_dir = "chemin/vers/dossier", connectionDB, config)
} # }
```
