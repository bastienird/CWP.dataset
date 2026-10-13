#' Serve the explorer for the reports of a folder
#'
#' Starts the explorer as a service, so that the links of the HTML reports
#' open it directly on their data. A link gives the steps to explore and the
#' filters to select in its address, for example
#' `http://127.0.0.1:8765/?steps=entities/my_dataset/Markdown/rawdata,entities/my_dataset/Markdown/final&source_authority=IOTC`.
#'
#' Run it on your computer (the default address is the one the reports link
#' to), or on a server (RStudio Server, a Docker container...) with access to
#' the folder of the jobs. In that case, render the reports with
#' `options(CWP.dataset.explorer_url = "<address of the service>",
#' CWP.dataset.explorer_root = "<folder served>")` so that their links point
#' to it.
#'
#' @param root Folder served: only the steps inside it can be explored. The
#'   paths in the links are relative to it. For the reports of
#'   `summarising_step()`, the folder of the job (its `main_dir`).
#' @param port,host Where the service listens. The default port is the one of
#'   the links, `options(CWP.dataset.explorer_port = )` to change it. Use
#'   `host = "0.0.0.0"` on a server.
#' @param launch.browser Open the start page in a browser.
#' @return Does not return while the service runs.
#' @export
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   cwp_explorer_service("path/to/the/job")
#' }
cwp_explorer_service <- function(root = ".", port = cwp_explorer_port(), host = "127.0.0.1",
                                 launch.browser = FALSE) {
  cwp_require_package("shiny", "to run the explorer")
  app <- cwp_explorer_service_app(root)
  message("Explorer running at http://", host, ":", port, " for the reports of ",
          normalizePath(root), ". Press Escape or Ctrl+C to stop it.")
  shiny::runApp(app, port = port, host = host, launch.browser = launch.browser)
}

#' Port of the explorer service
#' @keywords internal
#' @noRd
cwp_explorer_port <- function() {
  as.integer(getOption("CWP.dataset.explorer_port", 8765L))
}

#' Address the links of the reports point to
#' @keywords internal
#' @noRd
cwp_explorer_base_url <- function() {
  url <- getOption("CWP.dataset.explorer_url", NULL)
  if (is.null(url)) url <- paste0("http://127.0.0.1:", cwp_explorer_port())
  sub("/+$", "", url)
}

#' Read the request of a link: steps and filters
#'
#' @param query The parameters of the address (see [shiny::parseQueryString()]):
#'   `steps`, the step directories relative to `root` separated by commas, and
#'   for any column, the values to select separated by commas.
#' @param root Folder served (normalized, with forward slashes).
#' @return A list with `data` (output of `cwp_explore_datasets()`), `filters`
#'   and `selected`. An error with an explicit message when the steps are
#'   missing or outside `root`.
#' @keywords internal
#' @noRd
cwp_explorer_request <- function(query, root) {
  steps <- if (is.null(query$steps)) character() else strsplit(query$steps, ",", fixed = TRUE)[[1]]
  steps <- trimws(steps[nzchar(trimws(steps))])
  if (length(steps) == 0) {
    stop("No step to explore: the address must end with ?steps=<step directory>,<step directory>")
  }
  paths <- normalizePath(file.path(root, steps), winslash = "/", mustWork = FALSE)
  inside <- startsWith(paths, paste0(sub("/$", "", root), "/")) & dir.exists(paths)
  if (!all(inside)) {
    stop("Step not found in ", root, ": ", paste(steps[!inside], collapse = ", "))
  }

  data <- cwp_explore_datasets(stats::setNames(as.list(paths), basename(paths)))
  selected <- query[setdiff(names(query), "steps")]
  selected <- lapply(selected, function(values) strsplit(values, ",", fixed = TRUE)[[1]])
  selected <- selected[intersect(names(selected), names(data))]
  filters <- intersect(union(cwp_explore_default_filters, names(selected)), names(data))
  list(data = data, filters = filters, selected = selected)
}

#' The explorer service as a Shiny application
#'
#' @param root Folder served.
#' @return A Shiny application.
#' @keywords internal
#' @noRd
cwp_explorer_service_app <- function(root) {
  parts <- cwp_explorer_service_parts(root)
  shiny::shinyApp(parts$ui, parts$server)
}

#' User interface and server of the explorer service
#'
#' Both read the same request: it is computed once and kept for the few last
#' addresses asked for.
#'
#' @param root Folder served.
#' @return A list with the `ui` function (of the request) and the `server`.
#' @keywords internal
#' @noRd
cwp_explorer_service_parts <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  cache <- new.env(parent = emptyenv())
  cache$keys <- character()

  request_for <- function(search) {
    query <- sub("^\\?", "", if (is.null(search)) "" else search)
    key <- paste0("query:", query)
    if (!key %in% cache$keys) {
      cache[[key]] <- tryCatch(cwp_explorer_request(shiny::parseQueryString(query), root),
                               error = function(e) conditionMessage(e))
      cache$keys <- c(cache$keys, key)
      # Keep the datasets of the last 3 addresses only
      for (old in utils::head(cache$keys, -3)) rm(list = old, envir = cache)
      cache$keys <- utils::tail(cache$keys, 3)
    }
    cache[[key]]
  }

  ui <- function(req) {
    search <- if (is.null(req$QUERY_STRING)) "" else req$QUERY_STRING
    if (!nzchar(sub("^\\?", "", search))) {
      return(cwp_explorer_message_page(paste0(
        "The explorer is running for the reports of ", root,
        ". Open it from the link at the top of a report."
      )))
    }
    request <- request_for(search)
    if (is.character(request)) return(cwp_explorer_message_page(request))
    cwp_explore_ui(request$data, request$filters, request$selected)
  }

  server <- function(input, output, session) {
    request <- request_for(shiny::isolate(session$clientData$url_search))
    if (is.character(request)) return(invisible())
    cwp_explore_server(request$data, request$filters)(input, output, session)
  }

  list(ui = ui, server = server)
}

#' Page of the service showing a message
#' @keywords internal
#' @noRd
cwp_explorer_message_page <- function(text) {
  shiny::fluidPage(
    shiny::titlePanel("CWP dataset explorer", windowTitle = "CWP explorer"),
    shiny::p(text)
  )
}

#' Address opening the explorer on steps of a job
#'
#' @param step_dirs Directories of the steps to explore.
#' @param root Folder served by the service; the steps must be inside it.
#' @param selected Values to select in the filters (see `cwp_explore()`).
#'   `NULL` elements and the value `"all"` are ignored.
#' @param base_url Address of the service.
#' @return The address, a character string.
#' @keywords internal
#' @noRd
cwp_explorer_link <- function(step_dirs, root, selected = list(), base_url = cwp_explorer_base_url()) {
  # Symbolic links resolved on both sides (/var and /private/var on macOS)
  relative <- as.character(fs::path_rel(normalizePath(step_dirs, winslash = "/", mustWork = FALSE),
                                        start = normalizePath(root, winslash = "/", mustWork = FALSE)))
  if (any(startsWith(relative, ".."))) stop("The steps are not inside the folder served: ", root)
  selected <- Filter(function(x) length(x) > 0 && !identical(as.character(x), "all"), selected)
  parameters <- c(steps = paste(relative, collapse = ","),
                  vapply(selected, function(x) paste(as.character(x), collapse = ","), character(1)))
  query <- paste0(names(parameters), "=",
                  vapply(parameters, utils::URLencode, character(1), reserved = TRUE),
                  collapse = "&")
  paste0(base_url, "/?", query)
}

#' Box of the HTML report linking to the explorer
#'
#' Clicking the link checks that the explorer answers before opening it; when
#' it does not, the box explains how to start it.
#'
#' @param info List with `url` (output of `cwp_explorer_link()`), `base_url`,
#'   `root` and `script` (name of the explorer script, or `NULL`).
#' @return A character string: a raw HTML block for pandoc.
#' @keywords internal
#' @noRd
cwp_explorer_link_html <- function(info) {
  escape <- function(x) gsub("\"", "&quot;", cwp_html_escape(x), fixed = TRUE)
  script_help <- if (is.null(info$script)) "" else paste0(
    " Or, without the service: open <code>", escape(info$script), "</code> (in the folder of ",
    "this dataset, next to <code>Markdown/</code>) in RStudio and click <em>Source</em>."
  )
  paste0(
    "\n```{=html}\n",
    "<div class=\"cwp-explorer\" style=\"border:1px solid #9ec5e5;background:#eef6fc;",
    "border-radius:6px;padding:10px 14px;margin:1em 0;\">",
    "<strong>Explore these data in detail:</strong> ",
    "<a href=\"", escape(info$url), "\" data-base=\"", escape(info$base_url), "\" target=\"_blank\" ",
    "rel=\"noopener\" onclick=\"return cwpOpenExplorer(this);\">open the interactive explorer</a> ",
    "(filters on tRFMO, species, fishing fleet, gear and years; time series, tables, maps and ",
    "differences between the first and the last step).",
    "<div class=\"cwp-explorer-help\" style=\"display:none;margin-top:8px;\">",
    "The explorer does not answer at <code>", escape(info$base_url), "</code>. ",
    "Start it in R, keep R open, then click the link again:",
    "<pre><code>CWP.dataset::cwp_explorer_service(\"", escape(info$root), "\")</code></pre>",
    "It needs the shiny and DT packages: <code>install.packages(c(\"shiny\", \"DT\"))</code>.",
    script_help,
    "</div></div>\n",
    "<script>\n",
    "if (!window.cwpOpenExplorer) {\n",
    "  window.cwpOpenExplorer = function (link) {\n",
    "    var help = link.parentNode.querySelector('.cwp-explorer-help');\n",
    "    var target = window.open('about:blank', '_blank');\n",
    "    fetch(link.getAttribute('data-base'), {mode: 'no-cors', cache: 'no-store'})\n",
    "      .then(function () {\n",
    "        help.style.display = 'none';\n",
    "        if (target) { target.location = link.href; } else { window.location = link.href; }\n",
    "      })\n",
    "      .catch(function () {\n",
    "        if (target) { target.close(); }\n",
    "        help.style.display = 'block';\n",
    "      });\n",
    "    return false;\n",
    "  };\n",
    "}\n",
    "</script>\n",
    "```\n"
  )
}
