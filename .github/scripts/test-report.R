# Run the tests of the package and write a Markdown report: on the summary page
# of the GitHub Actions run when there is one, on the console otherwise. The
# script exits with an error when a test fails, so the check turns red.
#
# It can also be run locally from the package root:
#   Rscript .github/scripts/test-report.R

results <- as.data.frame(testthat::test_local(".", reporter = "summary", stop_on_failure = FALSE))

n_failed  <- sum(results$failed)
n_errors  <- sum(results$error)
n_skipped <- sum(results$skipped)
n_warning <- sum(results$warning)
n_passed  <- sum(results$passed)
ok <- n_failed + n_errors == 0

md_cell <- function(x) gsub("|", "\\|", gsub("\n", " ", as.character(x)), fixed = TRUE)
md_table <- function(df) {
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  rule <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows <- apply(df, 1, function(row) paste0("| ", paste(md_cell(row), collapse = " | "), " |"))
  c(header, rule, rows)
}

lines <- c(
  paste0("## ", if (ok) "Tests passed" else "Tests failed"),
  "",
  md_table(data.frame(
    Passed = n_passed, Failed = n_failed, Errors = n_errors,
    Warnings = n_warning, Skipped = n_skipped,
    `Duration (s)` = round(sum(results$real), 1),
    check.names = FALSE
  )),
  ""
)

problems <- results[results$failed > 0 | results$error, , drop = FALSE]
if (nrow(problems) > 0) {
  lines <- c(lines, "### Failing tests", "",
             md_table(data.frame(File = problems$file, Test = problems$test,
                                 Failed = problems$failed, Error = problems$error)), "")
}

skipped <- results[results$skipped, , drop = FALSE]
if (nrow(skipped) > 0) {
  lines <- c(lines, "### Skipped tests", "",
             md_table(data.frame(File = skipped$file, Test = skipped$test)), "")
}

by_file <- stats::aggregate(
  cbind(passed, failed, warning, real) ~ file, data = results, FUN = sum
)
by_file <- by_file[order(-by_file$real), ]
by_file$real <- round(by_file$real, 1)
names(by_file) <- c("File", "Passed", "Failed", "Warnings", "Duration (s)")
lines <- c(lines, "<details><summary>Results by file, slowest first</summary>", "",
           md_table(by_file), "", "</details>", "")

summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  cat(lines, file = summary_file, sep = "\n", append = TRUE)
} else {
  cat(lines, sep = "\n")
}

if (!ok) quit(status = 1)
