#!/usr/bin/env Rscript
# The single entry point for every CI tier.
#
# Runs the suite, writes the accounting and the completion sentinel, emits the
# job summary, and exits non-zero on failure. The sentinel is written BEFORE the
# failure exit on purpose: "did it finish" and "did it pass" are two independent
# signals, and conflating them is how a run that died halfway gets read as a
# clean pass.
#
#   Rscript scripts/run-ci.R            # everything
#   Rscript scripts/run-ci.R --tier=pr  # the fast gate

args <- commandArgs(trailingOnly = TRUE)
tier <- sub("^--tier=", "", grep("^--tier=", args, value = TRUE))
if (length(tier) == 0L) tier <- "full"

.script_path <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) normalizePath(f[1], mustWork = FALSE) else NA_character_
})
root <- if (!is.na(.script_path)) {
  normalizePath(file.path(dirname(.script_path), ".."))
} else {
  normalizePath(".")
}
setwd(root)

suppressPackageStartupMessages(library(testthat))
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

# RULE: lift testthat's failure cap. The default of 10 stops the run and reports
# a PREFIX of the suite as though it were the whole thing.
testthat::set_max_fails(Inf)

# The PR tier runs the fast, deterministic gate. The full tier adds the
# randomized, adversarial and sabotage layers. Both lists are EXPLICIT so that a
# file which silently stops being collected is detectable -- see the
# expected-vs-run comparison in verify-test-accounting.R.
PR_FILES <- c("test-contracts.R", "test-production-contract.R",
              "test-mathematical-invariants.R", "test-reference-crosscheck.R",
              "test-statistics-crosscheck.R", "test-workforce-invariants.R",
              "test-end-to-end.R", "test-reproducibility.R", "test-geography.R",
              "test-name-matching-primitives.R")
ALL_FILES <- c(PR_FILES, "test-metamorphic.R", "test-adversarial.R",
               "test-chunking-invariance.R", "test-mutation-sabotage.R")

expected <- if (identical(tier, "pr")) PR_FILES else ALL_FILES
present  <- list.files("tests/testthat", pattern = "^test.*[.]R$")
missing  <- setdiff(expected, present)
if (length(missing)) {
  # A test file named in the plan but absent from disk must FAIL. Silently
  # running fewer files than intended is the exact false-green this harness
  # exists to prevent.
  cat("::error title=Planned test file missing::", paste(missing, collapse = ", "), "\n")
  quit(status = 1)
}

dir.create("artifacts", showWarnings = FALSE, recursive = TRUE)
# Clear every artifact this run is supposed to produce. A stale
# mutation-report.csv from a previous run would otherwise be read by the summary
# below and reported as "12/12 mutants killed" by a PR tier that never ran a
# single mutant -- a small, plausible, entirely false claim.
unlink(c("artifacts/SUITE_COMPLETED", "artifacts/skips.txt",
         "artifacts/mutation-report.csv", "artifacts/mutation-report-chunking.csv",
         "artifacts/test-results.csv",
         "artifacts/test-accounting.txt", "artifacts/summary.txt",
         "artifacts/allocation-observation.csv"))

info <- production_info()
started <- Sys.time()

stems <- sub("^test[-_]", "", sub("[.][Rr]$", "", expected))
stems <- vapply(stems, function(x) gsub("([][{}()+*^$|?.\\\\])", "\\\\\\1", x),
                character(1), USE.NAMES = FALSE)

res <- test_dir("tests/testthat",
                filter = paste0("^(", paste(stems, collapse = "|"), ")$"),
                reporter = "progress", stop_on_failure = FALSE)

df <- as.data.frame(res)
acct <- write_accounting(res, dir = "artifacts", expected_files = expected)

n_pass <- sum(df$passed); n_fail <- sum(df$failed)
n_err  <- sum(df$error);  n_skip <- sum(df$skipped)
elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1)

# ------------------------------------------------------------- job summary ---
mut <- tryCatch(utils::read.csv("artifacts/mutation-report.csv", stringsAsFactors = FALSE),
                error = function(e) NULL)
per_file <- stats::aggregate(cbind(passed, failed, error, skipped) ~ file, data = df, FUN = sum)
verdict <- function(f) {
  r <- per_file[per_file$file == f, ]
  if (nrow(r) == 0L) "NOT RUN" else if (r$failed + r$error > 0) "FAIL" else "PASS"
}

summary_lines <- c(
  "Isochrones CI Harness",
  "=====================",
  "",
  sprintf("Tier:            %s", tier),
  sprintf("Production repo: %s", info$repo),
  sprintf("Production SHA:  %s", if (is.na(info$sha)) "(not recorded)" else info$sha),
  sprintf("Production ver:  %s", if (is.na(info$version)) "(absent)" else info$version),
  sprintf("R version:       %s", as.character(getRversion())),
  sprintf("Fixture version: %s", tryCatch(
    as.character(yaml::read_yaml("fixtures/manifest.yml")$fixture_version),
    error = function(e) "?")),
  sprintf("Seed:            %s", Sys.getenv("HARNESS_SEED", unset = "20260816")),
  sprintf("Random worlds:   %s", Sys.getenv("HARNESS_RANDOM_WORLDS", unset = "25")),
  sprintf("Elapsed:         %s min", elapsed),
  "",
  sprintf("Contract tests:          %s", verdict("test-contracts.R")),
  sprintf("Production contract:     %s", verdict("test-production-contract.R")),
  sprintf("Mathematical invariants: %s", verdict("test-mathematical-invariants.R")),
  sprintf("Reference cross-check:   %s", verdict("test-reference-crosscheck.R")),
  sprintf("Statistics cross-check:  %s", verdict("test-statistics-crosscheck.R")),
  sprintf("Workforce invariants:    %s", verdict("test-workforce-invariants.R")),
  sprintf("Metamorphic tests:       %s", verdict("test-metamorphic.R")),
  sprintf("Chunking invariance:     %s", verdict("test-chunking-invariance.R")),
  sprintf("Adversarial tests:       %s", verdict("test-adversarial.R")),
  sprintf("Geometry / CRS:          %s", verdict("test-geography.R")),
  sprintf("Reproducibility:         %s", verdict("test-reproducibility.R")),
  sprintf("End-to-end golden:       %s", verdict("test-end-to-end.R")),
  sprintf("Mutation sabotage:       %s", verdict("test-mutation-sabotage.R")),
  "",
  "Assertions:",
  sprintf("  %s passed", format(n_pass, big.mark = ",")),
  sprintf("  %s failed", format(n_fail, big.mark = ",")),
  sprintf("  %s errors", format(n_err, big.mark = ",")),
  sprintf("  %s skipped", format(n_skip, big.mark = ",")),
  "",
  {
    mc <- tryCatch(utils::read.csv("artifacts/mutation-report-chunking.csv",
                                   stringsAsFactors = FALSE), error = function(e) NULL)
    k <- sum(c(if (!is.null(mut)) mut$killed, if (!is.null(mc)) mc$killed))
    n <- sum(c(if (!is.null(mut)) nrow(mut), if (!is.null(mc)) nrow(mc)))
    if (n > 0L) sprintf("Mutation tests:\n  %d/%d mutants killed (%d core, %d chunking)",
                        k, n, if (is.null(mut)) 0L else nrow(mut),
                        if (is.null(mc)) 0L else nrow(mc))
    else "Mutation tests:\n  not run in this tier"
  },
  "",
  sprintf("Test accounting:\n  %s",
          if (file.exists("artifacts/SUITE_COMPLETED")) "COMPLETE" else "INCOMPLETE"),
  sprintf("  files planned %d, files run %d", length(expected), acct$tests_started),
  if (nzchar(acct$files_missing)) sprintf("  MISSING: %s", acct$files_missing) else "  none missing"
)

writeLines(summary_lines, "artifacts/summary.txt")
cat("\n", paste(summary_lines, collapse = "\n"), "\n", sep = "")

gs <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(gs)) {
  cat(paste0("```\n", paste(summary_lines, collapse = "\n"), "\n```\n"),
      file = gs, append = TRUE)
}

if (file.exists("artifacts/skips.txt")) {
  cat("\nRECORDED SKIPS (each is something this run did NOT check):\n")
  cat(paste0("  ", readLines("artifacts/skips.txt")), sep = "\n")
}

if (n_fail + n_err > 0) {
  bad <- df[df$failed > 0 | df$error > 0, c("file", "test", "failed", "error")]
  cat("\nFailing tests:\n"); print(bad, row.names = FALSE)
  quit(status = 1)
}
quit(status = 0)
