#!/usr/bin/env Rscript
# Verify that the run this workflow just performed was COMPLETE.
#
# Runs as a separate CI step, after run-ci.R, with `if: always()`. Its whole job
# is to refuse to let a truncated run be reported as a pass. It checks four
# things that a zero exit status cannot tell you apart:
#
#   1. the completion sentinel exists           -> the process reached the end
#   2. the accounting file exists and parses    -> the numbers were recorded
#   3. every planned test file actually ran     -> nothing was silently dropped
#   4. some assertions actually executed        -> the suite was not vacuous
#
# A run that satisfies the exit status but fails any of these is red.

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

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a)) b else a

fail <- function(title, msg) {
  cat(sprintf("::error title=%s::%s\n", title, msg))
  quit(status = 1)
}

if (!file.exists("artifacts/SUITE_COMPLETED")) {
  fail("Suite terminated early",
       paste("artifacts/SUITE_COMPLETED is absent, so the R process never reached",
             "the end of the accounting block. The suite exited prematurely -- a",
             "sourced script calling quit(), a segfault, an OOM kill -- and this",
             "result must NOT be read as a pass. Exit status alone cannot detect",
             "this: quit(status = 0) exits zero."))
}

if (!file.exists("artifacts/test-accounting.txt")) {
  fail("Accounting missing", "artifacts/test-accounting.txt was never written.")
}

kv <- strsplit(readLines("artifacts/test-accounting.txt"), "=", fixed = TRUE)
acct <- stats::setNames(vapply(kv, function(x) paste(x[-1], collapse = "="), character(1)),
                        vapply(kv, `[`, character(1), 1))

num <- function(k) suppressWarnings(as.numeric(acct[[k]]))

if (nzchar(acct[["files_missing"]] %||% "")) {
  fail("Planned test files did not run",
       sprintf(paste("These files were planned but produced no results: %s.",
                     "A suite that quietly runs a subset reports a PREFIX as if",
                     "it were the whole thing."), acct[["files_missing"]]))
}

if (is.na(num("tests_passed")) || num("tests_passed") <= 0) {
  fail("Vacuous run",
       paste("Zero assertions passed. A green run in which nothing was checked",
             "is the failure this harness exists to prevent."))
}

if (!is.na(num("tests_expected")) &&
    num("tests_started") < num("tests_expected")) {
  fail("Incomplete run",
       sprintf("only %s of %s planned files ran",
               acct[["tests_started"]], acct[["tests_expected"]]))
}

cat("test accounting verified:\n")
cat(paste0("  ", readLines("artifacts/test-accounting.txt")), sep = "\n")
cat("\nsentinel:\n")
cat(paste0("  ", readLines("artifacts/SUITE_COMPLETED")), sep = "\n")
quit(status = 0)
