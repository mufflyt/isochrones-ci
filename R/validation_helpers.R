# Comparison, tolerance, environment and test-accounting helpers.
#
# The recurring theme: a check that cannot fail is worse than no check, because
# it converts "nobody looked" into "we verified it". Several helpers here exist
# purely to make silent vacuity impossible -- a scan that finds nothing must say
# so, a guard that skips must be visible, and a suite that stops early must not
# be reportable as a pass.

# ---------------------------------------------------------------- tolerance ---

# Absolute tolerance for population counts. Populations are integers upstream,
# so any disagreement at all is a real disagreement, not floating point.
TOL_POPULATION <- 0

# Relative tolerance for accessibility ratios and shares. 1e-9 is far tighter
# than any scientifically meaningful difference and far looser than bit
# equality, which would fail on harmless reassociation of a sum.
TOL_RELATIVE <- 1e-9

# Geometric tolerance for containment, as a fraction of the smaller area. A
# projected buffer discretises a circle into segments, so exact containment of
# one disc in another is not achievable; 1e-6 is orders of magnitude below any
# real routing difference.
TOL_GEOMETRY_FRACTION <- 1e-6

#' Compare two numeric vectors, returning a readable diff rather than TRUE/FALSE
#'
#' A boolean tells you a test failed. This tells you which element, by how much,
#' and in which direction -- which is what a person needs at 2am, and what gets
#' written into the failure artifact.
numeric_diff <- function(observed, expected, tol = TOL_RELATIVE, label = "value") {
  stopifnot(length(observed) == length(expected))
  both_na <- is.na(observed) & is.na(expected)
  one_na  <- xor(is.na(observed), is.na(expected))
  denom <- pmax(abs(expected), 1)
  rel <- abs(observed - expected) / denom
  bad <- which(one_na | (!both_na & !is.na(rel) & rel > tol))
  if (length(bad) == 0L) return(NULL)
  data.frame(
    index    = bad,
    name     = if (!is.null(names(expected))) names(expected)[bad] else NA_character_,
    observed = observed[bad],
    expected = expected[bad],
    rel_diff = rel[bad],
    label    = label,
    stringsAsFactors = FALSE
  )
}

#' Format a diff for a failure message and an artifact
format_diff <- function(d, max_rows = 12L) {
  if (is.null(d) || nrow(d) == 0L) return("(no differences)")
  shown <- utils::head(d, max_rows)
  lines <- sprintf("  [%s] %s: observed=%s expected=%s rel=%.3g",
                   shown$index,
                   ifelse(is.na(shown$name), "-", shown$name),
                   format(shown$observed, digits = 10),
                   format(shown$expected, digits = 10),
                   shown$rel_diff)
  extra <- if (nrow(d) > max_rows) sprintf("\n  ... and %d more", nrow(d) - max_rows) else ""
  paste0(paste(lines, collapse = "\n"), extra)
}

# -------------------------------------------------------------- environment ---

#' Require sf, or FAIL -- never skip
#'
#' Geometry correctness is a headline claim of this harness. If sf is missing,
#' the honest outcome is a red build saying "the geometry claims were not
#' checked", not a green build with a quiet skip. Skips are for things that are
#' genuinely optional; a stated claim is not optional.
require_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("sf is required for the geometry claims in this harness. ",
         "A missing sf must FAIL rather than skip: a green run with the ",
         "geometry tests skipped would assert something this suite did not check.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Is the production package available, and at which commit?
#'
#' Returns a list even when absent, so callers always have something to record.
#' Every run writes this into the summary: a result that does not name the
#' commit it tested is not reproducible.
production_info <- function(pkg = "mufflyaccess") {
  have <- requireNamespace(pkg, quietly = TRUE)
  ver <- if (have) as.character(utils::packageVersion(pkg)) else NA_character_
  sha <- Sys.getenv("PRODUCTION_SHA", unset = NA_character_)
  if (is.na(sha) && have) {
    # RemoteSha is what remotes/pak records when installing from GitHub.
    d <- tryCatch(utils::packageDescription(pkg), error = function(e) NULL)
    if (!is.null(d) && !is.null(d$RemoteSha)) sha <- d$RemoteSha
  }
  list(package = pkg, available = have, version = ver,
       sha = if (is.na(sha)) NA_character_ else substr(sha, 1, 40),
       repo = Sys.getenv("PRODUCTION_REPO", unset = "mufflyt/mufflyaccess"))
}

#' Fail if production is absent when it was required
#'
#' PR and nightly runs REQUIRE production. Only an explicitly reference-only run
#' (REFERENCE_ONLY=1, used when debugging the harness itself) may proceed
#' without it -- and it says so loudly.
require_production <- function(pkg = "mufflyaccess") {
  info <- production_info(pkg)
  if (info$available) return(invisible(info))
  if (identical(Sys.getenv("REFERENCE_ONLY"), "1")) {
    message("REFERENCE_ONLY=1: production package '", pkg,
            "' absent; cross-checks will SKIP. This run proves nothing about production.")
    return(invisible(info))
  }
  stop("production package '", pkg, "' is not installed. ",
       "This harness exists to check production; running without it and ",
       "reporting green would be exactly the false signal it is built to prevent. ",
       "Set REFERENCE_ONLY=1 only when debugging the harness itself.",
       call. = FALSE)
}

# ---------------------------------------------------------------- accounting ---

#' Write the test accounting and completion sentinel
#'
#' The sentinel is written ONLY after the intended suite has run to the end, and
#' deliberately BEFORE the pass/fail exit, so that "did it finish" and "did it
#' pass" remain two independent signals. Conflating them is how a run that died
#' at file 400 of 4800 gets read as a clean pass.
write_accounting <- function(results, dir = "artifacts", expected_files = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  df <- as.data.frame(results)
  keep <- intersect(c("file", "context", "test", "passed", "failed",
                      "error", "skipped", "warning", "real"), names(df))
  df <- df[, keep, drop = FALSE]   # `result` is a LIST column; write.csv cannot encode it
  utils::write.csv(df, file.path(dir, "test-results.csv"), row.names = FALSE)

  files_run <- unique(as.character(df$file))
  acct <- list(
    tests_expected = if (is.null(expected_files)) NA_integer_ else length(expected_files),
    tests_started  = length(files_run),
    tests_completed = length(files_run),
    tests_passed   = sum(df$passed),
    tests_failed   = sum(df$failed),
    tests_skipped  = sum(df$skipped),
    tests_warned   = sum(df$warning),
    files_run      = paste(sort(files_run), collapse = ",")
  )
  missing <- if (is.null(expected_files)) character(0) else setdiff(expected_files, files_run)
  acct$files_missing <- paste(sort(missing), collapse = ",")

  writeLines(vapply(names(acct), function(k) paste0(k, "=", acct[[k]]), character(1)),
             file.path(dir, "test-accounting.txt"))

  # The sentinel. Its ABSENCE is the signal; its contents are for humans.
  writeLines(c(
    sprintf("completed_at=%s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
    sprintf("files=%d", length(files_run)),
    sprintf("passed=%d failed=%d errors=%d skipped=%d",
            sum(df$passed), sum(df$failed), sum(df$error), sum(df$skipped))
  ), file.path(dir, "SUITE_COMPLETED"))

  invisible(acct)
}
