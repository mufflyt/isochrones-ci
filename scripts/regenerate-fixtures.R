#!/usr/bin/env Rscript
# Regenerate the committed fixtures and golden expected values.
#
# GOLDEN OUTPUTS ARE NOT REWRITTEN SILENTLY. Run with no arguments and this
# script reports what WOULD change and exits non-zero if anything differs. Only
# `--accept` writes. That asymmetry is the whole point: a golden file that
# updates itself whenever the code changes records nothing, because it can never
# disagree.
#
#   Rscript scripts/regenerate-fixtures.R            # diff only, no writes
#   Rscript scripts/regenerate-fixtures.R --accept   # write, after you read the diff

args <- commandArgs(trailingOnly = TRUE)
accept <- "--accept" %in% args

# Resolve the repo root from the script's own path when run via Rscript, else
# from the working directory. Doing this WITHOUT sys.frame() keeps it working
# under `Rscript path/to/script.R`, where there is no calling frame.
.script_path <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) normalizePath(f[1], mustWork = FALSE) else NA_character_
})
root <- if (!is.na(.script_path)) {
  normalizePath(file.path(dirname(.script_path), ".."), mustWork = FALSE)
} else normalizePath(".", mustWork = FALSE)
if (!dir.exists(file.path(root, "R"))) root <- normalizePath(".", mustWork = FALSE)

for (f in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) {
  source(f)
}

IN  <- file.path(root, "fixtures", "inputs")
EXP <- file.path(root, "fixtures", "expected")
dir.create(IN, recursive = TRUE, showWarnings = FALSE)
dir.create(EXP, recursive = TRUE, showWarnings = FALSE)

FIXTURE_VERSION <- 1L
CANON_SEED <- 20260816L

# ---- the one canonical world -------------------------------------------------
world <- make_world(seed = CANON_SEED, n_providers = 8L, n_tracts = 25L,
                    isolated = 1L, zero_pop = 2L, duplicate_coords = TRUE)

# ---- golden expected values --------------------------------------------------
# Structured, scientifically meaningful values -- NOT a serialized blob. A blob
# comparison tells you "something changed"; this tells you WHICH claim broke.
golden <- local({
  rows <- list()
  for (band in c(30, 60, 120, 180)) {
    a <- ref_2sfca(world$tt, world$providers, world$tracts, band)
    rows[[length(rows) + 1L]] <- data.frame(
      band_min          = band,
      n_tracts_reached  = sum(ref_reached(world$tt, band)),
      pop_share_within  = ref_pop_share_within(world$tt, world$tracts, band),
      total_population  = sum(world$tracts$population),
      n_providers_used  = sum(!is.na(a$provider_ratio)),
      access_sum        = sum(a$tract_access),
      access_mean       = mean(a$tract_access),
      access_max        = max(a$tract_access),
      n_zero_access     = sum(a$tract_access == 0),
      stringsAsFactors  = FALSE
    )
  }
  do.call(rbind, rows)
})

# Per-tract accessibility at the primary band, for a handful of named tracts.
# Exact expected values for specific identifiers catch a reordering bug that
# aggregate totals cannot see.
canonical_tracts <- local({
  a <- ref_2sfca(world$tt, world$providers, world$tracts, 60)
  data.frame(tract_id = names(a$tract_access),
             access_60 = as.numeric(a$tract_access),
             population = world$tracts$population[
               match(names(a$tract_access), world$tracts$tract_id)],
             stringsAsFactors = FALSE)
})

targets <- list(
  "inputs/providers.csv"          = world$providers,
  "inputs/tracts.csv"             = world$tracts,
  "inputs/travel_times.csv"       = world$tt,
  "expected/golden_by_band.csv"   = golden,
  "expected/golden_tracts_60.csv" = canonical_tracts
)

changed <- character(0)
for (rel in names(targets)) {
  path <- file.path(root, "fixtures", rel)
  new_txt <- paste(utils::capture.output(
    utils::write.csv(targets[[rel]], stdout(), row.names = FALSE)), collapse = "\n")
  old_txt <- if (file.exists(path)) paste(readLines(path, warn = FALSE), collapse = "\n") else NA_character_
  if (is.na(old_txt) || !identical(old_txt, new_txt)) {
    changed <- c(changed, rel)
    if (accept) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      writeLines(new_txt, path)
    }
  }
}

manifest <- sprintf(
'fixture_version: %d
created: %s
generator_seed: %d
generator_version: 1
description: >
  A synthetic Colorado-shaped validation universe. Eight providers (one
  deliberately isolated, two sharing exact coordinates), twenty-five tracts (two
  with zero population), and a full travel-time matrix. Everything is generated
  from the seed above; nothing is derived from a real person, provider, address,
  or census tract.
sources:
  - name: synthetic
    origin: R/fixture_helpers.R::make_world()
    license: "MIT (this repository); no external data incorporated"
    redistributable: true
regenerate: "Rscript scripts/regenerate-fixtures.R --accept"
expected_production_contract:
  package: mufflyaccess
  asserts:
    - CANONICAL_BANDS == [30, 60, 120, 180]
    - PRIMARY_ACCESS_BAND_MIN is a member of CANONICAL_BANDS
    - CONUS_STATE_ABBR has 49 entries (48 states + DC)
    - NON_CONTIGUOUS_CODES excludes every CONUS_STATE_ABBR entry
', FIXTURE_VERSION, format(Sys.Date()), CANON_SEED)

mpath <- file.path(root, "fixtures", "manifest.yml")
old_m <- if (file.exists(mpath)) readLines(mpath, warn = FALSE) else character(0)
# Ignore the `created:` line when deciding whether the manifest changed, so a
# no-op rerun on a different day is not reported as a diff.
strip_created <- function(x) {
  x <- x[!grepl("^created:", x)]
  x[nzchar(trimws(x)) | seq_along(x) < length(x)]   # drop a trailing blank line
}
new_m <- strsplit(sub("\n$", "", manifest), "\n", fixed = TRUE)[[1]]
if (!identical(strip_created(old_m), strip_created(new_m))) {
  changed <- c(changed, "manifest.yml")
  if (accept) writeLines(manifest, mpath)
}

if (length(changed) == 0L) {
  cat("fixtures are up to date\n")
  quit(status = 0)
}

cat(sprintf("%s %d fixture file(s):\n", if (accept) "WROTE" else "WOULD CHANGE",
            length(changed)))
cat(paste0("  ", changed, collapse = "\n"), "\n")
if (!accept) {
  cat("\nRe-run with --accept to write them, AFTER reading the diff.\n",
      "A golden file that updates itself records nothing.\n", sep = "")
  quit(status = 1)
}
quit(status = 0)
