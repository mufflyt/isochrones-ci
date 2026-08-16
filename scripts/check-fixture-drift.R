#!/usr/bin/env Rscript
# Do the committed fixtures still reproduce from their recorded seed?
#
# THIS COMPARES VALUES, NOT TEXT, AND THAT DISTINCTION IS THE WHOLE POINT.
#
# The nightly originally regenerated the fixtures and ran `git diff`. That fails
# across R versions for a reason that has nothing to do with reproducibility:
# write.csv renders doubles via R's own formatting, and R 4.6.1 on the runner
# does not always produce the same last digit as R 4.4.2 locally. The first real
# nightly reported `inputs/travel_times.csv` drifted -- the file with by far the
# most doubles -- while providers.csv and tracts.csv did not.
#
# A byte comparison therefore answers "did R format this identically", when the
# claim being made is "does the generator still produce these numbers". This
# script asks the second question, and PRINTS the largest deviation so the cause
# is never ambiguous: ~1e-15 is float formatting, anything larger is a genuine
# determinism problem and must be treated as one.

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
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

CANON_SEED <- 20260816L
TOL <- 1e-9   # far below any scientifically meaningful difference, far above
              # the ~1e-16 noise of double formatting

world <- make_world(seed = CANON_SEED, n_providers = 8L, n_tracts = 25L,
                    isolated = 1L, zero_pop = 2L, duplicate_coords = TRUE)

compare <- function(rel, fresh) {
  disk <- utils::read.csv(file.path("fixtures", rel), stringsAsFactors = FALSE)
  problems <- character(0)
  if (!identical(dim(disk), dim(fresh))) {
    return(sprintf("%s: dimensions differ (disk %s, generated %s)",
                   rel, paste(dim(disk), collapse = "x"),
                   paste(dim(fresh), collapse = "x")))
  }
  if (!identical(sort(names(disk)), sort(names(fresh)))) {
    return(sprintf("%s: columns differ", rel))
  }
  worst <- 0
  for (col in names(fresh)) {
    a <- disk[[col]]; b <- fresh[[col]]
    if (is.numeric(b)) {
      d <- max(abs(as.numeric(a) - b))
      worst <- max(worst, d)
      if (d > TOL) problems <- c(problems, sprintf("%s$%s: max |diff| = %.3g", rel, col, d))
    } else if (!identical(as.character(a), as.character(b))) {
      problems <- c(problems, sprintf("%s$%s: character values differ", rel, col))
    }
  }
  cat(sprintf("  %-28s worst numeric deviation %.3g\n", rel, worst))
  problems
}

problems <- c(
  compare("inputs/providers.csv", world$providers),
  compare("inputs/tracts.csv", world$tracts),
  compare("inputs/travel_times.csv", world$tt)
)

if (length(problems)) {
  cat("::error title=Fixtures no longer reproduce from their seed::",
      paste(problems, collapse = "; "), "\n", sep = "")
  cat("\nThe committed fixtures are not what the generator produces. Either a\n",
      "fixture was hand-edited, or the generator changed. Regenerate\n",
      "deliberately with `Rscript scripts/regenerate-fixtures.R --accept` and\n",
      "put the before/after numbers in the commit message.\n", sep = "")
  quit(status = 1)
}
cat("fixtures still reproduce from seed ", CANON_SEED,
    " (tolerance ", format(TOL), ")\n", sep = "")
quit(status = 0)
