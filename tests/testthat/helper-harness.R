# Loaded once by testthat before any test file.
#
# Sources the harness's own R/ directory. This repo is a package only so that
# R CMD check works on it; the helpers are plain scripts because a reviewer
# should be able to read them without knowing anything about package
# namespaces.

.harness_root <- local({
  # testthat sets the working directory to tests/testthat.
  for (up in c("../..", "..", ".")) {
    if (dir.exists(file.path(up, "R")) && file.exists(file.path(up, "DESCRIPTION"))) {
      return(normalizePath(up))
    }
  }
  normalizePath(".")
})

for (.f in list.files(file.path(.harness_root, "R"), pattern = "[.]R$",
                      full.names = TRUE)) {
  source(.f)
}

fixture_path <- function(...) file.path(.harness_root, "fixtures", ...)

read_fixture <- function(rel) {
  p <- fixture_path(rel)
  if (!file.exists(p)) {
    stop("fixture missing: ", rel,
         " -- a missing fixture must FAIL, not skip, or the suite reports ",
         "green having checked nothing.", call. = FALSE)
  }
  utils::read.csv(p, stringsAsFactors = FALSE, colClasses = c(tract_id = "character"))
}

# The canonical world, rebuilt from the recorded seed rather than read from
# disk, so the generator and the committed fixtures are cross-checked against
# each other by test-end-to-end.R.
CANON_SEED <- 20260816L
canon_world <- function() {
  make_world(seed = CANON_SEED, n_providers = 8L, n_tracts = 25L,
             isolated = 1L, zero_pop = 2L, duplicate_coords = TRUE)
}

# How many randomized worlds to test. Cheap by default so PR CI stays fast;
# nightly raises it via the environment. Recorded in the summary either way --
# "500 worlds tested" and "5 worlds tested" are very different claims.
n_random_worlds <- function(default = 25L) {
  as.integer(Sys.getenv("HARNESS_RANDOM_WORLDS", unset = as.character(default)))
}
base_seed <- function() {
  as.integer(Sys.getenv("HARNESS_SEED", unset = "20260816"))
}

# Skip helper that is LOUD. testthat's skip() is invisible in a summary; this
# records the reason so the job summary can report how much was not checked.
skip_recorded <- function(reason) {
  dir.create(file.path(.harness_root, "artifacts"), showWarnings = FALSE, recursive = TRUE)
  cat(sprintf("%s\n", reason),
      file = file.path(.harness_root, "artifacts", "skips.txt"), append = TRUE)
  testthat::skip(reason)
}
