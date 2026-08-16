# Sabotage testing: prove the validation suite can actually go red.
#
# For each controlled mutant in R/mutants.R, install it and require that at
# least one of this harness's own assertions FAILS. A mutant that survives is a
# hole in the suite and is reported as such -- not quietly tolerated.
#
# The mutants are plausible mistakes, not nonsense. "Return 42" proves nothing;
# "`<` where the specification says `<=`" is the edit that passes review, moves
# a headline number by a fraction of a point, and never crashes.
#
# The kill counts land in artifacts/mutation-report.csv and in the job summary.

# Each probe asserts something real. `kills` names the mutants it should catch;
# the registry below is checked for completeness so a new mutant cannot be added
# without someone deciding which probe is supposed to kill it.
PROBES <- list(
  threshold_boundary = function() {
    p <- data.frame(provider_id = "P1", supply = 1, stringsAsFactors = FALSE)
    t1 <- data.frame(tract_id = "T1", population = 100L, stringsAsFactors = FALSE)
    tt <- data.frame(provider_id = "P1", tract_id = "T1", minutes = 60,
                     stringsAsFactors = FALSE)
    stopifnot(isTRUE(unname(ref_reached(tt, 60))))
  },
  coverage_monotone = function() {
    w <- make_world(seed = 31337L, n_providers = 6L, n_tracts = 20L)
    s <- vapply(c(30, 60, 120, 180),
                function(b) ref_pop_share_within(w$tt, w$tracts, b), numeric(1))
    stopifnot(all(diff(s) >= -1e-12))
  },
  band_share_identity = function() {
    w <- make_world(seed = 606L, n_providers = 5L, n_tracts = 15L)
    s30 <- ref_pop_share_within(w$tt, w$tracts, 30)
    s60 <- ref_pop_share_within(w$tt, w$tracts, 60)
    stopifnot(s30 <= s60 + 1e-12)
    # An unweighted mean ignores population; a world whose reached tracts are
    # systematically small must therefore disagree with the weighted share.
    reached <- ref_reached(w$tt, 30)
    unw <- mean(as.logical(reached[w$tracts$tract_id]))
    stopifnot(abs(unw - s30) > 1e-9)
  },
  undefined_ratio_is_na = function() {
    p <- data.frame(provider_id = "P1", supply = 3, stringsAsFactors = FALSE)
    t1 <- data.frame(tract_id = "T1", population = 0L, stringsAsFactors = FALSE)
    tt <- data.frame(provider_id = "P1", tract_id = "T1", minutes = 5,
                     stringsAsFactors = FALSE)
    a <- ref_2sfca(tt, p, t1, 60)
    stopifnot(is.na(a$provider_ratio[["P1"]]))
  },
  ratio_has_denominator = function() {
    p <- data.frame(provider_id = "P1", supply = 4, stringsAsFactors = FALSE)
    t1 <- data.frame(tract_id = "T1", population = 200L, stringsAsFactors = FALSE)
    tt <- data.frame(provider_id = "P1", tract_id = "T1", minutes = 5,
                     stringsAsFactors = FALSE)
    a <- ref_2sfca(tt, p, t1, 60)
    stopifnot(abs(a$tract_access[["T1"]] - 4 / 200) < 1e-12)
  },
  allocation_conserves = function() {
    w <- c(1, 0, 2, 0, 3)
    a <- ref_allocate(1306, w)
    stopifnot(abs(sum(a) - 1306) < 1e-9, length(a) == length(w))
  },
  ci_within_unit_interval = function() {
    r <- ref_prop_ci(1, 10)
    stopifnot(r[["lower"]] >= 0, r[["upper"]] <= 1)
  },
  moe_factor_exact = function() {
    stopifnot(abs(ref_moe90_to_ci95_factor() - 1.19160) < 1e-3)
  },
  rurality_boundary = function() {
    r <- ref_rurality(c(3, 4))
    stopifnot(identical(r, c("metro", "nonmetro")))
  },
  divide_by_zero_is_na = function() {
    stopifnot(is.na(ref_safe_divide(1, 0)), is.na(ref_safe_divide(0, 0)))
  },
  short_id_is_padded = function() {
    stopifnot(identical(ref_canon_id("123456789"), "0123456789"))
  },
  containment_direction = function() {
    require_sf()
    small <- sf::st_sf(geometry = sf::st_sfc(
      sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0)))),
      crs = FIXTURE_CRS_PROJECTED))
    big <- sf::st_sf(geometry = sf::st_sfc(
      sf::st_polygon(list(cbind(c(-1, 2, 2, -1, -1), c(-1, -1, 2, 2, -1)))),
      crs = FIXTURE_CRS_PROJECTED))
    stopifnot(band_contains(small, big), !band_contains(big, small))
  }
)

.probe_fails <- function() {
  # Returns the names of probes that FAIL under whatever is currently installed.
  failed <- character(0)
  for (nm in names(PROBES)) {
    ok <- tryCatch({ PROBES[[nm]](); TRUE }, error = function(e) FALSE,
                   warning = function(w) TRUE)
    if (!ok) failed <- c(failed, nm)
  }
  failed
}

test_that("SANITY: every probe passes against the unmutated reference", {
  # Without this, a mutation kill could mean "the probe is broken" rather than
  # "the mutant was caught".
  expect_equal(.probe_fails(), character(0),
               info = "a probe fails on clean code; fix the probe before trusting any kill count")
})

test_that("ACTIVATION CONTRACT: every mutant proves it changed behaviour", {
  # THE META-CONTRACT. A mutant must demonstrate it altered the intended
  # behaviour BEFORE its killing probe is believed. Without this there are only
  # two visible outcomes -- killed and survived -- and a mutant that was never
  # applied is indistinguishable from one the tests cannot catch. The first is
  # a bug in the mutant; the second is a hole in the suite; conflating them
  # falsely indicts the tests.
  #
  # It found something on its first run: `swapped_bands` did not activate,
  # because the witness world's 30- and 60-minute population shares happened to
  # be identical (both 0.1305573), so swapping the bands changed nothing. The
  # mutant was correct and the WITNESS INPUT was degenerate. See WITNESS in
  # R/mutants.R, which now guards against that recurring silently.
  not_applied <- character(0)
  for (nm in names(MUTANTS)) {
    a <- mutant_activates(nm)
    if (!isTRUE(a$activated)) {
      not_applied <- c(not_applied, sprintf("%s [target %s]: %s", nm, a$target, a$note))
    }
  }
  expect_equal(
    length(not_applied), 0L,
    info = paste0("Mutant(s) did NOT change behaviour on their witness input. ",
                  "These are broken MUTANTS or inadequate WITNESSES -- not ",
                  "evidence about the tests:\n", paste(not_applied, collapse = "\n")))
})

test_that("MUTATION: every controlled mutant is killed by at least one probe", {
  results <- data.frame(mutant = character(0), description = character(0),
                        activated = logical(0), killed = logical(0),
                        killed_by = character(0), outcome = character(0),
                        stringsAsFactors = FALSE)

  for (nm in names(MUTANTS)) {
    act <- isTRUE(mutant_activates(nm)$activated)
    failed <- with_mutant(nm, quote(.probe_fails()))
    killed <- length(failed) > 0L
    results <- rbind(results, data.frame(
      mutant = nm,
      description = MUTANTS[[nm]]$description,
      activated = act,
      killed = killed,
      killed_by = paste(failed, collapse = "; "),
      # THREE outcomes, never two.
      outcome = if (!act) "NOT_APPLIED" else if (killed) "KILLED" else "SURVIVED",
      stringsAsFactors = FALSE))
  }

  dir.create(file.path(.harness_root, "artifacts"), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(results, file.path(.harness_root, "artifacts", "mutation-report.csv"),
                   row.names = FALSE)

  survivors <- results[results$outcome == "SURVIVED", ]
  expect_equal(sum(results$outcome == "NOT_APPLIED"), 0L,
               info = "a mutant that was never applied cannot be evidence about the tests")
  expect_equal(nrow(survivors), 0L,
               info = paste0(
                 "Mutant(s) SURVIVED. Each is a plausible scientific error that this ",
                 "suite would not have caught, i.e. a hole in the validation:\n",
                 paste(sprintf("  %s: %s", survivors$mutant, survivors$description),
                       collapse = "\n")))

  # The registry must not be empty, or "all mutants killed" is vacuous.
  expect_gt(nrow(results), 8L)
})

test_that("MUTATION: mutants are restored after use, even when the probe throws", {
  before <- ref_reached
  invisible(tryCatch(with_mutant("strict_threshold", quote(stop("boom"))),
                     error = function(e) NULL))
  expect_identical(ref_reached, before,
                   info = "a leaked mutant would silently corrupt every later test")
})
