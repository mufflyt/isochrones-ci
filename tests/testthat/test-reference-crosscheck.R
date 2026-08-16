# Cross-check PRODUCTION against the independent reference.
#
# This is the file that gives the harness its point. Everything else checks that
# the model is self-consistent; this checks that the production implementation
# computes what the equations say it should.
#
# Production here is `mufflyaccess`, the public cross-repo SSOT that isochrones,
# cliff and twostep all read their canonical constants from. It is installed at
# an explicitly recorded commit -- see production_info() and the job summary.

test_that("production is present and its commit is recorded", {
  info <- require_production()
  if (!info$available) skip_recorded("production absent (REFERENCE_ONLY)")
  expect_true(info$available)
  # A run that cannot say WHICH commit it tested is not reproducible, so this is
  # a hard expectation in CI and a soft one locally.
  if (nzchar(Sys.getenv("CI"))) {
    expect_false(is.na(info$sha),
                 label = "CI must record the production SHA under test")
  }
})

test_that("CROSS-CHECK safe_divide: production and reference agree on zero semantics", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  cases <- list(c(6, 3), c(0, 5), c(0, 0), c(1, 0), c(-4, 2), c(1e9, 3))
  for (cs in cases) {
    prod_val <- mufflyaccess::safe_divide(cs[1], cs[2])
    ref_val  <- ref_safe_divide(cs[1], cs[2])
    if (is.na(ref_val)) {
      expect_true(is.na(prod_val),
                  info = sprintf("%g/%g is undefined; production returned %s",
                                 cs[1], cs[2], format(prod_val)))
    } else {
      expect_equal(as.numeric(prod_val), ref_val, tolerance = TOL_RELATIVE,
                   label = sprintf("%g/%g", cs[1], cs[2]))
    }
  }
})

test_that("CROSS-CHECK proportion CI: production Wilson matches the textbook formula", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  set.seed(base_seed())
  for (i in seq_len(max(5L, n_random_worlds()))) {
    n <- sample(5:2000, 1); x <- sample(0:n, 1)
    p <- mufflyaccess::calculate_proportion_ci(x, n)
    r <- ref_prop_ci_wilson(x, n)
    expect_equal(as.numeric(p$proportion), unname(r[["estimate"]]),
                 tolerance = 1e-10, label = sprintf("estimate x=%d n=%d", x, n))
    expect_equal(as.numeric(p$lower_ci), unname(r[["lower"]]),
                 tolerance = 1e-10, label = sprintf("lower x=%d n=%d", x, n))
    expect_equal(as.numeric(p$upper_ci), unname(r[["upper"]]),
                 tolerance = 1e-10, label = sprintf("upper x=%d n=%d", x, n))
    expect_gte(as.numeric(p$lower_ci), 0)
    expect_lte(as.numeric(p$upper_ci), 1)
  }
})

test_that("CROSS-CHECK MOE conversion: production factor matches the z-score ratio", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  exact <- ref_moe90_to_ci95_factor()          # qnorm(.975)/qnorm(.95) = 1.1916...
  prod  <- mufflyaccess::MOE90_TO_CI95_FACTOR  # 1.191489, the conventional
                                               # 1.96/1.645 rounding
  # Tolerance chosen deliberately. Production uses the conventional ROUNDED
  # z-scores, which differ from the exact ratio by about 1e-4 relative -- a
  # documented convention, not an error. 1e-3 accepts that and still rejects
  # 1.96/1.65 = 1.1879 (3e-3 off), which is the plausible typo.
  expect_equal(as.numeric(prod), exact, tolerance = 1e-3,
               label = "MOE 90->95 factor")
  expect_true(abs(prod - exact) / exact < 1e-3)
})

test_that("CROSS-CHECK allocation: production conserves mass exactly", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  w <- mufflyaccess::urps_state_alloc_weights()
  expect_equal(length(w), 49L, label = "allocation weights cover 48 states + DC")
  expect_equal(sum(w), 1, tolerance = 1e-12, label = "weights are normalised")

  # CONSERVATION, INTEGRALITY, NON-NEGATIVITY hold for every total, including
  # degenerate ones. These are the load-bearing properties: an allocator that
  # loses or invents people is broken regardless of how it rounds.
  for (total in c(1L, 7L, 49L, 1306L, 100000L)) {
    a <- mufflyaccess::urps_allocate_national(total, w)
    expect_equal(sum(a$n_allocated), as.numeric(total), tolerance = 1e-9,
                 label = sprintf("allocated mass for total=%d", total))
    expect_equal(nrow(a), 49L, label = "every unit receives a row")
    expect_true(all(a$n_allocated >= 0), label = "no negative allocation")
    expect_true(all(a$n_allocated == round(a$n_allocated)),
                label = "allocation is a whole number of people")
  }
})

test_that("CROSS-CHECK allocation: proportional at the scale it is used", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  w <- mufflyaccess::urps_state_alloc_weights()

  # PROPORTIONALITY is asserted only in the documented operating range: this
  # function allocates a national workforce count across 49 units, and the real
  # inputs are on the order of 1,300.
  #
  # THE BOUND IS EMPIRICAL, AND SAYING SO MATTERS. A correct largest-remainder
  # allocator is within 1 unit of the exact share everywhere. Production is not:
  # measured 2026-08-16, the worst deviation is 1.998 at total=1,000, 0.483 at
  # 1,306, 0.483 at 5,000, 0.493 at 20,000 and 1.157 at 100,000 -- not monotone,
  # and above 1 at two of the five. So this is a REGRESSION bound fitted to
  # observed behaviour of an unspecified rounding rule, not a proof of
  # proportionality. It will catch the allocator drifting to 50 units off; it
  # will not catch it drifting to 2.4.
  #
  # If production ever documents its rounding rule, replace this with an
  # assertion of that rule and delete the paragraph above.
  EMPIRICAL_DEVIATION_BOUND <- 2.5
  devs <- vapply(c(1000L, 1306L, 5000L, 20000L, 100000L), function(total) {
    a <- mufflyaccess::urps_allocate_national(total, w)
    obs <- a$n_allocated[match(names(w), a$state_abbr)]
    max(abs(obs - ref_allocate(as.numeric(total), as.numeric(w))))
  }, numeric(1))

  expect_true(all(devs < EMPIRICAL_DEVIATION_BOUND),
              info = sprintf(paste0("allocation deviation from the exact share exceeded the ",
                                    "observed bound of %.1f units: %s"),
                             EMPIRICAL_DEVIATION_BOUND,
                             paste(sprintf("%.3f", devs), collapse = ", ")))

  # A correct largest-remainder allocator would satisfy `< 1`. Production does
  # not, and this records that fact so it cannot be forgotten: if it ever starts
  # to, that is an improvement worth noticing and tightening the bound for.
  expect_true(any(devs > 1),
              info = paste0("production now allocates within 1 unit everywhere -- it may have ",
                            "adopted largest remainder. Tighten this test to `< 1`."))
})

test_that("OBSERVATION: allocation is far from proportional when the total is small", {
  # NOT AN ASSERTION ABOUT PRODUCTION. Recorded because a harness that notices
  # something and says nothing is worse than one that never looked.
  #
  # Measured 2026-08-16 against mufflyaccess:
  #   total=1      max deviation from exact share 0.88
  #   total=7      max deviation 5.17  (California receives 6 of 7; its exact
  #                share is 0.83, and 47 of 49 states receive nothing)
  #   total=49     max deviation 3.22
  #   total=1306   max deviation 0.48  <- the real operating scale, well behaved
  #
  # Mass is conserved in every case, so nothing is lost or invented. Whether the
  # small-total behaviour is intended (a greedy rule) or a rounding defect is a
  # question for the production maintainer; this harness records the numbers and
  # declines to guess. If it is intended, turn this into an assertion of the
  # intended rule. If it is not, the fix belongs in mufflyaccess.
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  w <- mufflyaccess::urps_state_alloc_weights()

  obs_tbl <- do.call(rbind, lapply(c(1L, 7L, 49L, 1306L), function(total) {
    a <- mufflyaccess::urps_allocate_national(total, w)
    o <- a$n_allocated[match(names(w), a$state_abbr)]
    e <- ref_allocate(as.numeric(total), as.numeric(w))
    data.frame(total = total, max_abs_deviation = max(abs(o - e)),
               n_nonzero = sum(o > 0), conserved = isTRUE(all.equal(sum(o), as.numeric(total))),
               stringsAsFactors = FALSE)
  }))
  dir.create(file.path(.harness_root, "artifacts"), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(obs_tbl, file.path(.harness_root, "artifacts", "allocation-observation.csv"),
                   row.names = FALSE)

  # The only hard claim here: conservation never breaks, at any scale.
  expect_true(all(obs_tbl$conserved),
              info = "mass conservation must hold even where proportionality does not")
  # And the observation itself must remain true, so this does not rot into a
  # comment describing behaviour that has since changed.
  expect_gt(obs_tbl$max_abs_deviation[obs_tbl$total == 7L], 1,
            label = "small-total deviation is still present (update this note if it is fixed)")
})

test_that("CROSS-CHECK identifier canonicalisation", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  cases <- c("1234567890", "123456789", " 12-34.56 7890 ", "abc1234567",
             "1.23e9", "", NA_character_, "12345678901")
  prod <- mufflyaccess::canon_npi(cases, verbose = FALSE)
  ref  <- ref_canon_id(cases)
  for (i in seq_along(cases)) {
    expect_identical(prod[i], ref[i],
                     info = sprintf("input %s: production=%s reference=%s",
                                    ifelse(is.na(cases[i]), "NA", shQuote(cases[i])),
                                    ifelse(is.na(prod[i]), "NA", prod[i]),
                                    ifelse(is.na(ref[i]), "NA", ref[i])))
  }
})

test_that("CROSS-CHECK rurality: the metro/nonmetro boundary sits between RUCA 3 and 4", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  codes <- c(1, 2, 3, 4, 5, 10)
  prod <- mufflyaccess::rurality_from_ruca(codes)
  ref  <- ref_rurality(codes)
  # Production uses "Metropolitan"/"Rural"; the reference uses "metro"/"nonmetro".
  # The CLAIM under test is the boundary, not the vocabulary, so compare the
  # partition rather than the strings.
  expect_identical(prod == prod[1], ref == ref[1],
                   info = paste0("metro/nonmetro partition differs.\n",
                                 "  production: ", paste(prod, collapse = ", "), "\n",
                                 "  reference : ", paste(ref, collapse = ", ")))
  expect_equal(as.numeric(mufflyaccess::RUCA_NONMETRO_MIN), 4,
               label = "the documented nonmetro threshold")
})
