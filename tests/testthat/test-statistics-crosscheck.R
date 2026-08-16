# Cross-check production's statistical helpers against closed-form references.
#
# Every reference here is written from the formula, not delegated to the R
# function production also calls. Comparing prop.test() to prop.test() would
# certify agreement between a function and itself.

test_that("CROSS-CHECK two-proportion test: production applies Yates correction", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")

  # THE CORRECTION IS NOT A ROUNDING DETAIL. For 10/100 vs 20/100 the
  # uncorrected test gives p = 0.0477 and the corrected test gives p = 0.0747 --
  # the same data, on opposite sides of 0.05. Which one production uses can flip
  # a reported conclusion, so it is pinned explicitly rather than accepted
  # either way.
  set.seed(base_seed())
  cases <- list(c(10, 100, 20, 100), c(5, 50, 10, 50), c(1, 10, 9, 10),
                c(100, 1000, 110, 1000), c(0, 30, 5, 30), c(25, 25, 20, 25))
  for (i in seq_len(max(6L, n_random_worlds()))) {
    n1 <- sample(20:2000, 1); n2 <- sample(20:2000, 1)
    cases[[length(cases) + 1L]] <- c(sample.int(n1, 1), n1, sample.int(n2, 1), n2)
  }

  for (cs in cases) {
    prod <- mufflyaccess::calculate_two_prop_test(cs[1], cs[2], cs[3], cs[4])
    ref  <- ref_two_prop_test_yates(cs[1], cs[2], cs[3], cs[4])
    lbl <- sprintf("x1=%d/n1=%d x2=%d/n2=%d", cs[1], cs[2], cs[3], cs[4])

    if (is.na(prod$p_value) || is.na(ref[["p"]])) next
    expect_equal(as.numeric(prod$p_value), unname(ref[["p"]]), tolerance = 1e-10,
                 label = paste("p-value", lbl))
    expect_equal(as.numeric(prod$test_statistic), unname(ref[["chisq"]]),
                 tolerance = 1e-10, label = paste("statistic", lbl))
    expect_gte(as.numeric(prod$p_value), 0)
    expect_lte(as.numeric(prod$p_value), 1)
  }

  # And an explicit negative control on the correction itself: the UNCORRECTED
  # p must differ, so this test would fail if production switched.
  unc <- ref_two_prop_test(10, 100, 20, 100)
  cor <- ref_two_prop_test_yates(10, 100, 20, 100)
  expect_gt(abs(unc[["p"]] - cor[["p"]]), 0.02,
            label = "corrected and uncorrected must be meaningfully different here")
})

test_that("CROSS-CHECK annual trend: slope matches the closed-form OLS estimate", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  set.seed(base_seed() + 11L)
  for (i in seq_len(max(5L, n_random_worlds() %/% 2L))) {
    yr <- 2013:2023
    val <- 10 + 0.8 * seq_along(yr) + stats::rnorm(length(yr), 0, 0.4)
    a <- mufflyaccess::annual_trend(yr, val)
    expect_equal(as.numeric(a[["slope"]]), ref_ols_slope(yr, val), tolerance = 1e-9,
                 label = "OLS slope")
    # A confidence interval that does not contain its own point estimate is a
    # real defect and has shipped before in this project family.
    expect_lte(as.numeric(a[["lo"]]), as.numeric(a[["slope"]]))
    expect_gte(as.numeric(a[["hi"]]), as.numeric(a[["slope"]]))
    expect_gte(as.numeric(a[["p"]]), 0); expect_lte(as.numeric(a[["p"]]), 1)
  }
})

test_that("annual trend recovers a known slope exactly on noiseless data", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  yr <- 2013:2023
  # annual_trend returns a NAMED ATOMIC VECTOR, not a list; `$` on an atomic
  # vector is an error, not a NULL. Accessing it with [[ ]] is the fix.
  a <- mufflyaccess::annual_trend(yr, 5 + 3 * (yr - 2013))
  expect_equal(as.numeric(a[["slope"]]), 3, tolerance = 1e-9,
               label = "a perfectly linear series must return its own slope")
})

test_that("CROSS-CHECK zero-access share, including its UNITS", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  # Units are the interesting part: production returns a PERCENT. A consumer
  # treating it as a fraction under-reports by a factor of 100, and nothing
  # about the value's plausibility would reveal that.
  cases <- list(list(a = c(0, 1, 0, 2), w = 1:4),
                list(a = c(0, 0, 0), w = c(1, 1, 1)),
                list(a = c(1, 2, 3), w = c(5, 5, 5)),
                list(a = c(0, 5), w = c(9, 1)))
  for (cs in cases) {
    expect_equal(as.numeric(mufflyaccess::zero_access_share(cs$a, cs$w)),
                 ref_zero_share_pct(cs$a, cs$w), tolerance = 1e-10,
                 label = paste0("zero share for access=(",
                                paste(cs$a, collapse = ","), ")"))
  }
  expect_equal(as.numeric(mufflyaccess::zero_access_share(c(0, 0), c(1, 1))), 100,
               label = "all-zero access is 100 percent, not 1")
  expect_equal(as.numeric(mufflyaccess::zero_access_share(c(1, 1), c(1, 1))), 0,
               label = "no zero-access units is a real 0")
})

test_that("Monte Carlo CI is seed-deterministic and brackets its point estimate", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  args <- list(access = c(1, 2, 3, 4), est = c(0.2, 0.4, 0.6, 0.8),
               se = c(0.01, 0.02, 0.03, 0.04), B = 400, seed = 42L)
  a <- do.call(mufflyaccess::mc_weighted_ci, args)
  b <- do.call(mufflyaccess::mc_weighted_ci, args)
  expect_equal(unlist(a), unlist(b),
               label = "same seed must give the same interval; a Monte Carlo CI that drifts run to run cannot be published")

  expect_lte(as.numeric(a[["lo"]]), as.numeric(a[["point"]]))
  expect_gte(as.numeric(a[["hi"]]), as.numeric(a[["point"]]))

  # A different seed should move the interval a little, or the seed is ignored
  # and the "Monte Carlo" is not sampling anything.
  c_ <- do.call(mufflyaccess::mc_weighted_ci, modifyList(args, list(seed = 43L)))
  expect_false(identical(unlist(a), unlist(c_)),
               label = "a different seed produced an identical interval; is the seed wired?")
})
