# Mathematical invariants that must hold for ANY valid world.
#
# These are properties of the accessibility model itself, not of a particular
# fixture. They are checked against many randomly generated worlds, so a
# violation means the model is wrong, not that one example was unlucky.
#
# WHAT IS DELIBERATELY *NOT* ASSERTED HERE
# Accessibility is NOT monotone in the travel-time threshold, and asserting that
# it were would be a scientific error on this harness's part. Under 2SFCA, a
# larger threshold enlarges every provider's catchment, which enlarges the
# denominator in step 1, which SHRINKS each provider-to-population ratio. The
# committed golden fixture shows exactly this: access_sum rises from 30 to 120
# minutes and then FALLS at 180. Coverage is monotone; accessibility is not.
# See SCIENTIFIC_VALIDATION.md.

test_that("COVERAGE MONOTONICITY: the set of reached tracts only grows with the threshold", {
  worlds <- lapply(seq_len(max(3L, n_random_worlds())), function(i) {
    make_world(seed = base_seed() + i, n_providers = 6L, n_tracts = 20L)
  })
  for (w in worlds) {
    r30  <- names(which(ref_reached(w$tt, 30)))
    r60  <- names(which(ref_reached(w$tt, 60)))
    r120 <- names(which(ref_reached(w$tt, 120)))
    expect_true(all(r30 %in% r60),
                info = sprintf("seed %d: a tract reachable in 30 min was not reachable in 60",
                               w$meta$seed))
    expect_true(all(r60 %in% r120),
                info = sprintf("seed %d: a tract reachable in 60 min was not reachable in 120",
                               w$meta$seed))
  }
})

test_that("POPULATION MONOTONICITY: reachable population is non-decreasing in the threshold", {
  for (i in seq_len(max(3L, n_random_worlds()))) {
    w <- make_world(seed = base_seed() + 1000L + i, n_providers = 6L, n_tracts = 20L)
    s <- vapply(c(30, 60, 120, 180),
                function(b) ref_pop_share_within(w$tt, w$tracts, b), numeric(1))
    expect_false(any(is.na(s)),
                 info = "this world has population, so no share should be undefined")
    expect_true(all(diff(s) >= -1e-12),
                info = sprintf("seed %d: population share fell as the threshold rose: %s",
                               w$meta$seed, paste(format(s), collapse = " -> ")))
    expect_true(all(s >= 0 & s <= 1),
                info = "a population share outside [0,1] is not a share")
  }
})

test_that("PROVIDER MONOTONICITY: removing a provider cannot increase raw coverage", {
  # Stated for COVERAGE, which is genuinely monotone in the provider set.
  # It is NOT stated for the 2SFCA score, which can rise when a provider is
  # removed: the removed provider's catchment population leaves its competitors'
  # denominators. That is real behaviour of the metric, not a defect, and a
  # harness that asserted otherwise would be generating false alarms.
  for (i in seq_len(max(3L, n_random_worlds() %/% 2L))) {
    w <- make_world(seed = base_seed() + 2000L + i, n_providers = 6L, n_tracts = 18L,
                    isolated = 0L)
    full <- sum(ref_reached(w$tt, 60))
    for (drop in w$providers$provider_id[1:2]) {
      tt2 <- w$tt[w$tt$provider_id != drop, , drop = FALSE]
      fewer <- sum(ref_reached(tt2, 60))
      expect_lte(fewer, full,
                 label = sprintf("seed %d: dropping %s INCREASED tracts reached (%d -> %d)",
                                 w$meta$seed, drop, full, fewer))
    }
  }
})

test_that("POPULATION CONSERVATION: allocation preserves the total exactly", {
  for (i in seq_len(max(3L, n_random_worlds()))) {
    set.seed(base_seed() + 3000L + i)
    w <- runif(20, 0, 10)
    w[sample.int(20, 3)] <- 0          # zero-weight units must not be dropped
    total <- 1306
    alloc <- ref_allocate(total, w)
    expect_equal(sum(alloc), total, tolerance = 1e-10,
                 label = "allocated mass must equal the national total")
    expect_equal(length(alloc), length(w),
                 label = "every unit must receive an allocation, including zero-weight ones")
    expect_true(all(alloc >= 0), label = "no negative allocation")
    expect_equal(sum(alloc[w == 0]), 0,
                 label = "zero-weight units receive exactly zero, not a rounding crumb")
  }
})

test_that("ALLOCATION refuses undefined inputs instead of guessing", {
  expect_error(ref_allocate(100, c(0, 0, 0)), "sum to zero")
  expect_error(ref_allocate(100, c(1, NA, 3)), "NA weight")
  expect_error(ref_allocate(100, c(1, -1, 3)), "negative weight")
})

test_that("ZERO SEMANTICS: 0, NA, NaN, Inf and 'empty' do not collapse into each other", {
  # This is the most important test in the file. Each of these states means
  # something different, and the failure mode is silent: a pipeline that turns
  # "undefined" into 0 reports full coverage of a population that does not exist.
  expect_true(is.na(ref_safe_divide(0, 0)),  label = "0/0 is undefined, not 0")
  expect_true(is.na(ref_safe_divide(1, 0)),  label = "x/0 is undefined, not Inf")
  expect_equal(ref_safe_divide(0, 5), 0,     label = "0/5 is a real zero")
  expect_true(is.na(ref_safe_divide(NA, 5)), label = "NA numerator stays NA")

  # A world with population but no reachable provider: a genuine 0.
  w <- make_world(seed = base_seed() + 77L, n_providers = 2L, n_tracts = 5L,
                  isolated = 2L, zero_pop = 0L)
  share <- ref_pop_share_within(w$tt, w$tracts, 1)   # 1 minute: nothing reachable
  expect_equal(share, 0, label = "unreachable population is 0 coverage, a real number")
  expect_false(is.na(share), label = "0 coverage must not be reported as NA")

  # A world with zero total population: undefined, NOT 0.
  w0 <- w; w0$tracts$population <- 0L
  expect_true(is.na(ref_pop_share_within(w0$tt, w0$tracts, 60)),
              label = "a share of nobody is undefined, not 0")
})

test_that("2SFCA is internally consistent: unreachable tracts score exactly zero", {
  w <- make_world(seed = base_seed() + 4000L, n_providers = 5L, n_tracts = 15L,
                  isolated = 2L)
  a <- ref_2sfca(w$tt, w$providers, w$tracts, 60)
  reached <- ref_reached(w$tt, 60)
  unreached <- names(which(!reached))
  expect_true(all(a$tract_access[unreached] == 0),
              info = "a tract reaching no provider must score 0, not NA and not a small number")
  expect_false(any(is.na(a$tract_access)),
               info = "tract accessibility must never be NA: 'no access' is 0")
  expect_true(all(a$tract_access >= 0), info = "accessibility cannot be negative")
})

test_that("2SFCA supply is bounded: total access-weighted population cannot exceed supply", {
  # sum over tracts of A_t * Pop_t equals the total supply that is actually
  # reachable, because each provider's supply is redistributed over exactly the
  # population in its catchment. Any excess means supply was double-counted.
  for (i in seq_len(max(3L, n_random_worlds() %/% 2L))) {
    w <- make_world(seed = base_seed() + 5000L + i, n_providers = 6L, n_tracts = 20L,
                    isolated = 0L, zero_pop = 0L)
    a <- ref_2sfca(w$tt, w$providers, w$tracts, 120)
    weighted <- sum(a$tract_access * w$tracts$population[
      match(names(a$tract_access), w$tracts$tract_id)])
    expect_lte(weighted, sum(w$providers$supply) + 1e-9,
               label = sprintf("seed %d: supply appears duplicated (%.6f > %.6f)",
                               w$meta$seed, weighted, sum(w$providers$supply)))
  }
})
