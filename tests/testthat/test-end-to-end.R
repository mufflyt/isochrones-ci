# The golden end-to-end fixture.
#
# One canonical miniature run, compared against committed expected values. The
# comparison is STRUCTURED, not a serialized-object diff: a blob comparison
# tells you "something moved", this tells you which scientific claim broke.

test_that("E2E: per-band golden summary still holds", {
  w <- canon_world()
  golden <- read_fixture("expected/golden_by_band.csv")
  expect_equal(nrow(golden), 4L, label = "one row per canonical band")

  for (i in seq_len(nrow(golden))) {
    band <- golden$band_min[i]
    a <- ref_2sfca(w$tt, w$providers, w$tracts, band)

    expect_equal(sum(ref_reached(w$tt, band)), golden$n_tracts_reached[i],
                 label = sprintf("tracts reached at %d min", band))
    expect_equal(ref_pop_share_within(w$tt, w$tracts, band),
                 golden$pop_share_within[i], tolerance = 1e-12,
                 label = sprintf("population share at %d min", band))
    expect_equal(sum(w$tracts$population), golden$total_population[i],
                 label = "total population is a fixture constant")
    expect_equal(sum(!is.na(a$provider_ratio)), golden$n_providers_used[i],
                 label = sprintf("providers with a defined ratio at %d min", band))
    expect_equal(sum(a$tract_access), golden$access_sum[i], tolerance = 1e-12,
                 label = sprintf("summed accessibility at %d min", band))
    expect_equal(max(a$tract_access), golden$access_max[i], tolerance = 1e-12,
                 label = sprintf("maximum accessibility at %d min", band))
    expect_equal(sum(a$tract_access == 0), golden$n_zero_access[i],
                 label = sprintf("tracts with zero access at %d min", band))
  }
})

test_that("E2E: per-tract golden values at the primary band", {
  # Aggregates can be right while individual tracts are shuffled. These are the
  # named values that catch that.
  w <- canon_world()
  gt <- read_fixture("expected/golden_tracts_60.csv")
  a <- ref_2sfca(w$tt, w$providers, w$tracts, 60)

  expect_setequal(gt$tract_id, names(a$tract_access))
  obs <- as.numeric(a$tract_access[gt$tract_id])
  d <- numeric_diff(obs, gt$access_60, tol = 1e-12, label = "access_60")
  expect_null(d, info = paste0("per-tract accessibility drifted from golden\n", format_diff(d)))

  expect_equal(gt$population[match(w$tracts$tract_id, gt$tract_id)],
               w$tracts$population,
               label = "golden populations still match the generator")
})

test_that("E2E: structural properties of the golden output", {
  gt <- read_fixture("expected/golden_tracts_60.csv")
  expect_equal(nrow(gt), 25L, label = "row count")
  expect_false(anyDuplicated(gt$tract_id) > 0L, label = "tract_id is a unique key")
  expect_false(any(is.na(gt$access_60)), label = "no NA accessibility: 'none' is 0")
  expect_true(all(gt$access_60 >= 0))
  expect_true(all(gt$population >= 0))
  expect_true(any(gt$population == 0),
              label = "the fixture must retain its zero-population tracts")
  expect_true(any(gt$access_60 == 0),
              label = "the fixture must retain its zero-access tracts")
})

test_that("E2E: the golden band summary is internally monotone where it should be", {
  # A guard on the FIXTURE itself. If a regeneration ever produced a golden file
  # whose coverage fell as the threshold rose, the fixture would be wrong and
  # every test above would be validating nonsense.
  golden <- read_fixture("expected/golden_by_band.csv")
  golden <- golden[order(golden$band_min), ]
  expect_true(all(diff(golden$n_tracts_reached) >= 0),
              info = "coverage must not fall as the band widens")
  expect_true(all(diff(golden$pop_share_within) >= 0),
              info = "population share must not fall as the band widens")
  expect_true(all(diff(golden$n_zero_access) <= 0),
              info = "the number of zero-access tracts must not rise as the band widens")
})

test_that("E2E: fixture manifest is present, parseable and self-consistent", {
  skip_if_not_installed("yaml")
  m <- yaml::read_yaml(fixture_path("manifest.yml"))
  expect_true(is.numeric(m$fixture_version) || is.integer(m$fixture_version))
  expect_equal(as.integer(m$generator_seed), CANON_SEED,
               label = "the manifest's seed must be the seed the tests use")
  expect_true(nzchar(m$description))
  expect_true(isTRUE(m$sources[[1]]$redistributable),
              label = "every fixture source must be redistributable in a public repo")
  expect_true(grepl("regenerate-fixtures", m$regenerate))
})
