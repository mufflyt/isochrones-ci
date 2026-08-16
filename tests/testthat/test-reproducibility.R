# Determinism. Same seed, same inputs, same answer -- byte for byte.

test_that("the world generator is a pure function of its seed", {
  a <- make_world(seed = 4242L, n_providers = 6L, n_tracts = 20L)
  b <- make_world(seed = 4242L, n_providers = 6L, n_tracts = 20L)
  expect_identical(a$providers, b$providers)
  expect_identical(a$tracts, b$tracts)
  expect_identical(a$tt, b$tt)
})

test_that("different seeds give different worlds (the generator is not constant)", {
  # Without this, "reproducible" could be satisfied by a generator that ignores
  # its seed entirely and returns the same thing forever.
  a <- make_world(seed = 1L, n_providers = 6L, n_tracts = 20L)
  b <- make_world(seed = 2L, n_providers = 6L, n_tracts = 20L)
  expect_false(identical(a$tracts$population, b$tracts$population))
})

test_that("the generator does not perturb the caller's RNG stream", {
  # A generator that leaves the global seed moved makes every downstream
  # "reproducible" claim false in a way that only shows up when test order
  # changes.
  set.seed(999L)
  before <- .Random.seed
  invisible(make_world(seed = 7L, n_providers = 4L, n_tracts = 10L))
  expect_identical(.Random.seed, before)
})

test_that("repeated computation on identical input is bit-identical", {
  w <- canon_world()
  runs <- replicate(5, ref_2sfca(w$tt, w$providers, w$tracts, 60)$tract_access,
                    simplify = FALSE)
  for (i in 2:5) expect_identical(runs[[i]], runs[[1]])
})

test_that("the committed fixtures still reproduce from the recorded seed", {
  # This is the link between the manifest and the data. If someone hand-edits a
  # fixture CSV, the generator and the file diverge and this fails -- which is
  # the point, because a hand-edited fixture is no longer reproducible.
  w <- canon_world()
  disk_p <- read_fixture("inputs/providers.csv")
  disk_t <- read_fixture("inputs/tracts.csv")
  expect_equal(nrow(disk_p), nrow(w$providers))
  expect_equal(nrow(disk_t), nrow(w$tracts))
  expect_equal(disk_p$provider_id, w$providers$provider_id)
  expect_equal(disk_t$tract_id, w$tracts$tract_id)
  expect_equal(disk_t$population, w$tracts$population)
  expect_equal(disk_p$supply, w$providers$supply)
  expect_equal(disk_p$lon, w$providers$lon, tolerance = 1e-12)
})
