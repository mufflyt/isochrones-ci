# The adversarial corpus: tiny pathological worlds, kept permanently.
#
# Every one of these is a shape that has broken accessibility code somewhere.
# They are deliberately minimal so that when one fails you can hold the whole
# input in your head. New entries are added here whenever a real bug is found --
# see CONTRIBUTING.md.

tiny <- function(providers, tracts, minutes) {
  tt <- expand.grid(provider_id = providers$provider_id,
                    tract_id = tracts$tract_id, stringsAsFactors = FALSE)
  tt$minutes <- minutes
  list(providers = providers, tracts = tracts, tt = tt)
}

test_that("ADVERSARIAL: provider exactly AT the threshold is inside it", {
  # The single most consequential boundary in the system. `<` instead of `<=`
  # silently drops every tract sitting exactly on 30 or 60 minutes.
  p <- data.frame(provider_id = "P1", supply = 1, stringsAsFactors = FALSE)
  t1 <- data.frame(tract_id = "T1", population = 100L, stringsAsFactors = FALSE)
  w <- tiny(p, t1, minutes = 60)
  expect_true(unname(ref_reached(w$tt, 60)), label = "exactly 60 minutes is within 60 minutes")
  expect_false(unname(ref_reached(w$tt, 59.999)))
})

test_that("ADVERSARIAL: no provider reachable gives 0 access and 0 coverage, not NA", {
  p <- data.frame(provider_id = "P1", supply = 3, stringsAsFactors = FALSE)
  t1 <- data.frame(tract_id = "T1", population = 500L, stringsAsFactors = FALSE)
  w <- tiny(p, t1, minutes = 999)
  a <- ref_2sfca(w$tt, w$providers, w$tracts, 60)
  expect_equal(unname(a$tract_access[["T1"]]), 0)
  expect_equal(ref_pop_share_within(w$tt, w$tracts, 60), 0)
})

test_that("ADVERSARIAL: provider whose catchment holds zero population has NA ratio, not Inf", {
  p <- data.frame(provider_id = "P1", supply = 3, stringsAsFactors = FALSE)
  t1 <- data.frame(tract_id = "T1", population = 0L, stringsAsFactors = FALSE)
  w <- tiny(p, t1, minutes = 10)
  a <- ref_2sfca(w$tt, w$providers, w$tracts, 60)
  expect_true(is.na(a$provider_ratio[["P1"]]),
              label = "supply / 0 population is undefined, and Inf would poison every sum")
  expect_equal(unname(a$tract_access[["T1"]]), 0)
})

test_that("ADVERSARIAL: two providers at identical coordinates both count", {
  p <- data.frame(provider_id = c("P1", "P2"), supply = c(2, 3), stringsAsFactors = FALSE)
  t1 <- data.frame(tract_id = "T1", population = 100L, stringsAsFactors = FALSE)
  w <- tiny(p, t1, minutes = 10)
  a <- ref_2sfca(w$tt, w$providers, w$tracts, 60)
  expect_equal(unname(a$tract_access[["T1"]]), (2 / 100) + (3 / 100))
})

test_that("ADVERSARIAL: exact ties in travel time do not drop a provider", {
  p <- data.frame(provider_id = c("P1", "P2", "P3"), supply = 1, stringsAsFactors = FALSE)
  t1 <- data.frame(tract_id = "T1", population = 300L, stringsAsFactors = FALSE)
  w <- tiny(p, t1, minutes = 42)          # all three identical
  a <- ref_2sfca(w$tt, w$providers, w$tracts, 60)
  expect_equal(unname(a$tract_access[["T1"]]), 3 * (1 / 300))
})

test_that("ADVERSARIAL: a single tract holding the entire population", {
  p <- data.frame(provider_id = "P1", supply = 1, stringsAsFactors = FALSE)
  t1 <- data.frame(tract_id = "T1", population = 1L, stringsAsFactors = FALSE)
  w <- tiny(p, t1, minutes = 5)
  expect_equal(ref_pop_share_within(w$tt, w$tracts, 60), 1)
})

test_that("ADVERSARIAL: an empty travel-time table reaches nothing without erroring", {
  tt <- data.frame(provider_id = character(0), tract_id = character(0),
                   minutes = numeric(0), stringsAsFactors = FALSE)
  expect_length(ref_reached(tt, 60), 0L)
})

test_that("ADVERSARIAL: extreme but valid travel times are handled, not overflowed", {
  p <- data.frame(provider_id = "P1", supply = 1, stringsAsFactors = FALSE)
  t1 <- data.frame(tract_id = "T1", population = 10L, stringsAsFactors = FALSE)
  w <- tiny(p, t1, minutes = 1e9)
  expect_false(unname(ref_reached(w$tt, 180)))
  w2 <- tiny(p, t1, minutes = 0)
  expect_true(unname(ref_reached(w2$tt, 30)), label = "zero travel time is reachable")
})

test_that("ADVERSARIAL geometry: an invalid self-intersecting polygon is detected", {
  require_sf()
  # Built in a PROJECTED CRS on purpose. With s2 enabled, validity and repair of
  # a degenerate polygon in lon/lat go through spherical geometry, where
  # st_make_valid() does not return a planar-valid result -- so testing repair
  # on a geographic bow-tie tests s2's edge cases rather than the claim. The
  # claim is about planar geometry, so the fixture is planar.
  bowtie <- sf::st_polygon(list(cbind(c(0, 1, 0, 1, 0), c(0, 1, 1, 0, 0))))
  g <- sf::st_sfc(bowtie, crs = FIXTURE_CRS_PROJECTED)
  expect_false(sf::st_is_valid(g), label = "a bow-tie must be reported invalid, not repaired silently")
  expect_true(all(sf::st_is_valid(sf::st_make_valid(g))),
              label = "and must be repairable when repair is the deliberate choice")
})

test_that("ADVERSARIAL geometry: empty geometry is distinguishable from a tiny one", {
  require_sf()
  empty <- sf::st_sfc(sf::st_polygon(), crs = FIXTURE_CRS_GEOGRAPHIC)
  speck <- sf::st_sfc(sf::st_polygon(list(cbind(c(0, 1e-9, 1e-9, 0, 0),
                                                c(0, 0, 1e-9, 1e-9, 0)))),
                      crs = FIXTURE_CRS_GEOGRAPHIC)
  expect_true(sf::st_is_empty(empty))
  expect_false(sf::st_is_empty(speck),
               label = "a 1e-9 degree polygon is tiny, not empty; collapsing the two loses data")
  expect_gt(as.numeric(sf::st_area(sf::st_transform(speck, FIXTURE_CRS_PROJECTED))), 0)
})

test_that("ADVERSARIAL geometry: multipart geometry survives a round trip", {
  require_sf()
  mp <- sf::st_multipolygon(list(
    list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))),
    list(cbind(c(3, 4, 4, 3, 3), c(3, 3, 4, 4, 3)))))
  g <- sf::st_sfc(mp, crs = FIXTURE_CRS_GEOGRAPHIC)
  expect_true(sf::st_is_valid(g))
  expect_equal(length(sf::st_cast(g, "POLYGON")), 2L,
               label = "both parts of a multipart tract must survive")
})

test_that("ADVERSARIAL geometry: containment check REPORTS FALSE for non-nested shapes", {
  # The negative control for band_contains(). Without this, the containment test
  # in test-geography.R could be a function that returns TRUE unconditionally.
  require_sf()
  a <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(0, 2, 2, 0, 0), c(0, 0, 2, 2, 0)))),
    crs = FIXTURE_CRS_PROJECTED))
  b <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(1, 3, 3, 1, 1), c(1, 1, 3, 3, 1)))),
    crs = FIXTURE_CRS_PROJECTED))
  expect_false(band_contains(a, b, TOL_GEOMETRY_FRACTION),
               label = "partially overlapping squares are NOT contained")
})

test_that("ADVERSARIAL identifiers: malformed inputs become NA rather than plausible ids", {
  bad <- c("", " ", "abc", "12345678901", "1.5e9", "-1234567890", NA_character_)
  out <- ref_canon_id(bad)
  expect_true(all(is.na(out)),
              info = paste0("a malformed identifier must be NA, never a plausible-looking id: ",
                            paste(ifelse(is.na(out), "NA", out), collapse = ", ")))
})
