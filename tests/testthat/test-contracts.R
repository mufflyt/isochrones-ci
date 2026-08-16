# Fixture and harness contracts. Cheap, and they fail fast when the ground
# under the rest of the suite has moved.

test_that("every committed fixture file exists and is non-empty", {
  for (rel in c("manifest.yml", "inputs/providers.csv", "inputs/tracts.csv",
                "inputs/travel_times.csv", "expected/golden_by_band.csv",
                "expected/golden_tracts_60.csv")) {
    p <- fixture_path(rel)
    expect_true(file.exists(p), label = paste("fixture present:", rel))
    expect_gt(file.info(p)$size, 0, label = paste("fixture non-empty:", rel))
  }
})

test_that("fixtures stay SMALL -- this repository must remain quick to understand", {
  # A hard ceiling, not a guideline. The moment fixtures grow past this, failures
  # stop being reproducible in your head and this stops being a harness.
  files <- list.files(fixture_path(), recursive = TRUE, full.names = TRUE)
  total <- sum(file.info(files)$size)
  expect_lt(total, 1e6,
            label = sprintf("total fixture bytes (%d) must stay under 1 MB", total))
  biggest <- files[which.max(file.info(files)$size)]
  expect_lt(max(file.info(files)$size), 5e5,
            label = paste("largest fixture is", basename(biggest)))
})

test_that("fixture inputs have the columns the harness relies on", {
  p <- read_fixture("inputs/providers.csv")
  t1 <- read_fixture("inputs/tracts.csv")
  tt <- read_fixture("inputs/travel_times.csv")
  expect_true(all(c("provider_id", "lon", "lat", "supply") %in% names(p)))
  expect_true(all(c("tract_id", "lon", "lat", "population") %in% names(t1)))
  expect_true(all(c("provider_id", "tract_id", "minutes") %in% names(tt)))
  expect_false(anyDuplicated(p$provider_id) > 0L)
  expect_false(anyDuplicated(t1$tract_id) > 0L)
  expect_equal(nrow(tt), nrow(p) * nrow(t1),
               label = "the travel-time table must be complete: every provider x tract pair")
})

test_that("fixture values are in plausible ranges", {
  p <- read_fixture("inputs/providers.csv")
  t1 <- read_fixture("inputs/tracts.csv")
  tt <- read_fixture("inputs/travel_times.csv")
  expect_true(all(t1$population >= 0), label = "population cannot be negative")
  expect_true(all(p$supply > 0), label = "a provider with no supply is not a provider")
  expect_true(all(tt$minutes >= 0), label = "travel time cannot be negative")
  expect_false(any(is.na(tt$minutes)), label = "a missing travel time must be absent, not NA")
  expect_true(all(t1$lat > 0 & t1$lat < 90))
  expect_true(all(t1$lon < 0 & t1$lon > -180), label = "CONUS longitudes are negative")
})

test_that("the fixture contains the cases it claims to contain", {
  # The manifest advertises specific pathologies. If a regeneration silently
  # dropped one, every test that believes it is exercising that case would be
  # exercising nothing.
  t1 <- read_fixture("inputs/tracts.csv")
  p <- read_fixture("inputs/providers.csv")
  tt <- read_fixture("inputs/travel_times.csv")
  expect_true(any(t1$population == 0), label = "a zero-population tract")
  expect_true(anyDuplicated(p[, c("lon", "lat")]) > 0L,
              label = "two providers at identical coordinates")
  far <- tapply(tt$minutes, tt$provider_id, min)
  expect_true(any(far > 180),
              label = "an isolated provider unreachable within every canonical band")
})

test_that("NO REAL-WORLD IDENTIFIERS leak into a public fixture", {
  # A synthetic identifier must be recognisably synthetic. These start with 9
  # and are generated, but the check is on the PROPERTY rather than the
  # generator, so a future hand-edit that pasted a real NPI would be caught.
  p <- read_fixture("inputs/providers.csv")
  expect_true(all(grepl("^9[0-9]{9}$", p$synth_id)),
              info = paste0("provider identifiers must stay in the synthetic 9-prefixed ",
                            "range; anything else risks publishing a real NPI"))
  expect_false(any(grepl("[A-Za-z]", paste(p$provider_id, collapse = "")) &
                     grepl("MD|DO|Dr", p$provider_id)),
               label = "no name-like content in identifiers")
  expect_false("name" %in% tolower(names(p)),
               label = "a public fixture must carry no name column")
})

test_that("harness tolerances are documented constants, not magic numbers", {
  expect_true(is.numeric(TOL_RELATIVE) && TOL_RELATIVE > 0 && TOL_RELATIVE < 1e-6)
  expect_equal(TOL_POPULATION, 0,
               label = "populations are integers upstream; any disagreement is real")
  expect_true(TOL_GEOMETRY_FRACTION > 0 && TOL_GEOMETRY_FRACTION < 1e-3)
})
