# Geometry and CRS claims.
#
# sf is REQUIRED, not optional: require_sf() fails rather than skips. A green
# run with the geometry tests quietly skipped would assert something this suite
# did not check, which is the failure mode the whole harness exists to prevent.

test_that("fixture geometry is valid, non-empty, and carries a known CRS", {
  require_sf()
  g <- make_fixture_geometry(canon_world())
  expect_true(all(sf::st_is_valid(g)), label = "every tract polygon is valid")
  expect_false(any(sf::st_is_empty(g)), label = "no empty geometry")
  expect_false(is.na(sf::st_crs(g)), label = "CRS must be known, never NA")
  expect_equal(sf::st_crs(g)$epsg, FIXTURE_CRS_GEOGRAPHIC)
})

test_that("identifiers stay attached to their own geometry through a transform", {
  # Reprojecting is a common place for a join to silently reorder rows. The
  # check is positional identity of the ID column plus a geometric one: each
  # transformed polygon must still contain its own tract's point.
  require_sf()
  w <- canon_world()
  g <- make_fixture_geometry(w)
  gp <- sf::st_transform(g, FIXTURE_CRS_PROJECTED)
  expect_identical(g$tract_id, gp$tract_id)
  expect_identical(g$population, gp$population)

  pts <- sf::st_as_sf(w$tracts, coords = c("lon", "lat"), crs = FIXTURE_CRS_GEOGRAPHIC)
  pts <- sf::st_transform(pts, FIXTURE_CRS_PROJECTED)
  pts <- pts[match(gp$tract_id, pts$tract_id), ]
  inside <- as.logical(sf::st_intersects(pts, gp, sparse = FALSE)[cbind(seq_len(nrow(pts)),
                                                                        seq_len(nrow(gp)))])
  expect_true(all(inside),
              info = "a tract's own centre point fell outside its own polygon after reprojection")
})

test_that("coordinate order is not silently reversed", {
  # lon/lat swapped is the classic GIS error: it produces geometry that is still
  # 'valid' and lands in the Indian Ocean. Colorado's longitude is negative and
  # its latitude positive, so the sign pattern alone detects the swap.
  require_sf()
  g <- make_fixture_geometry(canon_world())
  bb <- sf::st_bbox(g)
  expect_lt(bb[["xmax"]], 0, label = "longitudes in CONUS are negative")
  expect_gt(bb[["ymin"]], 0, label = "latitudes in CONUS are positive")
  expect_gt(bb[["xmin"]], -180); expect_lt(bb[["ymax"]], 90)
})

test_that("area is computed in a projected CRS, not a geographic one", {
  # Areas taken in degrees are meaningless. An equal-area projection of a
  # ~0.16 x 0.16 degree square at 39N should be on the order of 250 km^2; in
  # degrees the same square is 0.0256, which is nine orders of magnitude away.
  require_sf()
  g <- make_fixture_geometry(canon_world())
  a <- as.numeric(sf::st_area(sf::st_transform(g, FIXTURE_CRS_PROJECTED)))
  expect_true(all(a > 1e7 & a < 1e9),
              info = paste0("projected tract areas are implausible (m^2): ",
                            paste(format(range(a), digits = 4), collapse = " to "),
                            " -- is the CRS wrong?"))
})

test_that("BAND CONTAINMENT: 30 within 60 within 120 within 180", {
  # The discs are nested by construction, so this is an oracle for the CHECK:
  # if band_contains() cannot confirm a containment that is true by definition,
  # the checker is broken. test-adversarial.R proves it can also report FALSE.
  require_sf()
  w <- canon_world()
  discs <- make_band_discs(w$providers$lon[1], w$providers$lat[1])
  for (i in seq_len(nrow(discs) - 1L)) {
    expect_true(band_contains(discs[i, ], discs[i + 1L, ], TOL_GEOMETRY_FRACTION),
                label = sprintf("%d-minute band within %d-minute band",
                                discs$band_min[i], discs$band_min[i + 1L]))
  }
})

test_that("band areas are strictly increasing with drive time", {
  require_sf()
  w <- canon_world()
  discs <- make_band_discs(w$providers$lon[1], w$providers$lat[1])
  a <- as.numeric(sf::st_area(discs))
  expect_true(all(diff(a) > 0),
              info = paste0("band areas are not increasing: ",
                            paste(format(a, digits = 4), collapse = ", ")))
})
