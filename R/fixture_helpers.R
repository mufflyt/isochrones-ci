# Deterministic synthetic test universes.
#
# EVERYTHING HERE IS SYNTHETIC. No real physician, no real NPI, no real address,
# no licensed dataset. The geography is a stylised grid placed at Colorado-ish
# coordinates so that projected distances are realistic, but no tract in it is a
# real census tract and no provider in it is a real person. That is a hard
# requirement of this repository being public, and it is also good testing
# practice: a fixture you can regenerate from a seed is a fixture whose failures
# reproduce.
#
# The generators are pure functions of (seed, size), so a failing randomized case
# is fully described by its seed.

# ---------------------------------------------------------------- constants ---

# A stylised Colorado-ish bounding box (lon/lat, EPSG:4326). Chosen because the
# production study is CONUS and Colorado spans metro Front Range and genuinely
# remote rural counties, which is the contrast the accessibility metric exists to
# measure. The numbers are round on purpose: nothing here is a real boundary.
CO_BBOX <- c(xmin = -109.0, ymin = 37.0, xmax = -102.0, ymax = 41.0)

# Projected CRS for area/distance work in this region. EPSG:5070 (CONUS Albers
# Equal Area) is the right family for area-preserving CONUS work; using a
# geographic CRS for area is one of the mistakes test-geography.R checks for.
FIXTURE_CRS_GEOGRAPHIC <- 4326L
FIXTURE_CRS_PROJECTED  <- 5070L

# Drive-time bands, mirroring the production contract. The harness does NOT
# hardcode a belief about these; test-production-contract.R asserts that this
# list still equals mufflyaccess::CANONICAL_BANDS, so a production change to the
# band set fails here loudly instead of being silently ignored.
FIXTURE_BANDS_MIN <- c(30L, 60L, 120L, 180L)

# --------------------------------------------------------------- generators ---

#' Build a deterministic synthetic world
#'
#' @description
#' Returns a list of plain data frames describing providers, tracts, and a
#' travel-time matrix between them. No geometry: geometry is added separately by
#' `make_fixture_geometry()`, so tests that do not need sf can run without it.
#'
#' The travel-time model is intentionally crude and TRANSPARENT: great-circle
#' distance divided by an average speed, with a per-pair roughness factor. It is
#' NOT a routing engine and does not pretend to be. Its only job is to produce a
#' travel-time matrix with realistic structure (near pairs fast, far pairs slow,
#' some noise) so that invariants have something meaningful to hold over.
#'
#' @param seed Integer seed. A failing randomized case is reproduced from this.
#' @param n_providers Number of providers.
#' @param n_tracts Number of tracts.
#' @param isolated Number of providers placed far from every tract (tests the
#'   "provider nobody can reach" case).
#' @param zero_pop Number of tracts given population 0 (tests zero semantics,
#'   which must not collapse into NA).
#' @param duplicate_coords If TRUE, force two providers onto identical
#'   coordinates (exact-tie case).
#' @return A list with `providers`, `tracts`, `tt` (long travel-time frame),
#'   and `meta`.
make_world <- function(seed = 1L,
                       n_providers = 8L,
                       n_tracts = 25L,
                       isolated = 1L,
                       zero_pop = 2L,
                       duplicate_coords = FALSE) {
  stopifnot(n_providers >= 1L, n_tracts >= 1L,
            isolated <= n_providers, zero_pop <= n_tracts)

  # withr-free, dependency-free determinism: save and restore the RNG so calling
  # a generator never perturbs a caller's stream. A generator with a side effect
  # on global RNG state makes "same seed reproduces" quietly false.
  old_seed <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL
  on.exit({
    if (is.null(old_seed)) {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)

  lon <- function(n) runif(n, CO_BBOX[["xmin"]], CO_BBOX[["xmax"]])
  lat <- function(n) runif(n, CO_BBOX[["ymin"]], CO_BBOX[["ymax"]])

  providers <- data.frame(
    provider_id = sprintf("P%03d", seq_len(n_providers)),
    # A synthetic 10-digit identifier. Deliberately NOT a real NPI: the range is
    # chosen so these cannot collide with issued NPIs, and canon_npi tests use
    # explicitly constructed strings rather than these.
    synth_id    = sprintf("9%09d", seq_len(n_providers)),
    lon         = lon(n_providers),
    lat         = lat(n_providers),
    # Supply in arbitrary "clinician-equivalents". Integer so conservation
    # checks are exact rather than floating-point-approximate.
    supply      = sample(1:5, n_providers, replace = TRUE),
    stringsAsFactors = FALSE
  )

  if (isolated > 0L) {
    # Push the last `isolated` providers into the far southwest corner, well
    # away from where tracts are drawn. These must end up unreachable.
    idx <- seq.int(n_providers - isolated + 1L, n_providers)
    providers$lon[idx] <- CO_BBOX[["xmin"]] - 6
    providers$lat[idx] <- CO_BBOX[["ymin"]] - 4
  }

  if (duplicate_coords && n_providers >= 2L) {
    providers$lon[2L] <- providers$lon[1L]
    providers$lat[2L] <- providers$lat[1L]
  }

  tracts <- data.frame(
    tract_id   = sprintf("T%05d", seq_len(n_tracts)),
    lon        = lon(n_tracts),
    lat        = lat(n_tracts),
    population = as.integer(round(runif(n_tracts, 200, 9000))),
    stringsAsFactors = FALSE
  )
  if (zero_pop > 0L) {
    tracts$population[seq_len(zero_pop)] <- 0L
  }

  tt <- .travel_times(providers, tracts, seed = seed)

  list(
    providers = providers,
    tracts    = tracts,
    tt        = tt,
    meta      = list(seed = seed, n_providers = n_providers,
                     n_tracts = n_tracts, isolated = isolated,
                     zero_pop = zero_pop,
                     duplicate_coords = duplicate_coords,
                     generator_version = 1L)
  )
}

#' Great-circle distance in km (haversine)
#'
#' Written out rather than pulled from a package so the reference side of this
#' harness depends on as little as possible. Earth radius 6371.0088 km (mean).
haversine_km <- function(lon1, lat1, lon2, lat2) {
  r <- 6371.0088
  to_rad <- pi / 180
  dlon <- (lon2 - lon1) * to_rad
  dlat <- (lat2 - lat1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

# Travel time model: distance / speed, inflated by a deterministic per-pair
# roughness in [1.0, 1.6] standing in for road circuity. Crude and honest.
.travel_times <- function(providers, tracts, seed = 1L, kph = 70) {
  old_seed <- if (exists(".Random.seed", envir = globalenv())) {
    get(".Random.seed", envir = globalenv())
  } else NULL
  on.exit({
    if (is.null(old_seed)) {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed + 99L)

  grid <- expand.grid(provider_id = providers$provider_id,
                      tract_id = tracts$tract_id,
                      stringsAsFactors = FALSE)
  pi_ <- match(grid$provider_id, providers$provider_id)
  ti_ <- match(grid$tract_id, tracts$tract_id)

  km <- haversine_km(providers$lon[pi_], providers$lat[pi_],
                     tracts$lon[ti_], tracts$lat[ti_])
  circuity <- runif(nrow(grid), 1.0, 1.6)
  grid$minutes <- (km * circuity) / kph * 60
  grid$km <- km
  grid[order(grid$provider_id, grid$tract_id), , drop = FALSE]
}

#' Attach simple square polygons to tracts, for the geometry tests
#'
#' @description
#' Each tract becomes a small axis-aligned square centred on its point. Squares
#' are trivially valid, which is the point: the ADVERSARIAL geometry cases are
#' constructed deliberately in test-adversarial.R rather than emerging by
#' accident here, so a geometry failure is always attributable.
#'
#' Requires sf. Callers that cannot load sf must fail, not skip -- see
#' `require_sf()` in validation_helpers.R.
make_fixture_geometry <- function(world, half_side_deg = 0.08) {
  require_sf()
  polys <- lapply(seq_len(nrow(world$tracts)), function(i) {
    x <- world$tracts$lon[i]; y <- world$tracts$lat[i]
    sf::st_polygon(list(cbind(
      c(x - half_side_deg, x + half_side_deg, x + half_side_deg,
        x - half_side_deg, x - half_side_deg),
      c(y - half_side_deg, y - half_side_deg, y + half_side_deg,
        y + half_side_deg, y - half_side_deg))))
  })
  sf::st_sf(
    tract_id   = world$tracts$tract_id,
    population = world$tracts$population,
    geometry   = sf::st_sfc(polys, crs = FIXTURE_CRS_GEOGRAPHIC)
  )
}

#' Circular "isochrone" bands around a provider, in projected space
#'
#' @description
#' Nested discs whose radii scale with the band minutes. These are NOT routed
#' isochrones and must never be described as such -- they exist so that band
#' CONTAINMENT (30 within 60 within 120) is exactly true by construction, which
#' makes them a valid oracle for a containment test: if the containment check
#' cannot confirm a relationship that holds by construction, the CHECK is
#' broken. test-adversarial.R separately supplies non-nested shapes to prove the
#' check can also fail.
make_band_discs <- function(lon, lat, bands_min = FIXTURE_BANDS_MIN, kph = 70) {
  require_sf()
  pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = FIXTURE_CRS_GEOGRAPHIC)
  pt <- sf::st_transform(pt, FIXTURE_CRS_PROJECTED)
  radii_m <- (bands_min / 60) * kph * 1000
  geoms <- lapply(radii_m, function(r) sf::st_buffer(pt, r)[[1]])
  sf::st_sf(band_min = bands_min,
            geometry = sf::st_sfc(geoms, crs = FIXTURE_CRS_PROJECTED))
}
