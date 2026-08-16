# Contract tests against the production package.
#
# These assert the SHAPE of what production promises: band sets, scope sets,
# denominators. They are cheap and they fail the instant production changes a
# canonical constant, which is what makes this harness an early-warning system
# for the downstream repos rather than only a correctness checker.

test_that("CANONICAL_BANDS still matches what this harness's fixtures assume", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  expect_identical(as.integer(mufflyaccess::CANONICAL_BANDS), FIXTURE_BANDS_MIN,
                   info = paste0(
                     "The production band set changed. The fixtures in this repo were built ",
                     "for ", paste(FIXTURE_BANDS_MIN, collapse = "/"), " minutes. ",
                     "Regenerate them deliberately (scripts/regenerate-fixtures.R --accept) ",
                     "rather than loosening this test."))
  expect_identical(as.integer(mufflyaccess::get_canonical_bands()),
                   as.integer(mufflyaccess::CANONICAL_BANDS),
                   info = "the accessor and the constant must not drift apart")
})

test_that("the primary access band is a member of the canonical set", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  expect_true(mufflyaccess::PRIMARY_ACCESS_BAND_MIN %in% mufflyaccess::CANONICAL_BANDS,
              label = "headline band must be one the pipeline actually generates")
  expect_equal(as.numeric(mufflyaccess::PRIMARY_ACCESS_BAND_SEC),
               as.numeric(mufflyaccess::PRIMARY_ACCESS_BAND_MIN) * 60,
               label = "seconds and minutes forms must agree")
})

test_that("CONUS scope: the contiguous and non-contiguous sets partition cleanly", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  conus <- mufflyaccess::CONUS_STATE_ABBR
  noncon <- mufflyaccess::NON_CONTIGUOUS_CODES

  expect_equal(length(conus), 49L, label = "48 contiguous states + DC")
  expect_true("DC" %in% conus, label = "DC is in scope")
  expect_length(intersect(conus, noncon), 0L)
  expect_false(any(c("AK", "HI", "PR") %in% conus),
               label = "Alaska, Hawaii and Puerto Rico are out of scope (no road network)")
  expect_true(all(c("AK", "HI") %in% noncon))
  expect_false(anyDuplicated(conus) > 0L)
  expect_true(all(grepl("^[A-Z]{2}$", conus)))
})

test_that("census denominator vocabulary is well formed", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  d <- mufflyaccess::DENOMINATOR_CATEGORY
  expect_true(length(d) > 0L)
  expect_false(anyDuplicated(d) > 0L, label = "a duplicated category would double-count")
  expect_true(all(nzchar(d)))
})

test_that("published prevalence table matches the source publication's structure", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  tab <- mufflyaccess::WU2014_PFD_PREVALENCE
  expect_true(all(c("condition", "p_65_79", "p_80plus") %in% names(tab)))
  expect_true(nrow(tab) > 0L)
  # Prevalences are proportions. A table that has silently switched to
  # percentages reads 34 instead of 0.34 and quietly inflates every downstream
  # demand estimate by a factor of 100.
  for (col in c("p_65_79", "p_80plus")) {
    v <- tab[[col]]
    expect_true(all(v >= 0 & v <= 1, na.rm = TRUE),
                info = sprintf("%s is outside [0,1]: %s -- percentages, not proportions?",
                               col, paste(utils::head(v), collapse = ", ")))
  }
  # CORRECTED 2026-08-16. This test originally asserted that prevalence rises
  # with age for every condition. That is FALSE, and production was right:
  # in Wu et al. 2014 Table 1, pelvic organ prolapse DECLINES from 0.047 at
  # 65-79 to 0.040 at 80+. The harness was asserting a scientific claim the
  # source does not make, which would have produced a permanent false alarm.
  #
  # What IS a real constraint, and what is actually worth checking:
  #   (a) the transcribed values match the publication exactly;
  #   (b) "any PFD" is at least as common as each individual disorder, which is
  #       true by set inclusion and is the assertion a transcription error would
  #       violate.
  published <- data.frame(
    condition = c("any_PFD", "UI", "FI", "POP"),
    p_65_79   = c(0.368, 0.272, 0.154, 0.047),
    p_80plus  = c(0.497, 0.382, 0.210, 0.040),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(published))) {
    row <- tab[tab$condition == published$condition[i], ]
    expect_equal(nrow(row), 1L,
                 label = paste("condition present:", published$condition[i]))
    expect_equal(as.numeric(row$p_65_79), published$p_65_79[i], tolerance = 1e-12,
                 label = paste(published$condition[i], "65-79 (Wu 2014 Table 1)"))
    expect_equal(as.numeric(row$p_80plus), published$p_80plus[i], tolerance = 1e-12,
                 label = paste(published$condition[i], "80+ (Wu 2014 Table 1)"))
  }

  any_row <- tab[tab$condition == "any_PFD", ]
  others  <- tab[tab$condition != "any_PFD", ]
  expect_true(all(others$p_65_79 <= any_row$p_65_79),
              info = "no single disorder can be more common than 'any disorder'")
  expect_true(all(others$p_80plus <= any_row$p_80plus),
              info = "no single disorder can be more common than 'any disorder'")
})
