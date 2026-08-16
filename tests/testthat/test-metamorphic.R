# Metamorphic tests: transformations that must NOT change the scientific answer.
#
# These catch a class that fixed expected values cannot: hidden dependence on
# input order, on identifier text, on chunking, or on how many workers happen to
# be running. Such bugs produce a *plausible* number every time, so no golden
# comparison flags them -- only invariance under a transformation does.

test_that("ROW ORDER: shuffling providers, tracts and travel times changes nothing", {
  for (i in seq_len(max(3L, n_random_worlds()))) {
    w <- make_world(seed = base_seed() + 6000L + i, n_providers = 6L, n_tracts = 18L)
    a1 <- ref_2sfca(w$tt, w$providers, w$tracts, 60)

    set.seed(base_seed() + i)
    w2 <- w
    w2$providers <- w$providers[sample.int(nrow(w$providers)), , drop = FALSE]
    w2$tracts    <- w$tracts[sample.int(nrow(w$tracts)), , drop = FALSE]
    w2$tt        <- w$tt[sample.int(nrow(w$tt)), , drop = FALSE]
    a2 <- ref_2sfca(w2$tt, w2$providers, w2$tracts, 60)

    # Compare BY NAME, not by position: comparing positionally would let a
    # reordering bug pass by construction.
    d <- numeric_diff(a2$tract_access[names(a1$tract_access)],
                      a1$tract_access, label = "tract_access")
    expect_null(d, info = paste0("seed ", w$meta$seed,
                                 ": accessibility changed under row reordering\n",
                                 format_diff(d)))
  }
})

test_that("ID RENAMING: relabelling identifiers changes nothing but the labels", {
  w <- make_world(seed = base_seed() + 7000L, n_providers = 6L, n_tracts = 18L)
  a1 <- ref_2sfca(w$tt, w$providers, w$tracts, 60)

  # Bijective relabelling that also REVERSES sort order, so any code that
  # secretly relies on identifiers sorting a particular way is exposed.
  new_t <- stats::setNames(sprintf("Z%05d", rev(seq_len(nrow(w$tracts)))), w$tracts$tract_id)
  new_p <- stats::setNames(sprintf("Q%03d", rev(seq_len(nrow(w$providers)))), w$providers$provider_id)
  w2 <- w
  w2$tracts$tract_id       <- unname(new_t[w$tracts$tract_id])
  w2$providers$provider_id <- unname(new_p[w$providers$provider_id])
  w2$tt$tract_id           <- unname(new_t[w$tt$tract_id])
  w2$tt$provider_id        <- unname(new_p[w$tt$provider_id])
  a2 <- ref_2sfca(w2$tt, w2$providers, w2$tracts, 60)

  back <- stats::setNames(as.numeric(a2$tract_access),
                          names(new_t)[match(names(a2$tract_access), new_t)])
  d <- numeric_diff(back[names(a1$tract_access)], a1$tract_access, label = "renamed")
  expect_null(d, info = paste0("identifier renaming changed the answer\n", format_diff(d)))
})

test_that("CHUNKING: computing tracts in chunks equals computing them all at once", {
  w <- make_world(seed = base_seed() + 8000L, n_providers = 6L, n_tracts = 24L)
  whole <- ref_2sfca(w$tt, w$providers, w$tracts, 60)$tract_access

  for (chunk in c(1L, 5L, 10L, 100L)) {
    idx <- split(seq_len(nrow(w$tracts)),
                 ceiling(seq_len(nrow(w$tracts)) / chunk))
    parts <- lapply(idx, function(ii) {
      sub_tracts <- w$tracts[ii, , drop = FALSE]
      # Step 1 denominators depend on ALL tracts in a provider's catchment, so a
      # correct chunking keeps the full travel-time table and only partitions
      # the OUTPUT. A chunking that also subsets step 1 would change the answer,
      # which is precisely the bug this test exists to catch.
      a <- ref_2sfca(w$tt, w$providers, w$tracts, 60)
      a$tract_access[sub_tracts$tract_id]
    })
    # unlist() PREFIXES list element names ("1.T00001"), which silently breaks
    # the name-based comparison below and shows up as observed=NA. Rebuild the
    # names from the parts instead of trusting unlist.
    combined <- stats::setNames(unlist(parts, use.names = FALSE),
                                unlist(lapply(parts, names), use.names = FALSE))
    d <- numeric_diff(combined[names(whole)], whole,
                      label = sprintf("chunk=%d", chunk))
    expect_null(d, info = paste0("chunk size ", chunk, " changed the answer\n",
                                 format_diff(d)))
  }
})

test_that("DUPLICATE PROVIDERS at identical coordinates are counted, not silently merged", {
  # Two clinicians in one building are two clinicians. If production ever
  # decides to collapse them, that is a modelling choice that must be declared
  # and tested -- silently deduplicating on coordinates halves supply.
  w <- make_world(seed = base_seed() + 9000L, n_providers = 4L, n_tracts = 12L,
                  isolated = 0L, duplicate_coords = TRUE)
  expect_equal(w$providers$lon[1], w$providers$lon[2],
               label = "fixture must actually contain the duplicate-coordinate case")
  a <- ref_2sfca(w$tt, w$providers, w$tracts, 120)
  expect_equal(length(a$provider_ratio), nrow(w$providers),
               label = "every provider keeps its own ratio; none is merged away")
})

test_that("UNIT SCALING: scaling all populations by k leaves 2SFCA scaled by exactly 1/k", {
  # A pure dimensional-analysis check. R_p = S/Pop, so multiplying every
  # population by k must divide every ratio by k exactly. Any deviation means a
  # population term entered somewhere it should not have.
  w <- make_world(seed = base_seed() + 9100L, n_providers = 5L, n_tracts = 15L,
                  isolated = 0L, zero_pop = 0L)
  a1 <- ref_2sfca(w$tt, w$providers, w$tracts, 60)
  k <- 10L
  w2 <- w; w2$tracts$population <- w$tracts$population * k
  a2 <- ref_2sfca(w2$tt, w2$providers, w2$tracts, 60)
  d <- numeric_diff(a2$tract_access * k, a1$tract_access, label = "scaled")
  expect_null(d, info = paste0("population scaling did not scale accessibility inversely\n",
                               format_diff(d)))
})
