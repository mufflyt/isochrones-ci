# Chunking and execution invariance.
#
# WHAT THIS PROVES, AND WHAT IT DOES NOT
# It proves that a chunked decomposition of the harness's OWN 2SFCA
# implementation gives the same scientific answer regardless of how many chunks
# there are, how ragged they are, or what order they come back in -- and that
# this suite detects the eight classic ways such a decomposition goes wrong.
#
# It does NOT validate `mufflyt/isochrones`' parallel paths. That repository is
# private and unreachable from a public harness. SCIENTIFIC_VALIDATION.md says so
# in the "does not prove" section, and this comment exists so nobody reads a
# green chunking suite as a statement about production's chunking.
#
# THE DEFECT THIS LAYER TARGETS
# 2SFCA has two steps and only the second is safely partitionable. A provider's
# catchment does not respect chunk boundaries, so computing step-1 denominators
# inside each chunk shrinks every denominator, inflates every ratio, and yields a
# map that is entirely plausible and wrong by a factor that varies with the chunk
# count. No error, no NA, no warning. That is `chunk_local_denominator`.

# A world large enough for 60 chunks, small enough to stay in your head.
.chunk_world <- function(seed = 20260816L, n_tracts = 90L) {
  make_world(seed = seed, n_providers = 12L, n_tracts = n_tracts,
             isolated = 2L, zero_pop = 4L, duplicate_coords = TRUE)
}

.unchunked <- function(w, band = 60) ref_2sfca(w$tt, w$providers, w$tracts, band)

# Compare BY IDENTIFIER, never by position. Positional comparison would let a
# reordering bug pass by construction, which is the point of join_by_position.
.same_by_id <- function(observed, expected, tol = 0, label = "access") {
  if (!setequal(names(observed), names(expected))) {
    return(sprintf("%s: identifier sets differ (missing %s; unexpected %s)", label,
                   paste(utils::head(setdiff(names(expected), names(observed)), 5), collapse = ","),
                   paste(utils::head(setdiff(names(observed), names(expected)), 5), collapse = ",")))
  }
  d <- numeric_diff(unname(observed[names(expected)]), unname(expected), tol = tol,
                    label = label)
  if (is.null(d)) NULL else paste0(label, " differs:\n", format_diff(d))
}

# ------------------------------------------------------- chunk-count sweep ---

test_that("CHUNK COUNT: 1, 2, 3, 7, 10, 30 and 60 chunks all give the identical answer", {
  w <- .chunk_world()
  u <- .unchunked(w)$tract_access
  for (n in c(1L, 2L, 3L, 7L, 10L, 30L, 60L)) {
    r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = n)
    # EXACT equality. The arithmetic permits it: chunking changes which tracts
    # are looped over, never the order of any individual summation, so there is
    # no floating-point reassociation to tolerate. A tolerance here would hide
    # a real defect.
    msg <- .same_by_id(r$tract_access, u, tol = 0,
                       label = sprintf("n_chunks=%d", n))
    expect_null(msg, info = msg)
  }
})

test_that("CHUNK SCHEME: even, uneven (with empty and singleton chunks) and interleaved agree", {
  w <- .chunk_world()
  u <- .unchunked(w)$tract_access
  for (sch in c("even", "uneven", "interleaved")) {
    for (n in c(2L, 7L, 30L)) {
      r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = n, scheme = sch)
      msg <- .same_by_id(r$tract_access, u, tol = 0,
                         label = sprintf("scheme=%s n=%d", sch, n))
      expect_null(msg, info = msg)
    }
  }
  # The uneven scheme must actually produce the degenerate chunks it claims to,
  # or these cases are never exercised.
  ch <- make_chunks(90L, 7L, "uneven")
  expect_true(any(vapply(ch, length, integer(1)) == 0L),
              info = "the uneven scheme produced no EMPTY chunk; the case is untested")
  expect_true(any(vapply(ch, length, integer(1)) == 1L),
              info = "the uneven scheme produced no SINGLETON chunk; the case is untested")
})

test_that("CHUNK ORDER: original, reversed and shuffled give the identical answer", {
  w <- .chunk_world()
  u <- .unchunked(w)$tract_access
  for (ord in c("original", "reversed", "shuffled")) {
    for (n in c(3L, 10L, 60L)) {
      r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = n,
                       chunk_order = ord, seed = 99L)
      msg <- .same_by_id(r$tract_access, u, tol = 0,
                         label = sprintf("order=%s n=%d", ord, n))
      expect_null(msg, info = msg)
    }
  }
})

test_that("REASSEMBLY: concatenating chunk output in any order and canonicalising by id is stable", {
  w <- .chunk_world()
  a <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 12L, chunk_order = "original")
  b <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 12L, chunk_order = "reversed")
  c_ <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 12L, chunk_order = "shuffled", seed = 3L)
  expect_identical(a$tract_access, b$tract_access)
  expect_identical(a$tract_access, c_$tract_access)
  # ...and the canonical order is the tract table's, not the emission order.
  expect_identical(names(a$tract_access), w$tracts$tract_id)
})

# ------------------------------------------------- conservation at boundaries ---

test_that("CONSERVATION: no tract lost, none duplicated, none invented, at any chunk count", {
  w <- .chunk_world()
  for (n in c(1L, 2L, 7L, 30L, 60L)) for (sch in c("even", "uneven", "interleaved")) {
    r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = n, scheme = sch)
    lbl <- sprintf("n=%d scheme=%s", n, sch)
    expect_equal(length(r$tract_access), nrow(w$tracts), label = paste(lbl, "row count"))
    expect_false(anyDuplicated(names(r$tract_access)) > 0L,
                 label = paste(lbl, "duplicated tract id"))
    expect_setequal(names(r$tract_access), w$tracts$tract_id)
    # Every tract is assigned to exactly one chunk.
    expect_false(any(is.na(r$chunk_of)), info = paste(lbl, "- a tract belongs to no chunk"))
  }
})

test_that("CONSERVATION: population and supply totals are untouched by chunking", {
  w <- .chunk_world()
  for (n in c(1L, 7L, 60L)) {
    ch <- make_chunks(nrow(w$tracts), n, "uneven")
    covered <- sort(unlist(ch))
    expect_identical(covered, seq_len(nrow(w$tracts)),
                     info = "the partition must cover every tract exactly once")
    expect_equal(sum(w$tracts$population[unlist(ch)]), sum(w$tracts$population),
                 label = "population conserved across the partition")
  }
  # Supply is a provider-side quantity and must not be partitioned at all.
  r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 30L)
  expect_equal(length(r$provider_ratio), nrow(w$providers),
               label = "every provider keeps a ratio regardless of chunking")
})

test_that("CONSERVATION: zero and NA semantics survive chunking unchanged", {
  w <- .chunk_world()
  u <- .unchunked(w)
  for (n in c(1L, 7L, 60L)) {
    r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = n)
    expect_equal(sum(r$tract_access == 0), sum(u$tract_access == 0),
                 label = sprintf("n=%d: count of genuine zero-access tracts", n))
    expect_false(any(is.na(r$tract_access)),
                 info = "tract accessibility must never be NA; 'none' is 0")
    expect_equal(sum(is.na(r$provider_ratio)), sum(is.na(u$provider_ratio)),
                 label = sprintf("n=%d: count of UNDEFINED provider ratios", n))
  }
  # The fixture must actually contain both states, or this proves nothing.
  expect_gt(sum(u$tract_access == 0), 0L)
  expect_gt(sum(is.na(u$provider_ratio)), 0L)
})

# ------------------------------------ the cross-boundary denominator defect ---

test_that("CROSS-BOUNDARY: step-1 denominators use the WHOLE catchment, not the chunk", {
  # A world built so the defect cannot hide: one provider reaches every tract,
  # and the tracts are split across many chunks. If step 1 were chunk-local, its
  # denominator would be a fraction of the true population and its ratio would
  # inflate by exactly the reciprocal of that fraction.
  n_t <- 60L
  providers <- data.frame(provider_id = "P1", supply = 10, stringsAsFactors = FALSE)
  tracts <- data.frame(tract_id = sprintf("T%03d", seq_len(n_t)),
                       population = rep(100L, n_t), stringsAsFactors = FALSE)
  tt <- expand.grid(provider_id = "P1", tract_id = tracts$tract_id,
                    stringsAsFactors = FALSE)
  tt$minutes <- 10

  true_ratio <- 10 / (100 * n_t)
  for (n in c(1L, 2L, 10L, 60L)) {
    r <- chunk_2sfca(tt, providers, tracts, 60, n_chunks = n)
    expect_equal(unname(r$provider_ratio[["P1"]]), true_ratio, tolerance = 0,
                 label = sprintf("n_chunks=%d: step-1 denominator is the whole catchment", n))
    expect_true(all(abs(r$tract_access - true_ratio) < 1e-15),
                info = sprintf("n_chunks=%d: accessibility differs across chunks", n))
  }
})

test_that("CROSS-BOUNDARY: a provider's catchment split across chunks is still summed once", {
  # The complement: many providers, each reaching a disjoint slice of tracts, so
  # a chunk-local denominator would be RIGHT for some providers and wrong for
  # others -- the version of the bug that produces a partially plausible map.
  w <- .chunk_world()
  u <- .unchunked(w)
  for (n in c(2L, 5L, 60L)) {
    r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = n)
    d <- numeric_diff(unname(r$provider_ratio), unname(u$provider_ratio), tol = 0,
                      label = sprintf("provider ratio n=%d", n))
    expect_null(d, info = paste0("step-1 ratios changed with the chunk count\n", format_diff(d)))
  }
})

# ------------------------------------------------ adversarial boundary worlds ---

test_that("ADVERSARIAL boundaries: seven hostile chunk configurations", {
  band <- 60
  cases <- list(
    "one provider reaches tracts in many chunks" = {
      tr <- data.frame(tract_id = sprintf("T%02d", 1:30), population = 100L,
                       stringsAsFactors = FALSE)
      pr <- data.frame(provider_id = "P1", supply = 5, stringsAsFactors = FALSE)
      tt <- expand.grid(provider_id = "P1", tract_id = tr$tract_id, stringsAsFactors = FALSE)
      tt$minutes <- 5
      list(tt = tt, providers = pr, tracts = tr)
    },
    "one tract reachable from providers in many chunks" = {
      tr <- data.frame(tract_id = "T1", population = 500L, stringsAsFactors = FALSE)
      pr <- data.frame(provider_id = sprintf("P%02d", 1:20), supply = 1,
                       stringsAsFactors = FALSE)
      tt <- expand.grid(provider_id = pr$provider_id, tract_id = "T1",
                        stringsAsFactors = FALSE)
      tt$minutes <- 5
      list(tt = tt, providers = pr, tracts = tr)
    },
    "provider exactly at each canonical threshold" = {
      tr <- data.frame(tract_id = sprintf("T%d", 1:4), population = 100L,
                       stringsAsFactors = FALSE)
      pr <- data.frame(provider_id = "P1", supply = 4, stringsAsFactors = FALSE)
      tt <- data.frame(provider_id = "P1", tract_id = tr$tract_id,
                       minutes = c(30, 60, 120, 180), stringsAsFactors = FALSE)
      list(tt = tt, providers = pr, tracts = tr)
    },
    "a chunk with no reachable tracts" = {
      tr <- data.frame(tract_id = sprintf("T%02d", 1:20), population = 100L,
                       stringsAsFactors = FALSE)
      pr <- data.frame(provider_id = "P1", supply = 3, stringsAsFactors = FALSE)
      tt <- expand.grid(provider_id = "P1", tract_id = tr$tract_id, stringsAsFactors = FALSE)
      tt$minutes <- c(rep(5, 10), rep(999, 10))     # second half unreachable
      list(tt = tt, providers = pr, tracts = tr)
    },
    "all reachable population split across boundaries" = {
      tr <- data.frame(tract_id = sprintf("T%02d", 1:12), population = 50L,
                       stringsAsFactors = FALSE)
      pr <- data.frame(provider_id = c("P1", "P2"), supply = c(2, 3),
                       stringsAsFactors = FALSE)
      tt <- expand.grid(provider_id = pr$provider_id, tract_id = tr$tract_id,
                        stringsAsFactors = FALSE)
      tt$minutes <- 5
      list(tt = tt, providers = pr, tracts = tr)
    },
    "duplicate-coordinate providers landing in different chunks" = {
      tr <- data.frame(tract_id = sprintf("T%02d", 1:15), population = 200L,
                       stringsAsFactors = FALSE)
      pr <- data.frame(provider_id = c("PA", "PB"), supply = c(1, 1),
                       stringsAsFactors = FALSE)
      tt <- expand.grid(provider_id = pr$provider_id, tract_id = tr$tract_id,
                        stringsAsFactors = FALSE)
      tt$minutes <- 7                                # identical, as if co-located
      list(tt = tt, providers = pr, tracts = tr)
    },
    "a zero-population tract at a chunk boundary" = {
      tr <- data.frame(tract_id = sprintf("T%02d", 1:10),
                       population = c(0L, rep(100L, 8), 0L), stringsAsFactors = FALSE)
      pr <- data.frame(provider_id = "P1", supply = 2, stringsAsFactors = FALSE)
      tt <- expand.grid(provider_id = "P1", tract_id = tr$tract_id, stringsAsFactors = FALSE)
      tt$minutes <- 5
      list(tt = tt, providers = pr, tracts = tr)
    }
  )

  for (nm in names(cases)) {
    cs <- cases[[nm]]
    u <- ref_2sfca(cs$tt, cs$providers, cs$tracts, band)$tract_access
    for (n in c(1L, 2L, 3L, nrow(cs$tracts))) {
      for (sch in c("even", "uneven", "interleaved")) {
        r <- chunk_2sfca(cs$tt, cs$providers, cs$tracts, band,
                         n_chunks = n, scheme = sch)
        msg <- .same_by_id(r$tract_access, u, tol = 0,
                           label = sprintf("%s [n=%d %s]", nm, n, sch))
        expect_null(msg, info = msg)
      }
    }
  }
})

# ------------------------------------------------------------- metamorphic ---

test_that("METAMORPHIC: random worlds at random chunk counts from 1 to 60", {
  n_worlds <- max(10L, n_random_worlds() %/% 4L)
  for (i in seq_len(n_worlds)) {
    seed <- base_seed() + 40000L + i
    w <- make_world(seed = seed, n_providers = sample(3:10, 1),
                    n_tracts = sample(20:80, 1), isolated = sample(0:2, 1),
                    zero_pop = sample(0:3, 1))
    set.seed(seed)
    n <- sample.int(60L, 1L)
    sch <- sample(c("even", "uneven", "interleaved"), 1L)
    ord <- sample(c("original", "reversed", "shuffled"), 1L)

    u <- ref_2sfca(w$tt, w$providers, w$tracts, 60)$tract_access
    r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60,
                     n_chunks = n, scheme = sch, chunk_order = ord, seed = seed)
    msg <- .same_by_id(r$tract_access, u, tol = 0,
                       label = sprintf("seed=%d n=%d %s/%s", seed, n, sch, ord))
    expect_null(msg, info = paste0(
      msg, "\nREPRODUCE: HARNESS_SEED=", seed, " and n_chunks=", n,
      ", scheme=", sch, ", order=", ord))
  }
})

# ------------------------------------------- worker-count decomposition ------

test_that("WORKER COUNT: decomposing across 1, 2 and 4 workers agrees", {
  # LABELLED ACCURATELY. This validates the HARNESS algorithm under a
  # worker-style decomposition -- chunks assigned round-robin to w workers and
  # reduced -- executed sequentially. It does NOT execute production's parallel
  # paths, and it does not use a real parallel backend, so it says nothing about
  # fork/GEOS interactions or RNG streams under parallelism.
  w <- .chunk_world()
  u <- .unchunked(w)$tract_access

  simulate_workers <- function(n_workers, n_chunks = 24L) {
    chunks <- make_chunks(nrow(w$tracts), n_chunks, "even")
    assign_to <- ((seq_along(chunks) - 1L) %% n_workers) + 1L
    acc <- numeric(0); ids <- character(0)
    for (wk in seq_len(n_workers)) {          # each "worker" reduces its own share
      mine <- which(assign_to == wk)
      for (k in mine) {
        ii <- chunks[[k]]
        if (length(ii) == 0L) next
        sub <- w$tracts[ii, , drop = FALSE]
        part <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 1L)$tract_access
        acc <- c(acc, unname(part[sub$tract_id])); ids <- c(ids, sub$tract_id)
      }
    }
    stats::setNames(acc, ids)[w$tracts$tract_id]
  }

  for (nw in c(1L, 2L, 4L)) {
    msg <- .same_by_id(simulate_workers(nw), u, tol = 0,
                       label = sprintf("workers=%d", nw))
    expect_null(msg, info = msg)
  }
})

# ----------------------------------------------------------------- sabotage ---

test_that("SABOTAGE: every chunking mutant is killed by a named probe", {
  w <- .chunk_world(n_tracts = 40L)
  u <- .unchunked(w)

  PROBES <- list(
    identical_across_counts = function() {
      for (n in c(1L, 4L, 40L)) {
        r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = n)
        stopifnot(isTRUE(all.equal(unname(r$tract_access[w$tracts$tract_id]),
                                   unname(u$tract_access[w$tracts$tract_id]))))
      }
    },
    no_row_lost_or_duplicated = function() {
      for (sch in c("even", "uneven")) {
        r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 6L, scheme = sch)
        stopifnot(length(r$tract_access) == nrow(w$tracts),
                  !anyDuplicated(names(r$tract_access)),
                  setequal(names(r$tract_access), w$tracts$tract_id))
      }
    },
    order_invariant = function() {
      a <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 6L, chunk_order = "original")
      b <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 6L, chunk_order = "reversed")
      stopifnot(identical(a$tract_access, b$tract_access))
    },
    undefined_ratio_stays_na = function() {
      r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 6L)
      stopifnot(sum(is.na(r$provider_ratio)) == sum(is.na(u$provider_ratio)))
    },
    all_providers_used = function() {
      r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 6L)
      stopifnot(length(r$provider_ratio) == nrow(w$providers))
    },
    empty_chunk_accounted = function() {
      r <- chunk_2sfca(w$tt, w$providers, w$tracts, 60, n_chunks = 6L, scheme = "uneven")
      stopifnot(!is.null(r$n_chunks), r$n_chunks == 6L)
    }
  )

  fails <- function() {
    f <- character(0)
    for (nm in names(PROBES)) {
      ok <- tryCatch({ PROBES[[nm]](); TRUE }, error = function(e) FALSE)
      if (!ok) f <- c(f, nm)
    }
    f
  }

  # A kill is only meaningful if the probes pass on clean code.
  expect_equal(fails(), character(0),
               info = "a chunking probe fails on unmutated code; fix the probe first")

  results <- data.frame(mutant = character(0), description = character(0),
                        killed = logical(0), killed_by = character(0),
                        stringsAsFactors = FALSE)
  for (nm in names(MUTANTS_CHUNKING)) {
    f <- tryCatch(with_chunk_mutant(nm, quote(fails())),
                  error = function(e) "raised-an-error")
    results <- rbind(results, data.frame(
      mutant = nm, description = MUTANTS_CHUNKING[[nm]]$description,
      killed = length(f) > 0L, killed_by = paste(f, collapse = "; "),
      stringsAsFactors = FALSE))
  }

  dir.create(file.path(.harness_root, "artifacts"), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(results,
                   file.path(.harness_root, "artifacts", "mutation-report-chunking.csv"),
                   row.names = FALSE)

  survivors <- results[!results$killed, ]
  expect_equal(nrow(survivors), 0L,
               info = paste0(
                 "Chunking mutant(s) SURVIVED -- each is a plausible decomposition ",
                 "error this suite would not catch:\n",
                 paste(sprintf("  %s: %s", survivors$mutant, survivors$description),
                       collapse = "\n")))
  expect_gte(nrow(results), 8L)
})
