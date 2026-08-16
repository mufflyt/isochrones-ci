# Chunked decomposition of the 2SFCA calculation.
#
# WHAT THIS IS, PRECISELY
# This is the harness's OWN chunked implementation, and the thing the chunking
# tests exercise. It is NOT production's chunking, and nothing here validates
# `mufflyt/isochrones`' private parallel paths -- that repository is private and
# unreachable from a public harness. What these tests prove is that a chunked
# decomposition of THIS algorithm is invariant to how it is decomposed, and that
# the suite can detect the classic ways such a decomposition goes wrong.
# SCIENTIFIC_VALIDATION.md states that limit explicitly.
#
# THE DEFECT THIS EXISTS TO CATCH
# 2SFCA has two steps, and only the SECOND is safely partitionable:
#
#   step 1 (per provider): R_p = S_p / sum of population in p's catchment
#   step 2 (per tract):    A_t = sum of R_p over providers reachable from t
#
# A provider's catchment does not respect chunk boundaries. If step 1 is
# computed inside each chunk -- using only the population of the tracts in that
# chunk -- then every denominator shrinks, every ratio inflates, and the map
# looks entirely plausible while being wrong by a factor that varies with the
# chunk count. It produces no error, no NA, and no warning.
#
# So: chunking partitions the OUTPUT, never the step-1 denominators.

#' Partition indices into `n` chunks
#'
#' @param n_items Number of items.
#' @param n_chunks Number of chunks.
#' @param scheme "even" (contiguous, near-equal), "uneven" (deliberately ragged,
#'   including empty and single-item chunks), or "interleaved".
#' @return A list of integer vectors. Chunks may be empty; that is a case the
#'   tests require to work, not one to avoid.
make_chunks <- function(n_items, n_chunks, scheme = c("even", "uneven", "interleaved")) {
  scheme <- match.arg(scheme)
  stopifnot(n_chunks >= 1L)
  idx <- seq_len(n_items)
  if (scheme == "even") {
    if (n_chunks == 1L) return(list(idx))
    g <- ceiling(seq_along(idx) / (n_items / n_chunks))
    g[g > n_chunks] <- n_chunks
    out <- split(idx, factor(g, levels = seq_len(n_chunks)))
  } else if (scheme == "interleaved") {
    out <- split(idx, factor(((idx - 1L) %% n_chunks) + 1L, levels = seq_len(n_chunks)))
  } else {
    # Deliberately ragged: one empty chunk, one single-item chunk, the rest
    # sharing what is left. Empty and singleton chunks are where off-by-one
    # boundary handling shows up.
    out <- vector("list", n_chunks)
    if (n_chunks == 1L) return(list(idx))
    out[[1]] <- integer(0)
    out[[2]] <- idx[1]
    rest <- idx[-1]
    if (n_chunks == 2L) { out[[2]] <- idx; return(out[1:2]) }
    g <- ceiling(seq_along(rest) / (length(rest) / (n_chunks - 2L)))
    g[g > (n_chunks - 2L)] <- n_chunks - 2L
    sp <- split(rest, factor(g, levels = seq_len(n_chunks - 2L)))
    for (k in seq_along(sp)) out[[k + 2L]] <- sp[[k]]
  }
  lapply(out, function(x) as.integer(x))
}

#' 2SFCA computed in chunks over the TRACT dimension
#'
#' @param n_chunks How many chunks to split the output across.
#' @param scheme Partition scheme, see `make_chunks()`.
#' @param chunk_order "original", "reversed", or "shuffled" -- the order the
#'   chunks are processed and concatenated in. The answer must not depend on it.
#' @param seed Seed for "shuffled".
#' @return A list with `tract_access` (named, canonicalised by identifier),
#'   `provider_ratio`, and `chunk_of` recording which chunk produced each tract.
#' @details
#' Step 1 is computed ONCE, from the complete travel-time table, before any
#' partitioning. That is the whole correctness argument, and it is why
#' `mut_chunk_local_denominator()` below is a plausible mutant rather than a
#' silly one: moving that computation inside the loop is a one-line edit.
chunk_2sfca <- function(tt, providers, tracts, threshold_min,
                        n_chunks = 1L,
                        scheme = c("even", "uneven", "interleaved"),
                        chunk_order = c("original", "reversed", "shuffled"),
                        seed = 1L) {
  scheme <- match.arg(scheme)
  chunk_order <- match.arg(chunk_order)

  # --- step 1, GLOBAL, over the entire reachable population -------------------
  pop_of <- stats::setNames(tracts$population, tracts$tract_id)
  ratio <- stats::setNames(rep(NA_real_, nrow(providers)), providers$provider_id)
  for (i in seq_len(nrow(providers))) {
    pid <- providers$provider_id[i]
    inside <- tt$tract_id[tt$provider_id == pid & tt$minutes <= threshold_min]
    denom <- sum(pop_of[inside])
    ratio[pid] <- if (length(inside) == 0L || denom == 0) NA_real_ else providers$supply[i] / denom
  }

  # --- step 2, chunked over tracts -------------------------------------------
  chunks <- make_chunks(nrow(tracts), n_chunks, scheme)
  ord <- switch(chunk_order,
                original = seq_along(chunks),
                reversed = rev(seq_along(chunks)),
                shuffled = { set.seed(seed); sample(seq_along(chunks)) })

  parts <- list()
  for (k in ord) {
    ii <- chunks[[k]]
    if (length(ii) == 0L) {           # an empty chunk contributes an empty part,
      parts[[length(parts) + 1L]] <-  # not a skipped one -- a dropped empty
        list(k = k, acc = stats::setNames(numeric(0), character(0)))  # chunk is
      next                            # indistinguishable from a lost tract
    }
    sub <- tracts[ii, , drop = FALSE]
    acc <- stats::setNames(rep(0, nrow(sub)), sub$tract_id)
    for (j in seq_len(nrow(sub))) {
      tid <- sub$tract_id[j]
      reach <- tt$provider_id[tt$tract_id == tid & tt$minutes <= threshold_min]
      if (length(reach) == 0L) { acc[tid] <- 0; next }
      r <- ratio[reach]
      acc[tid] <- sum(r[!is.na(r)])
    }
    parts[[length(parts) + 1L]] <- list(k = k, acc = acc)
  }

  # --- reassembly, canonicalised BY IDENTIFIER -------------------------------
  # Never by row position. Concatenating in chunk order and trusting the order
  # is precisely the bug `mut_join_by_position()` introduces.
  acc_all <- unlist(lapply(parts, function(p) p$acc), use.names = FALSE)
  ids_all <- unlist(lapply(parts, function(p) names(p$acc)), use.names = FALSE)
  chunk_of <- stats::setNames(
    rep(vapply(parts, function(p) p$k, integer(1)),
        vapply(parts, function(p) length(p$acc), integer(1))),
    ids_all)

  out <- stats::setNames(acc_all, ids_all)
  out <- out[tracts$tract_id]          # canonical order, by identifier

  list(tract_access = out, provider_ratio = ratio,
       chunk_of = chunk_of[tracts$tract_id],
       n_chunks = n_chunks, scheme = scheme, chunk_order = chunk_order)
}

# Pristine copy, captured at load. Mutants are built by textual substitution on
# THIS, so a mutant can never silently drift away from the function it claims to
# corrupt -- an anchor that stops matching raises rather than producing a mutant
# identical to the original (which would then "survive" and read as a coverage
# hole that is really a bookkeeping bug).
CHUNK_2SFCA_ORIGINAL <- chunk_2sfca
