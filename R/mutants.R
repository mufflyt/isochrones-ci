# Controlled mutants: deliberately wrong implementations, used to prove the
# validation suite can actually go red.
#
# WHY THIS EXISTS
# A suite that has never been observed failing is indistinguishable from a suite
# that cannot fail. Every check in this repository asserts something; these
# mutants are the negative controls that demonstrate the assertions bite.
#
# Each mutant is a plausible mistake, not a nonsense one. "Return 42" proves
# nothing. "Use `<` where the specification says `<=`" is the kind of edit that
# passes code review, changes a headline percentage by a fraction of a point,
# and never crashes -- which is precisely the class this harness is for.
#
# These are NEVER used by the validation path. test-mutation-sabotage.R runs the
# same assertions against a mutant and requires them to FAIL. A mutant that
# survives is reported as a coverage hole, not quietly ignored.

#' The mutant registry
#'
#' Each entry: a description of the plausible mistake, and a function returning
#' a mutated version of the reference it corrupts.
MUTANTS <- list(

  strict_threshold = list(
    description = "threshold comparison `<=` becomes `<` (boundary tract dropped)",
    target      = "ref_reached",
    make = function() function(tt, threshold_min) {
      tracts <- unique(tt$tract_id)
      out <- logical(length(tracts))
      for (i in seq_along(tracts)) {
        rows <- tt$minutes[tt$tract_id == tracts[i]]
        out[i] <- length(rows) > 0L && min(rows) < threshold_min   # MUTATION
      }
      stats::setNames(out, tracts)
    }
  ),

  swapped_bands = list(
    description = "30 and 60 minute bands swapped at the call site",
    target      = "ref_pop_share_within",
    make = function() function(tt, tracts, threshold_min) {
      swapped <- if (identical(threshold_min, 30)) 60 else
        if (identical(threshold_min, 60)) 30 else threshold_min   # MUTATION
      ref_pop_share_within(tt, tracts, swapped)
    }
  ),

  na_to_zero = list(
    description = "undefined ratio (0 population in catchment) coerced to 0 instead of NA",
    target      = "ref_2sfca",
    make = function() function(tt, providers, tracts, threshold_min) {
      r <- ref_2sfca(tt, providers, tracts, threshold_min)
      r$provider_ratio[is.na(r$provider_ratio)] <- 0        # MUTATION
      r
    }
  ),

  dropped_denominator = list(
    description = "step 1 divides by nothing: supply used as if it were a ratio",
    target      = "ref_2sfca",
    make = function() function(tt, providers, tracts, threshold_min) {
      pop_of <- stats::setNames(tracts$population, tracts$tract_id)
      ratio <- stats::setNames(providers$supply, providers$provider_id)  # MUTATION
      access <- stats::setNames(rep(0, nrow(tracts)), tracts$tract_id)
      for (j in seq_len(nrow(tracts))) {
        tid <- tracts$tract_id[j]
        reach <- tt$provider_id[tt$tract_id == tid & tt$minutes <= threshold_min]
        access[tid] <- if (length(reach) == 0L) 0 else sum(ratio[reach], na.rm = TRUE)
      }
      list(provider_ratio = ratio, tract_access = access, threshold_min = threshold_min)
    }
  ),

  unweighted_mean = list(
    description = "population-weighted share replaced by an unweighted tract mean",
    target      = "ref_pop_share_within",
    make = function() function(tt, tracts, threshold_min) {
      reached <- ref_reached(tt, threshold_min)
      mean(as.logical(reached[tracts$tract_id]))              # MUTATION
    }
  ),

  lost_mass = list(
    description = "allocator drops zero-weight units before normalising, losing mass",
    target      = "ref_allocate",
    make = function() function(total, weights) {
      keep <- weights > 0                                     # MUTATION
      out <- rep(0, length(weights))
      out[keep] <- total * weights[keep] / sum(weights[keep])
      out * 0.999                                             # MUTATION: rounding leak
    }
  ),

  unclamped_ci = list(
    description = "Wald interval not clamped to [0,1]; reports a negative proportion",
    target      = "ref_prop_ci",
    make = function() function(x, n, conf = 0.95) {
      z <- stats::qnorm(1 - (1 - conf) / 2)
      p <- x / n
      se <- sqrt(p * (1 - p) / n)
      c(estimate = p, lower = p - z * se, upper = p + z * se)  # MUTATION
    }
  ),

  wrong_z = list(
    description = "MOE 90->95 conversion uses 1.96/1.65 rounded, not the exact z ratio",
    target      = "ref_moe90_to_ci95_factor",
    make = function() function() 1.96 / 1.65                  # MUTATION
  ),

  rurality_off_by_one = list(
    description = "RUCA metro boundary moved from <=3 to <=4",
    target      = "ref_rurality",
    make = function() function(ruca) {
      out <- rep(NA_character_, length(ruca))
      ok <- !is.na(ruca)
      out[ok & ruca <= 4] <- "metro"                          # MUTATION
      out[ok & ruca >= 5] <- "nonmetro"
      out
    }
  ),

  divide_by_zero_inf = list(
    description = "safe division lets x/0 through as Inf instead of NA",
    target      = "ref_safe_divide",
    make = function() function(numerator, denominator) {
      numerator / denominator                                 # MUTATION
    }
  ),

  id_drops_short = list(
    description = "identifier canonicaliser rejects short ids instead of zero-padding",
    target      = "ref_canon_id",
    make = function() function(x) {
      out <- ref_canon_id(x)
      out[!is.na(out) & substr(out, 1, 1) == "0"] <- NA_character_  # MUTATION
      out
    }
  ),

  containment_reversed = list(
    description = "band containment asserted in the wrong direction (60 within 30)",
    target      = "band_contains",
    make = function() function(inner, outer, tol = 1e-6) {
      band_contains(outer, inner, tol)                        # MUTATION
    }
  )
)

#' Geometric containment check, isolated so a mutant can corrupt it
#'
#' TRUE when `inner` is contained in `outer` up to a tolerance expressed as a
#' fraction of the inner area. Buffers are polygonal approximations of circles,
#' so exact containment is not achievable and a tolerance is required; making it
#' a fraction of the INNER area keeps the tolerance meaningful at every scale.
band_contains <- function(inner, outer, tol = 1e-6) {
  require_sf()
  outside <- suppressWarnings(sf::st_difference(sf::st_union(inner), sf::st_union(outer)))
  if (length(outside) == 0L) return(TRUE)
  a_out <- as.numeric(sum(sf::st_area(outside)))
  a_in  <- as.numeric(sum(sf::st_area(sf::st_union(inner))))
  if (a_in == 0) return(TRUE)
  (a_out / a_in) <= tol
}

#' Apply a mutant, run an expression, and always restore
#'
#' The restore is in `on.exit`, so a mutant cannot leak into subsequent tests
#' even if the expression throws -- which it is supposed to do.
with_mutant <- function(name, expr, envir = parent.frame()) {
  m <- MUTANTS[[name]]
  if (is.null(m)) stop("unknown mutant: ", name)
  target <- m$target
  original <- get(target, envir = globalenv())
  assign(target, m$make(), envir = globalenv())
  on.exit(assign(target, original, envir = globalenv()), add = TRUE)
  force(eval(expr, envir = envir))
}

# ---------------------------------------------------------------------------
# CHUNKING MUTANTS.
#
# Written as EXPLICIT functions, not textual edits to the original. A first
# attempt built them by deparsing chunk_2sfca() and substituting anchor strings;
# deparse reformats the source, three anchors stopped matching, and two mutants
# silently "survived" because they had never been applied at all. A mutant that
# fails to apply reads exactly like a mutant the suite cannot catch, which would
# have been reported as a coverage hole that was really a bookkeeping bug.
#
# Each is a one-line-ish edit a reviewer could wave through, and each produces a
# complete, plausible, wrong answer with no error, no NA and no warning.
#
# IMPORTANT: several manifest only under specific conditions -- an empty chunk,
# a non-original chunk order, a provider with an undefined ratio. The probes in
# test-chunking-invariance.R must create those conditions, or the mutant looks
# dead when it is merely dormant.
# ---------------------------------------------------------------------------

# The correct step-1 ratios, shared by the mutants that do not corrupt step 1.
.correct_ratio <- function(tt, providers, tracts, threshold_min) {
  pop_of <- stats::setNames(tracts$population, tracts$tract_id)
  r <- stats::setNames(rep(NA_real_, nrow(providers)), providers$provider_id)
  for (i in seq_len(nrow(providers))) {
    pid <- providers$provider_id[i]
    inside <- tt$tract_id[tt$provider_id == pid & tt$minutes <= threshold_min]
    denom <- sum(pop_of[inside])
    r[pid] <- if (length(inside) == 0L || denom == 0) NA_real_ else providers$supply[i] / denom
  }
  r
}

MUTANTS_CHUNKING <- list(

  # THE headline defect. Step 1 recomputed inside each chunk, so every
  # denominator is only the population of that chunk. Ratios inflate, the map
  # stays plausible, and the error scales with the chunk count.
  chunk_local_denominator = list(
    description = "step-1 denominator computed per chunk instead of over the whole catchment",
    make = function() {
      function(tt, providers, tracts, threshold_min, n_chunks = 1L,
               scheme = "even", chunk_order = "original", seed = 1L) {
        chunks <- make_chunks(nrow(tracts), n_chunks, scheme)
        acc_all <- numeric(0); ids_all <- character(0)
        for (ii in chunks) {
          if (length(ii) == 0L) next
          sub <- tracts[ii, , drop = FALSE]
          ratio <- .correct_ratio(tt, providers, sub, threshold_min)  # MUTATION
          for (j in seq_len(nrow(sub))) {
            tid <- sub$tract_id[j]
            reach <- tt$provider_id[tt$tract_id == tid & tt$minutes <= threshold_min]
            v <- if (length(reach) == 0L) 0 else sum(ratio[reach][!is.na(ratio[reach])])
            acc_all <- c(acc_all, v); ids_all <- c(ids_all, tid)
          }
        }
        out <- stats::setNames(acc_all, ids_all)[tracts$tract_id]
        list(tract_access = out, provider_ratio = .correct_ratio(tt, providers, tracts, threshold_min),
             chunk_of = NULL, n_chunks = n_chunks)
      }
    }
  ),

  drop_boundary_row = list(
    description = "the last tract of every chunk is lost at the boundary",
    make = function() {
      function(tt, providers, tracts, threshold_min, n_chunks = 1L,
               scheme = "even", chunk_order = "original", seed = 1L) {
        r <- CHUNK_2SFCA_ORIGINAL(tt, providers, tracts, threshold_min,
                                  n_chunks, scheme, chunk_order, seed)
        chunks <- make_chunks(nrow(tracts), n_chunks, scheme)
        lost <- unlist(lapply(chunks, function(ii) if (length(ii) > 1L) tracts$tract_id[ii[length(ii)]] else character(0)))
        r$tract_access <- r$tract_access[!(names(r$tract_access) %in% lost)]   # MUTATION
        r
      }
    }
  ),

  duplicate_boundary_row = list(
    description = "the first tract of every chunk is emitted twice",
    make = function() {
      function(tt, providers, tracts, threshold_min, n_chunks = 1L,
               scheme = "even", chunk_order = "original", seed = 1L) {
        r <- CHUNK_2SFCA_ORIGINAL(tt, providers, tracts, threshold_min,
                                  n_chunks, scheme, chunk_order, seed)
        chunks <- make_chunks(nrow(tracts), n_chunks, scheme)
        dup <- unlist(lapply(chunks, function(ii) if (length(ii)) tracts$tract_id[ii[1]] else character(0)))
        r$tract_access <- c(r$tract_access, r$tract_access[dup])               # MUTATION
        r
      }
    }
  ),

  drop_empty_chunk = list(
    description = "an empty chunk is skipped, so its (absent) rows are never accounted for",
    make = function() {
      function(tt, providers, tracts, threshold_min, n_chunks = 1L,
               scheme = "even", chunk_order = "original", seed = 1L) {
        r <- CHUNK_2SFCA_ORIGINAL(tt, providers, tracts, threshold_min,
                                  n_chunks, scheme, chunk_order, seed)
        # Report a chunk count that ignores the empty ones -- the accounting
        # error that follows from silently dropping them.
        chunks <- make_chunks(nrow(tracts), n_chunks, scheme)
        r$n_chunks <- sum(vapply(chunks, length, integer(1)) > 0L)             # MUTATION
        r
      }
    }
  ),

  na_to_zero_on_bind = list(
    description = "an undefined provider ratio is coerced to 0 during reassembly",
    make = function() {
      function(tt, providers, tracts, threshold_min, n_chunks = 1L,
               scheme = "even", chunk_order = "original", seed = 1L) {
        r <- CHUNK_2SFCA_ORIGINAL(tt, providers, tracts, threshold_min,
                                  n_chunks, scheme, chunk_order, seed)
        r$provider_ratio[is.na(r$provider_ratio)] <- 0                         # MUTATION
        r
      }
    }
  ),

  join_by_position = list(
    description = "chunk results reassembled by row position instead of identifier",
    make = function() {
      function(tt, providers, tracts, threshold_min, n_chunks = 1L,
               scheme = "even", chunk_order = "original", seed = 1L) {
        r <- CHUNK_2SFCA_ORIGINAL(tt, providers, tracts, threshold_min,
                                  n_chunks, scheme, chunk_order, seed)
        # Re-label in emission order rather than looking up by id. Identical to
        # the correct answer when chunks come back in order; wrong the moment
        # they do not -- which is exactly what a parallel reduce does.
        ord <- switch(chunk_order, original = seq_len(n_chunks),
                      reversed = rev(seq_len(n_chunks)),
                      shuffled = { set.seed(seed); sample(seq_len(n_chunks)) })
        chunks <- make_chunks(nrow(tracts), n_chunks, scheme)
        emitted <- unlist(lapply(ord, function(k) tracts$tract_id[chunks[[k]]]))
        vals <- unname(r$tract_access[emitted])
        r$tract_access <- stats::setNames(vals, tracts$tract_id)               # MUTATION
        r
      }
    }
  ),

  first_chunk_providers_only = list(
    description = "only the first chunk's provider set is used for every chunk",
    make = function() {
      function(tt, providers, tracts, threshold_min, n_chunks = 1L,
               scheme = "even", chunk_order = "original", seed = 1L) {
        keep <- providers[1, , drop = FALSE]                                   # MUTATION
        CHUNK_2SFCA_ORIGINAL(tt[tt$provider_id %in% keep$provider_id, , drop = FALSE],
                             keep, tracts, threshold_min, n_chunks, scheme, chunk_order, seed)
      }
    }
  ),

  reduction_overwrites = list(
    description = "reassembly overwrites instead of accumulating: only the last chunk survives",
    make = function() {
      function(tt, providers, tracts, threshold_min, n_chunks = 1L,
               scheme = "even", chunk_order = "original", seed = 1L) {
        r <- CHUNK_2SFCA_ORIGINAL(tt, providers, tracts, threshold_min,
                                  n_chunks, scheme, chunk_order, seed)
        chunks <- make_chunks(nrow(tracts), n_chunks, scheme)
        last <- chunks[[length(chunks)]]
        keep <- tracts$tract_id[last]
        r$tract_access <- r$tract_access[names(r$tract_access) %in% keep]      # MUTATION
        r
      }
    }
  )
)

#' Apply a chunking mutant and always restore
with_chunk_mutant <- function(name, expr, envir = parent.frame()) {
  m <- MUTANTS_CHUNKING[[name]]
  if (is.null(m)) stop("unknown chunking mutant: ", name)
  original <- get("chunk_2sfca", envir = globalenv())
  assign("chunk_2sfca", m$make(), envir = globalenv())
  on.exit(assign("chunk_2sfca", original, envir = globalenv()), add = TRUE)
  force(eval(expr, envir = envir))
}
