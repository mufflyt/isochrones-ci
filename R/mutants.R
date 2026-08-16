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
