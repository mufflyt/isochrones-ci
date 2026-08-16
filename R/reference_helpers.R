# INDEPENDENT REFERENCE IMPLEMENTATIONS.
#
# READ THIS BEFORE EDITING.
#
# Nothing in this file may call, source, import, or copy production code. Its
# whole value is independence: if it shares a helper with the implementation it
# checks, a bug in that helper is invisible to both and the cross-check silently
# certifies the wrong answer.
#
# These are written for OBVIOUS CORRECTNESS, not speed. Loops over a handful of
# rows are preferred to vectorised cleverness, because a reviewer has to be able
# to read the equation off the page and agree with it. Every function states its
# formula in the comment above it.
#
# They operate only on tiny fixtures. If one of these ever needs to be fast,
# that is a signal the fixture has grown too large, not that the reference
# should be optimised.

# ------------------------------------------------------- coverage / access ---

#' REFERENCE: which tracts are within `threshold_min` of any provider
#'
#' Equation: reached(t) = TRUE iff min over providers p of tt(p,t) <= threshold.
#'
#' Note the inclusive `<=`. A threshold is a closed interval on the production
#' side too; `<` vs `<=` at the boundary is one of the mutations this harness
#' deliberately introduces, because it is exactly the kind of off-by-one that
#' changes a headline number by a fraction of a percent and never crashes.
ref_reached <- function(tt, threshold_min) {
  stopifnot(is.data.frame(tt), threshold_min > 0)
  tracts <- unique(tt$tract_id)
  out <- logical(length(tracts))
  for (i in seq_along(tracts)) {
    rows <- tt$minutes[tt$tract_id == tracts[i]]
    out[i] <- length(rows) > 0L && min(rows) <= threshold_min
  }
  stats::setNames(out, tracts)
}

#' REFERENCE: population-weighted share of population within a threshold
#'
#' Equation: share = sum_{t reached} pop(t) / sum_{all t} pop(t).
#'
#' ZERO SEMANTICS, stated explicitly because this is where accessibility code
#' most often goes quietly wrong:
#'   * total population 0            -> NA_real_ (undefined, NOT 0)
#'   * no tract reached, pop > 0     -> 0 (a real, meaningful zero)
#'   * every tract reached           -> 1
#' Returning 0 for "0/0" is the single most common way a coverage statistic
#' becomes a confident lie, so the reference refuses to do it.
ref_pop_share_within <- function(tt, tracts, threshold_min) {
  reached <- ref_reached(tt, threshold_min)
  total <- sum(tracts$population)
  if (total == 0) return(NA_real_)
  num <- 0
  for (i in seq_len(nrow(tracts))) {
    tid <- tracts$tract_id[i]
    if (isTRUE(unname(reached[tid]))) num <- num + tracts$population[i]
  }
  num / total
}

#' REFERENCE: two-step floating catchment area (2SFCA)
#'
#' Step 1, for each provider p:
#'     R_p = S_p / sum_{t : tt(p,t) <= threshold} Pop_t
#'   i.e. supply per head of the population inside the provider's catchment.
#'   If the catchment holds zero population, R_p is undefined -> NA (not 0, and
#'   not Inf): a provider serving nobody has no meaningful ratio, and coercing
#'   that to Inf poisons every downstream sum.
#'
#' Step 2, for each tract t:
#'     A_t = sum_{p : tt(p,t) <= threshold} R_p
#'   accessibility is the sum of the provider-to-population ratios reachable
#'   from that tract. A tract reaching no provider scores a genuine 0.
#'
#' Reference: Luo & Wang (2003), Environment and Planning B 30(6):865-884.
ref_2sfca <- function(tt, providers, tracts, threshold_min) {
  stopifnot(all(c("provider_id", "supply") %in% names(providers)))
  stopifnot(all(c("tract_id", "population") %in% names(tracts)))

  pop_of <- stats::setNames(tracts$population, tracts$tract_id)

  # ---- step 1 ------------------------------------------------------------
  ratio <- stats::setNames(rep(NA_real_, nrow(providers)), providers$provider_id)
  for (i in seq_len(nrow(providers))) {
    pid <- providers$provider_id[i]
    inside <- tt$tract_id[tt$provider_id == pid & tt$minutes <= threshold_min]
    denom <- sum(pop_of[inside])
    ratio[pid] <- if (length(inside) == 0L || denom == 0) {
      NA_real_          # undefined, deliberately not 0 and not Inf
    } else {
      providers$supply[i] / denom
    }
  }

  # ---- step 2 ------------------------------------------------------------
  access <- stats::setNames(rep(0, nrow(tracts)), tracts$tract_id)
  for (j in seq_len(nrow(tracts))) {
    tid <- tracts$tract_id[j]
    reach <- tt$provider_id[tt$tract_id == tid & tt$minutes <= threshold_min]
    if (length(reach) == 0L) { access[tid] <- 0; next }
    r <- ratio[reach]
    # A provider whose own catchment is empty contributes nothing rather than
    # NA-poisoning the tract. Documented rather than silent: this is a modelling
    # choice, and the production side must make the same one.
    access[tid] <- sum(r[!is.na(r)])
  }

  list(provider_ratio = ratio, tract_access = access, threshold_min = threshold_min)
}

# --------------------------------------------------------------- allocation ---

#' REFERENCE: allocate a national total across units by weights
#'
#' Equation: alloc_i = total * w_i / sum(w).
#'
#' CONSERVATION is the property that matters: sum(alloc) == total, exactly, up
#' to floating point. Allocators lose mass through rounding, through dropping
#' zero-weight units, and through normalising by the wrong denominator. This
#' reference does none of those, so a production allocator that disagrees is
#' either rounding (which must be declared) or losing mass (which is a bug).
ref_allocate <- function(total, weights) {
  stopifnot(is.numeric(total), length(total) == 1L, is.numeric(weights))
  if (any(is.na(weights))) stop("ref_allocate: NA weight; refuse to guess")
  if (any(weights < 0)) stop("ref_allocate: negative weight")
  s <- sum(weights)
  if (s == 0) stop("ref_allocate: weights sum to zero; allocation undefined")
  total * weights / s
}

#' REFERENCE: integer allocation by largest remainder (Hamilton's method)
#'
#' Fractional allocation cannot be reported as a count of people. The standard
#' fix is largest remainder: floor everything, then hand the leftover units one
#' at a time to the largest fractional parts.
#'
#'   base_i      = floor(total * w_i / sum(w))
#'   leftover    = total - sum(base)
#'   recipients  = the `leftover` units with the largest fractional remainders
#'
#' The property that matters is unchanged: sum(alloc) == total EXACTLY. Rounding
#' each share independently does not have that property, and is how an allocator
#' quietly loses or invents a handful of clinicians.
#'
#' Ties are broken by index, so the result is deterministic. A tie-break by
#' anything order-dependent would make the allocator fail the metamorphic
#' row-order test, which is why this is spelled out.
ref_allocate_integer <- function(total, weights) {
  exact <- ref_allocate(total, weights)
  base <- floor(exact)
  leftover <- as.integer(round(total - sum(base)))
  if (leftover > 0L) {
    rem <- exact - base
    ord <- order(-rem, seq_along(rem))
    base[ord[seq_len(leftover)]] <- base[ord[seq_len(leftover)]] + 1
  }
  base
}

# ------------------------------------------------------------- proportions ---

#' REFERENCE: Wald confidence interval for a proportion
#'
#' Equation: p +/- z * sqrt(p(1-p)/n), clamped to [0, 1].
#' z = 1.959964 for 95%, 1.644854 for 90%.
#'
#' Clamping matters: an unclamped Wald interval on a small n happily reports a
#' lower bound below zero, which is not a proportion.
ref_prop_ci <- function(x, n, conf = 0.95) {
  stopifnot(x >= 0, n > 0, x <= n)
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- x / n
  se <- sqrt(p * (1 - p) / n)
  c(estimate = p,
    lower = max(0, p - z * se),
    upper = min(1, p + z * se))
}

#' REFERENCE: Wilson score interval for a proportion
#'
#' Equation, with z the standard normal quantile and n the denominator:
#'
#'   centre     = (p + z^2/(2n)) / (1 + z^2/n)
#'   half_width = z/(1 + z^2/n) * sqrt( p(1-p)/n + z^2/(4n^2) )
#'
#' Wilson is used rather than Wald because it stays inside [0, 1] by
#' construction and behaves at small n and extreme p, where Wald produces
#' intervals that run off the end of the scale. Production uses Wilson; this
#' reference derives it independently from the formula rather than calling
#' production, which is the entire point of a cross-check.
#'
#' Reference: Wilson EB (1927), JASA 22(158):209-212.
ref_prop_ci_wilson <- function(x, n, conf = 0.95) {
  stopifnot(x >= 0, n > 0, x <= n)
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denom  <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half   <- (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  c(estimate = p,
    lower = max(0, centre - half),
    upper = min(1, centre + half))
}

#' REFERENCE: convert an ACS 90% margin of error to a 95% CI half-width
#'
#' ACS publishes MOE at 90% (z = 1.644854). Rescaling to 95% (z = 1.959964) is
#' a ratio of z-scores: factor = 1.959964 / 1.644854 = 1.191601...
ref_moe90_to_ci95_factor <- function() {
  stats::qnorm(0.975) / stats::qnorm(0.95)
}

# -------------------------------------------------------------- identifiers ---

#' REFERENCE: canonicalise a 10-digit provider identifier
#'
#' Rules, stated so they can be argued with:
#'   * NULL           -> character(0)
#'   * NA or ""       -> NA
#'   * strip spaces, hyphens, dots
#'   * any letter     -> NA (an identifier with a letter is not a 10-digit id)
#'   * scientific notation -> NA (it has already lost precision)
#'   * > 10 digits    -> NA
#'   * < 10 digits    -> left-pad with zeros
#' Zero-padding rather than rejection is the interesting one: identifiers that
#' pass through a numeric column lose leading zeros, and silently dropping those
#' rows is how a cohort quietly shrinks.
ref_canon_id <- function(x) {
  if (is.null(x)) return(character(0))
  original <- as.character(x)
  out <- original
  out[is.na(out) | out == ""] <- NA_character_
  out[grepl("[eE][+-]?[0-9]+", out) & !is.na(out)] <- NA_character_
  # A SIGNED value is not an identifier. Stripping the minus and keeping the
  # digits turns "-1234567890" into a perfectly plausible id, which is worse
  # than rejecting it: the row survives, pointing at the wrong entity.
  out[grepl("^[[:space:]]*[-+]", out) & !is.na(out)] <- NA_character_
  stripped <- gsub("[[:space:].-]", "", out)
  out[grepl("[A-Za-z]", stripped) & !is.na(out)] <- NA_character_
  digits <- gsub("[^0-9]", "", out)
  digits[digits == ""] <- NA_character_
  digits[!is.na(digits) & nchar(digits) > 10L] <- NA_character_
  keep <- !is.na(digits)
  # ZERO-pad explicitly. formatC(flag = "0") pads a CHARACTER vector with
  # SPACES -- the flag only applies to numeric formats -- so the obvious
  # spelling silently produces " 123456789", which is not an identifier and
  # compares unequal to everything. Caught by the sabotage probe.
  digits[keep] <- vapply(digits[keep], function(d) {
    if (nchar(d) >= 10L) d else paste0(strrep("0", 10L - nchar(d)), d)
  }, character(1), USE.NAMES = FALSE)
  digits
}

# ---------------------------------------------------------------- rurality ---

#' REFERENCE: RUCA code -> metro / nonmetro
#'
#' USDA RUCA primary codes 1-3 are metropolitan; 4 and above are not. The
#' boundary is the whole content of the function, which is why it is worth
#' testing: an off-by-one here reclassifies every micropolitan tract.
ref_rurality <- function(ruca) {
  out <- rep(NA_character_, length(ruca))
  ok <- !is.na(ruca)
  out[ok & ruca <= 3] <- "metro"
  out[ok & ruca >= 4] <- "nonmetro"
  out
}

# ------------------------------------------------------------ safe division ---

#' REFERENCE: division that refuses to invent a number
#'
#' 0/0 is undefined -> NA. x/0 for x != 0 is undefined here too: in a rate
#' context an infinite rate is never the intended answer, and letting Inf
#' through is how a summary statistic becomes Inf three joins later.
ref_safe_divide <- function(numerator, denominator) {
  stopifnot(length(numerator) == length(denominator) ||
              length(numerator) == 1L || length(denominator) == 1L)
  n <- numerator; d <- denominator
  out <- rep(NA_real_, max(length(n), length(d)))
  n <- rep(n, length.out = length(out))
  d <- rep(d, length.out = length(out))
  ok <- !is.na(n) & !is.na(d) & d != 0
  out[ok] <- n[ok] / d[ok]
  out
}

# ------------------------------------------------------- survival analysis ---

#' REFERENCE: reconstruct a survival curve from its annual hazards
#'
#' A discrete-time survival function and its hazard are not two independent
#' quantities -- one determines the other:
#'
#'   S(a_0)   = 1 - h(a_0)          (surviving the first interval)
#'   S(a+1)   = S(a) * (1 - h(a+1))
#'
#' So publishing both is publishing a checkable identity. If production's
#' `p_still_active` column and its `annual_hazard` column disagree, at least one
#' is wrong, and no amount of plausibility in either alone would reveal it.
#'
#' This is the most valuable kind of cross-check available: it needs no external
#' data and no second implementation of the model, only the definition.
ref_survival_from_hazard <- function(hazard) {
  stopifnot(is.numeric(hazard), all(hazard >= 0 & hazard <= 1, na.rm = TRUE))
  cumprod(1 - hazard)
}

#' REFERENCE: hazard recovered from a survival curve
#'
#'   h(a+1) = 1 - S(a+1)/S(a)
#'
#' The inverse of the above. Provided so the identity can be checked in both
#' directions -- a bug that happens to be self-consistent in one direction is
#' rarer, but not impossible.
ref_hazard_from_survival <- function(surv) {
  stopifnot(is.numeric(surv))
  c(1 - surv[1], 1 - surv[-1] / surv[-length(surv)])
}

# ------------------------------------------------------------- statistics ---

#' REFERENCE: pooled two-proportion z-test
#'
#'   p_pool = (x1 + x2) / (n1 + n2)
#'   se     = sqrt( p_pool (1 - p_pool) (1/n1 + 1/n2) )
#'   z      = (p1 - p2) / se
#'   p      = 2 * (1 - Phi(|z|))                 [two-sided]
#'
#' Pooled, because the null hypothesis is that the two proportions are equal;
#' using the unpooled standard error under the null is a common and subtle
#' error that shifts every p-value slightly.
ref_two_prop_test <- function(x1, n1, x2, n2) {
  stopifnot(n1 > 0, n2 > 0, x1 >= 0, x2 >= 0, x1 <= n1, x2 <= n2)
  p1 <- x1 / n1; p2 <- x2 / n2
  p_pool <- (x1 + x2) / (n1 + n2)
  se <- sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))
  if (se == 0) return(c(z = NA_real_, p = NA_real_))
  z <- (p1 - p2) / se
  c(z = z, p = 2 * stats::pnorm(-abs(z)))
}

#' REFERENCE: two-proportion test WITH Yates continuity correction
#'
#' Closed form for a 2x2 table (a = x1, b = n1-x1, c = x2, d = n2-x2, N = n1+n2):
#'
#'   chi2 = N * ( |ad - bc| - N/2 )^2
#'          / ( (a+b)(c+d)(a+c)(b+d) )
#'
#' Written from the formula rather than by calling prop.test(), so the
#' comparison is genuinely independent of the function under test.
#'
#' WHY THE CORRECTION IS WORTH PINNING. It is not a rounding detail. For
#' x1=10/n1=100 vs x2=20/n2=100 the uncorrected test gives p = 0.0477 and the
#' corrected test gives p = 0.0747 -- the same data, on opposite sides of 0.05.
#' Whether production applies the correction is therefore a decision that can
#' flip a reported conclusion, and a harness that accepted either would be
#' asserting nothing about it. Production uses prop.test's default, which is
#' corrected; this reference asserts that choice explicitly so a silent switch
#' to correct = FALSE fails here.
ref_two_prop_test_yates <- function(x1, n1, x2, n2) {
  stopifnot(n1 > 0, n2 > 0, x1 >= 0, x2 >= 0, x1 <= n1, x2 <= n2)
  # as.numeric throughout: with integer inputs the four-way product overflows
  # 32-bit integer for n around 2,000 and silently becomes NA, which turned into
  # "missing value where TRUE/FALSE needed" inside the guard below. Integer
  # overflow in a denominator is exactly the sort of thing that produces a
  # confident wrong answer elsewhere.
  a <- as.numeric(x1); b <- as.numeric(n1 - x1)
  c_ <- as.numeric(x2); d <- as.numeric(n2 - x2)
  N <- as.numeric(n1) + as.numeric(n2)
  denom <- (a + b) * (c_ + d) * (a + c_) * (b + d)
  if (denom == 0) return(c(chisq = NA_real_, p = NA_real_))
  num <- N * (abs(a * d - b * c_) - N / 2)^2
  chi <- num / denom
  # The correction can overshoot on tiny tables; prop.test floors the corrected
  # deviation at zero rather than letting it go negative.
  if (abs(a * d - b * c_) < N / 2) chi <- 0
  c(chisq = chi, p = stats::pchisq(chi, df = 1, lower.tail = FALSE))
}

#' REFERENCE: ordinary least squares slope, closed form
#'
#'   b = sum((x - xbar)(y - ybar)) / sum((x - xbar)^2)
#'
#' Written out rather than delegated to lm(), so the comparison is against the
#' formula itself.
ref_ols_slope <- function(x, y) {
  stopifnot(length(x) == length(y), length(x) >= 2L)
  xb <- mean(x); yb <- mean(y)
  sum((x - xb) * (y - yb)) / sum((x - xb)^2)
}

#' REFERENCE: share of weight sitting on zero-valued units, as a PERCENT
#'
#'   share = 100 * sum(w[access == 0]) / sum(w)
#'
#' The units matter and are easy to get wrong in either direction: production
#' returns a percent, and a consumer that treats it as a fraction under-reports
#' by a factor of 100. Asserted here so the units are pinned, not assumed.
ref_zero_share_pct <- function(access, w) {
  stopifnot(length(access) == length(w))
  tot <- sum(w)
  if (tot == 0) return(NA_real_)
  100 * sum(w[access == 0]) / tot
}
