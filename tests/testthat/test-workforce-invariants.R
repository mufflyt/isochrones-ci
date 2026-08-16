# Workforce survival, hazard and FTE invariants.
#
# This is the layer with the most scientific leverage in the whole harness,
# because production publishes BOTH a survival curve and the annual hazards it
# came from. Those are not independent quantities -- one determines the other --
# so the pair is a checkable identity that needs no external data and no second
# implementation of the model, only the definition:
#
#   S(a+1) = S(a) * (1 - h(a+1))
#
# If the two columns disagree, at least one is wrong, and no amount of
# plausibility in either alone would reveal it.

.pathways <- c("ABOG", "ABU")
.sexes    <- c("female", "male")

test_that("SURVIVAL/HAZARD IDENTITY: the published curve is the product of its own hazards", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  for (pw in .pathways) for (sx in .sexes) {
    s <- mufflyaccess::urps_survival_curve(sex = sx, pathway = pw)
    expect_true(all(c("age", "p_still_active", "annual_hazard") %in% names(s)),
                info = sprintf("%s/%s: survival curve is missing a column", pw, sx))

    # ADJUDICATED 2026-08-16. The identity asserted is the INCREMENT form:
    #
    #   S(a+1) / S(a) = 1 - h(a+1)
    #
    # NOT the absolute form S(a) = prod(1 - h). The absolute form fails, and
    # production is right: its curve is anchored by a constant factor
    # (measured 0.9995695, identical at every age), so S(35) is 0.9994472 rather
    # than 1 - h(35) = 0.9998777. That is a modelling choice about what the
    # cohort has already survived before age 35, and it is production's to make.
    #
    # The increment identity holds EXACTLY -- the hazard recovered from
    # consecutive survivals reproduces the published hazard column to the last
    # digit -- and it is the strong claim: it pins all 45 hazards against all 46
    # survival values with no free parameter. An anchoring constant cannot hide
    # a wrong hazard from it.
    recovered <- ref_hazard_from_survival(s$p_still_active)
    d2 <- numeric_diff(recovered[-1], s$annual_hazard[-1], tol = 1e-10,
                       label = sprintf("%s/%s hazard", pw, sx))
    expect_null(d2, info = paste0(
      sprintf("%s/%s: the hazard implied by consecutive survival values does not ", pw, sx),
      "match the published annual_hazard column. One of the two is wrong.\n",
      format_diff(d2)))

    # The anchoring factor must be a CONSTANT. If it varies with age, the two
    # columns are not describing the same process at all.
    k <- s$p_still_active / cumprod(1 - s$annual_hazard)
    expect_lt(max(k) - min(k), 1e-12,
              label = sprintf("%s/%s: the survival/hazard anchoring factor is not constant", pw, sx))
  }
})

test_that("SURVIVAL: p_still_active is a survival function", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  for (pw in .pathways) for (sx in .sexes) {
    s <- mufflyaccess::urps_survival_curve(sex = sx, pathway = pw)
    lbl <- sprintf("%s/%s", pw, sx)
    expect_true(all(s$p_still_active >= 0 & s$p_still_active <= 1),
                info = paste(lbl, "- a survival probability outside [0,1]"))
    # Monotone NON-INCREASING. A survival function that rises means people are
    # un-retiring, which is not what the model claims.
    expect_true(all(diff(s$p_still_active) <= 1e-12),
                info = paste(lbl, "- survival rose with age"))
    expect_true(all(s$annual_hazard >= 0 & s$annual_hazard <= 1),
                info = paste(lbl, "- a hazard outside [0,1] is not a probability"))
    expect_false(anyDuplicated(s$age) > 0L, info = paste(lbl, "- duplicated age"))
    expect_true(all(diff(s$age) == 1L), info = paste(lbl, "- ages are not consecutive"))
  }
})

test_that("HAZARD: retirement risk rises across the career", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  # Not asserted pointwise-monotone: a fitted hazard may wobble locally and that
  # is not a defect. The claim is the direction over the span, which is what the
  # model is for.
  for (pw in .pathways) for (sx in .sexes) {
    s <- mufflyaccess::urps_survival_curve(sex = sx, pathway = pw)
    early <- mean(utils::head(s$annual_hazard, 5))
    late  <- mean(utils::tail(s$annual_hazard, 5))
    expect_gt(late, early)
  }
})

test_that("METAMORPHIC: shifting retirement later can only raise survival", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  # A pure directional check on a lever the scenarios use. If a +2 year shift
  # made people retire EARLIER, every scenario built on it would be inverted.
  base  <- mufflyaccess::urps_survival_curve(sex = "female", pathway = "ABOG",
                                             retirement_shift_years = 0)
  later <- mufflyaccess::urps_survival_curve(sex = "female", pathway = "ABOG",
                                             retirement_shift_years = 2)
  earlier <- mufflyaccess::urps_survival_curve(sex = "female", pathway = "ABOG",
                                               retirement_shift_years = -2)
  common <- intersect(base$age, intersect(later$age, earlier$age))
  b <- base$p_still_active[match(common, base$age)]
  l <- later$p_still_active[match(common, later$age)]
  e <- earlier$p_still_active[match(common, earlier$age)]

  expect_true(all(l >= b - 1e-12),
              info = "shifting retirement 2 years LATER decreased survival")
  expect_true(all(e <= b + 1e-12),
              info = "shifting retirement 2 years EARLIER increased survival")
  expect_true(any(l > b + 1e-9),
              info = "a +2 year shift changed nothing; the lever is not wired")
})

test_that("FTE: the age weight is a bounded, unimodal career arc", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")

  # ADJUDICATED 2026-08-16. This test originally asserted the weight was
  # non-increasing in age. That is FALSE, and production is right. The measured
  # ABOG curve RISES from 0.175 at age 35 to 1.000 at 44 -- the early-career
  # ramp, when clinicians are still completing training or working part time --
  # and only then tapers, to 0.105 by 80. It is unimodal, not monotone. A
  # harness asserting monotonicity would have raised a permanent false alarm
  # against a correct model. (Same class of error as the prevalence assumption;
  # see SCIENTIFIC_VALIDATION.md.)
  #
  # What IS assertable: the weight is a share of full time, it peaks in
  # mid-career rather than at either end, and it genuinely declines afterwards.
  ages <- 35:80
  for (pw in .pathways) {
    w <- vapply(ages, function(a) as.numeric(mufflyaccess::urps_fte_weight(a, pw)), numeric(1))
    lbl <- sprintf("%s", pw)

    expect_true(all(w > 0 & w <= 1),
                info = paste(lbl, "- an FTE weight outside (0,1] is not a share of full time"))

    peak <- ages[which.max(w)]
    expect_gt(peak, min(ages)) ; expect_lt(peak, max(ages))
    expect_true(peak >= 40 && peak <= 60,
                info = sprintf("%s - peak clinical FTE at age %d is implausible", lbl, peak))

    # A real late-career taper: the last five years average well below the peak.
    expect_lt(mean(utils::tail(w, 5)), max(w) * 0.6,
              label = paste(lbl, "- no meaningful late-career taper"))
    # ...and an early ramp, which is what makes it unimodal rather than flat.
    expect_lt(w[1], max(w),
              label = paste(lbl, "- no early-career ramp; curve is not unimodal"))
  }
})

test_that("p_still_active agrees with the curve it is drawn from", {
  info <- production_info(); if (!info$available) skip_recorded("production absent")
  # Two entry points to the same quantity. Consumers use both; if they disagree,
  # two published numbers describe the same physician differently.
  s <- mufflyaccess::urps_survival_curve(sex = "female", pathway = "ABOG")
  for (a in c(40L, 50L, 60L, 70L)) {
    if (!a %in% s$age) next
    direct <- as.numeric(mufflyaccess::urps_p_still_active(a, "female", "ABOG"))
    from_curve <- s$p_still_active[match(a, s$age)]
    expect_equal(direct, from_curve, tolerance = 1e-10,
                 label = sprintf("age %d: accessor vs curve", a))
  }
})
