#!/usr/bin/env Rscript
#' Tests for R/name_matching_primitives.R
#'
#' Fixture provenance is labelled throughout:
#'   observed             — appeared in the AMCB midwifery linkage output
#'   adjudicated observed — appeared in that output AND was inspected by hand
#'   synthetic/adversarial — constructed to exercise a rule; not from a cohort
#'
#' The distinction matters: an illustrative example must never be mistaken for
#' measured cohort evidence by a later reader.

library(testthat)
library(here)
library(stringr)

.np_ok <- tryCatch({
  for (f in c("string_normalization.R", "nickname_system.R",
              "enhanced_name_parsing.R", "name_matching_primitives.R")) {
    suppressMessages(suppressWarnings(source(here::here("R", f))))
  }
  TRUE
}, error = function(e) {
  message("SKIP reason: ", conditionMessage(e)); FALSE
})

# =============================================================================
# name_surname_components()
# =============================================================================

test_that("name_surname_components splits hyphen and whitespace identically", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_equal(name_surname_components("Barlow-Reed")[[1]], c("BARLOW", "REED"),
               label = "Hyphenated compound must split into whole components")
  expect_equal(name_surname_components("Barlow Reed")[[1]], c("BARLOW", "REED"),
               label = "Unhyphenated compound must split identically — sources disagree on the hyphen")
})

test_that("name_surname_components retains particles", {
  skip_if(!.np_ok, "primitives failed to source")
  # observed (AMCB roster): "Sybelle B.E. van Erven"
  expect_equal(name_surname_components("van Erven")[[1]], c("VAN", "ERVEN"),
               label = "Particles carry identity information and must be retained as components")
})

test_that("name_surname_components normalizes case and diacritics internally", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_equal(name_surname_components("larode")[[1]],
               name_surname_components("LaRodé")[[1]],
               label = "Mixed case and diacritics must fold — the caller cannot be required to pre-normalize")
})

# =============================================================================
# names_have_compatible_surname()  — the containment defect
# =============================================================================

test_that("names_have_compatible_surname rejects substring containment", {
  skip_if(!.np_ok, "primitives failed to source")
  # adjudicated observed: all four attributed one person's record to another
  expect_false(names_have_compatible_surname("Anderson", "Sanderson"),
               label = "ANDERSON is contained in SANDERSON but they are different families")
  expect_false(names_have_compatible_surname("Williams", "Williamson"),
               label = "WILLIAMS is contained in WILLIAMSON but they are different families")
  expect_false(names_have_compatible_surname("Martin", "Martinez"),
               label = "MARTIN is contained in MARTINEZ but they are different families")
  expect_false(names_have_compatible_surname("Erven", "Ervenson"),
               label = "Containment must fail regardless of which side is longer")
})

test_that("names_have_compatible_surname accepts component subsets", {
  skip_if(!.np_ok, "primitives failed to source")
  # observed: married/hyphenated surnames are the largest source of legitimate
  # disagreement between a roster and an external source
  expect_true(names_have_compatible_surname("Nelson", "Nelson-Becker"),
              label = "A roster surname may be a component of a married surname")
  expect_true(names_have_compatible_surname("Dyer", "Dyer Hill"),
              label = "Unhyphenated compound: DYER is a component of DYER HILL")
  expect_true(names_have_compatible_surname("O'Connor", "Oconnor"),
              label = "Apostrophe handling must not split the surname into fragments")
  # observed (isochrones Healthgrades verifier): roster hyphenated, profile compact
  expect_true(names_have_compatible_surname("Abu-Ghazaleh", "Abughazaleh"),
              label = "A source may drop the separator entirely; concatenation must still match")
  expect_false(names_have_compatible_surname("Ander-Son", "Sanderson"),
              label = "Concatenation is EXACT equality, never containment: ANDERSON != SANDERSON")
})

test_that("a particle alone is not surname evidence", {
  skip_if(!.np_ok, "primitives failed to source")
  # synthetic/adversarial: component subset would otherwise make a grammatical
  # connective sufficient, recreating a collision mechanism in the canonical layer
  expect_false(names_have_compatible_surname("Van", "van Erven"),
               label = "VAN is a particle, not a family name — it must not establish compatibility")
  expect_false(names_have_compatible_surname("De", "de la Cruz"),
               label = "DE alone identifies nobody")
  expect_false(names_have_compatible_surname("La", "de la Cruz"),
               label = "LA alone identifies nobody")
  # and the real surname component still matches
  expect_true(names_have_compatible_surname("Erven", "van Erven"),
              label = "ERVEN is the family name and must match")
  expect_true(names_have_compatible_surname("Cruz", "de la Cruz"),
              label = "CRUZ is the family name and must match")
})

test_that("name_surname_match_type reports evidence strength, not just TRUE", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_equal(name_surname_match_type("Smith", "Smith"), "exact")
  expect_equal(name_surname_match_type("Barlow-Reed", "Barlow Reed"), "separator_equivalent",
               label = "Same components, different separators — weaker than exact, stronger than subset")
  expect_equal(name_surname_match_type("Abu-Ghazaleh", "Abughazaleh"), "concatenated_equivalent")
  expect_equal(name_surname_match_type("Nelson", "Nelson-Becker"), "component_subset",
               label = "Subset is the weakest positive evidence and must be distinguishable")
  expect_equal(name_surname_match_type("Anderson", "Sanderson"), "none")
  expect_equal(name_surname_match_type("Van", "van Erven"), "none")
})

test_that("identity vectors are never silently recycled", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_error(names_have_compatible_surname(c("Smith","Jones"), c("Smith","Jones","Brown")),
               "Refusing to recycle",
               label = "Comparing 2 people against 3 must error, not fabricate a third pairing")
  expect_error(names_have_compatible_given(c("Mary","Jane"), c("Mary","Jane","Ann"),
                                           mode = "positional_ie"),
               "Refusing to recycle")
  expect_error(name_given_match_position(c("Mary","Jane"), c("Mary","Jane","Ann")),
               "Refusing to recycle")
  expect_error(name_given_tokens(c("Mary","Jane","Ann"), c("A","B")),
               "Refusing to recycle",
               label = "A middle vector that does not correspond to the first names must error")
  # scalar broadcast remains legal and elementwise
  expect_equal(names_have_compatible_surname("Smith", c("Smith","Jones")), c(TRUE, FALSE),
               label = "Length-1 broadcasting is deliberate and must still work")
})

test_that("first defines the record count; a longer middle vector errors", {
  skip_if(!.np_ok, "primitives failed to source")
  # Silent truncation, not broadcasting: rep_len() would keep ANN and discard
  # two records entirely.
  expect_error(name_given_tokens("Mary", c("Ann", "Jane")), "Refusing to recycle",
               label = "A middle vector longer than the first-name vector must error, not truncate")
  expect_error(name_given_match_position("Mary", "Mary", a_middle = c("Ann", "Jane")),
               "Refusing to recycle")
  expect_error(names_have_compatible_given("Mary", "Mary", a_middle = c("Ann", "Jane"),
                                           mode = "any_token"),
               "Refusing to recycle")
  # the legitimate direction: one middle value broadcast across several records
  expect_equal(length(name_given_tokens(c("Mary", "Jane"), "Ann")), 2L,
               label = "One middle value across several first names is deliberate broadcasting")
})

test_that("a zero-length vector is not a scalar", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_equal(length(names_have_compatible_surname(character(0), character(0))), 0L,
               label = "Both empty yields an empty result")
  expect_error(names_have_compatible_surname(character(0), "Smith"), "Refusing to recycle",
               label = "Empty against non-empty must error, not broadcast nothing across everything")
})

test_that("names_have_compatible_surname fails closed on NA and empty", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_false(names_have_compatible_surname(NA_character_, "Smith"),
               label = "Absence is never evidence — NA must not match")
  expect_false(names_have_compatible_surname("", "Smith"),
               label = "Empty string must not match; nzchar(NA) is TRUE and must never be the test")
  expect_false(names_have_compatible_surname(NA_character_, NA_character_),
               label = "Two absences must not agree with each other")
})

# =============================================================================
# name_given_tokens() / name_leading_given()
# =============================================================================

test_that("name_given_tokens excludes initials but name_leading_given retains them", {
  skip_if(!.np_ok, "primitives failed to source")
  # adjudicated observed: "Dowdle, S. Addreina" vs "Shaquinda Addreina Dowdle"
  expect_equal(name_given_tokens("S. Addreina")[[1]], "ADDREINA",
               label = "An initial identifies nobody and must not enter a set compared by intersection")
  expect_equal(name_leading_given("S. Addreina"), "S",
               label = "The leading given must retain the initial — dropping it promotes the middle name")
})

test_that("name_given_tokens is case invariant", {
  skip_if(!.np_ok, "primitives failed to source")
  # This is the exact defect that made a downstream tokenizer return zero
  # tokens for mixed-case input and silently match nobody.
  expect_equal(name_given_tokens("mary jane")[[1]], name_given_tokens("MARY JANE")[[1]],
               label = "Mixed-case input must tokenize identically to uppercase")
  expect_gt(length(name_given_tokens("mary jane")[[1]]), 0,
            label = "Mixed-case input must not silently produce zero tokens")
})

test_that("name_leading_given returns NA for absent input", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_true(is.na(name_leading_given(NA_character_)))
  expect_true(is.na(name_leading_given("")))
})

# =============================================================================
# names_have_compatible_given(mode = "positional_ie")
# =============================================================================

test_that("positional_ie accepts equality and nickname variants", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_true(names_have_compatible_given("Mary", "Mary", mode = "positional_ie"))
  # observed: Beth/Elizabeth was logged downstream as a permanent miss until the
  # canonical nickname dictionary was consulted
  expect_true(names_have_compatible_given("Beth", "Elizabeth", mode = "positional_ie"),
              label = "Canonical nickname equivalence must be honoured on full tokens")
})

test_that("positional_ie performs initial expansion", {
  skip_if(!.np_ok, "primitives failed to source")
  # adjudicated observed: both were real class-1 links recovered by this rule
  expect_true(names_have_compatible_given("C", "Chad", mode = "positional_ie"),
              label = "C. expands to Chad — an expanded initial is not a collision")
  expect_true(names_have_compatible_given("Shaquinda", "S", mode = "positional_ie"),
              label = "Expansion must work in both directions")
})

test_that("positional_ie never lets initials alone identify a person", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_false(names_have_compatible_given("S", "S", mode = "positional_ie"),
               label = "Two agreeing initials identify nobody — S. matches every S")
  expect_false(names_have_compatible_given("C", "S", mode = "positional_ie"),
               label = "Disagreeing initials must fail")
})

test_that("positional_ie rejects first-name containment accidents", {
  skip_if(!.np_ok, "primitives failed to source")
  # adjudicated observed: two certificants were matched to one profile this way
  expect_false(names_have_compatible_given("Linda", "Melinda", mode = "positional_ie"),
               label = "LINDA is contained in MELINDA but they are different people")
  expect_false(names_have_compatible_given("Elissa", "Melissa", mode = "positional_ie"),
               label = "ELISSA is contained in MELISSA but they are different people")
})

test_that("positional_ie rejects a first name that is the other's middle name", {
  skip_if(!.np_ok, "primitives failed to source")
  # adjudicated observed: "Anderson, Elizabeth" vs "Annagrace Elizabeth Anderson"
  expect_false(names_have_compatible_given("Elizabeth", "Annagrace",
                                           a_middle = NULL, b_middle = "Elizabeth",
                                           mode = "positional_ie"),
               label = "A shared middle name must not satisfy a positional comparison")
})

test_that("positional_ie fails closed on missing given names", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_false(names_have_compatible_given(NA_character_, "Mary", mode = "positional_ie"))
  expect_false(names_have_compatible_given("", "Mary", mode = "positional_ie"))
})

# =============================================================================
# names_have_compatible_given(mode = "any_token") + position evidence
# =============================================================================

test_that("any_token admits a shared full token regardless of position", {
  skip_if(!.np_ok, "primitives failed to source")
  # synthetic/adversarial: the reordered-byline case that motivates the mode.
  # NOTE: this exact pattern did NOT occur in the 50 observed cases; the
  # observed rescues were expanded initials and preferred middle names.
  expect_true(names_have_compatible_given("W. Jon", "Jon", a_middle = NULL,
                                          b_middle = "W", mode = "any_token"),
              label = "Reordered author strings share JON and must remain reachable")
  # adjudicated observed: publishes under her middle name
  expect_true(names_have_compatible_given("Marian Vanita", "Vanita",
                                          mode = "any_token"),
              label = "Preferred-middle-name publication is only reachable under any_token")
})

test_that("any_token still requires a FULL token, never an initial", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_false(names_have_compatible_given("W", "W", mode = "any_token"),
               label = "A shared initial must not admit a candidate in any mode")
})

test_that("name_given_match_position keeps middle-only matches distinguishable", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_equal(name_given_match_position("Mary", "Mary"), "both_leading")
  # adjudicated observed: "Jones, Mary K." vs "Angela Mary Jones"
  expect_equal(name_given_match_position("Mary", "Angela", NULL, "Mary"),
               "one_leading",
               label = "A first-vs-middle agreement must be labelled, not silently accepted as strong")
  expect_equal(name_given_match_position("Ann Marie", "Jane Marie"),
               "neither_leading",
               label = "A middle-only agreement must be distinguishable so it is never promoted")
  expect_true(is.na(name_given_match_position("Mary", "Jane")),
              label = "No shared token means no position")
})

# =============================================================================
# Invariants
# =============================================================================

test_that("primitives are vectorized and order invariant", {
  skip_if(!.np_ok, "primitives failed to source")
  a <- c("Nelson", "Anderson", "Dyer")
  b <- c("Nelson-Becker", "Sanderson", "Dyer Hill")
  fwd <- names_have_compatible_surname(a, b)
  expect_equal(fwd, c(TRUE, FALSE, TRUE),
               label = "Vectorized comparison must be elementwise")
  idx <- c(3L, 1L, 2L)
  expect_equal(names_have_compatible_surname(a[idx], b[idx]), fwd[idx],
               label = "Row order must not change any result")
})

test_that("surname compatibility is symmetric", {
  skip_if(!.np_ok, "primitives failed to source")
  expect_equal(names_have_compatible_surname("Nelson", "Nelson-Becker"),
               names_have_compatible_surname("Nelson-Becker", "Nelson"),
               label = "Subset in EITHER direction — argument order must not matter")
})

test_that("mode must be stated and is never inferred", {
  skip_if(!.np_ok, "primitives failed to source")
  # The two modes must disagree on exactly the case that distinguishes them,
  # proving the caller's choice is doing real work.
  expect_false(names_have_compatible_given("Elizabeth", "Annagrace", NULL,
                                           "Elizabeth", mode = "positional_ie"))
  expect_true(names_have_compatible_given("Elizabeth", "Annagrace", NULL,
                                          "Elizabeth", mode = "any_token"))
  expect_error(names_have_compatible_given("Mary", "Mary", mode = "guess"),
               label = "An unknown mode must error rather than fall back to a default")
  # OMISSION is the case that matters: mode = c("a","b") + match.arg() would
  # silently select the first option, defeating the explicit-mode contract.
  expect_error(names_have_compatible_given("Mary", "Mary"),
               "mode must be explicitly specified",
               label = "Omitting mode must error, never default to positional_ie")
})
