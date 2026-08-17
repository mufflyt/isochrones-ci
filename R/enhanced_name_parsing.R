#' @title Enhanced Name Parsing for NPI Matching
#'
#' @description
#' Handles complex name variations, nicknames, and international names to
#' improve match rates in the ABOG-NPI pipeline.
#'
#' @family step1-name-matching
#' @name enhanced_name_parsing
NULL

#!/usr/bin/env Rscript
# =============================================================================
# Enhanced Name Parsing for NPI Matching
# =============================================================================
# Purpose: Handle complex name variations, nicknames, and international names
# Author: Tyler Muffly & Claude Code
# Date: 2025-11-05
# =============================================================================

# BUG FIX #7 (2026-01-28): Load string normalization library
# Replace repeated toupper(trimws()) calls with pre-normalized strings
source(here::here("R", "string_normalization.R"))

#' Common nickname mappings
#'
#' @return Named list of formal names to nickname vectors
#' @export
get_nickname_map <- function() {
  list(
    # Male names
    "WILLIAM" = c("BILL", "BILLY", "WILL", "WILLIE"),
    "ROBERT" = c("BOB", "BOBBY", "ROB", "ROBBIE"),
    "RICHARD" = c("RICK", "RICKY", "DICK", "RICH"),
    "JAMES" = c("JIM", "JIMMY", "JAMIE"),
    "JOHN" = c("JACK", "JOHNNY", "JACK"),
    "MICHAEL" = c("MIKE", "MIKEY", "MICK"),
    "THOMAS" = c("TOM", "TOMMY", "THOM"),
    "CHARLES" = c("CHARLIE", "CHUCK", "CHAS"),
    "JOSEPH" = c("JOE", "JOEY"),
    "CHRISTOPHER" = c("CHRIS", "TOPHER"),
    "DANIEL" = c("DAN", "DANNY"),
    "MATTHEW" = c("MATT", "MATTY"),
    "ANTHONY" = c("TONY", "ANT"),
    "DAVID" = c("DAVE", "DAVEY"),
    "DONALD" = c("DON", "DONNIE"),
    "STEVEN" = c("STEVE", "STEVIE"),
    "EDWARD" = c("ED", "EDDIE", "TED", "TEDDY"),
    "ANDREW" = c("ANDY", "DREW"),
    "KENNETH" = c("KEN", "KENNY"),
    "TIMOTHY" = c("TIM", "TIMMY"),
    "JEFFREY" = c("JEFF", "JEFFERY"),
    "LAWRENCE" = c("LARRY", "LANCE"),
    "NICHOLAS" = c("NICK", "NICKY"),
    "BENJAMIN" = c("BEN", "BENNY", "BENJI"),
    "SAMUEL" = c("SAM", "SAMMY"),
    "GREGORY" = c("GREG", "GREGG"),
    "RAYMOND" = c("RAY"),
    "ALEXANDER" = c("ALEX", "SANDY", "SASHA"),
    "PATRICK" = c("PAT", "PATTY", "RICK"),
    "JONATHAN" = c("JON", "JONNY", "NATHAN"),

    # Female names
    "ELIZABETH" = c("LIZ", "LIZZY", "BETH", "BETTY", "BETSY", "ELIZA"),
    "MARGARET" = c("MAGGIE", "PEGGY", "MEG", "MARGE", "MARGIE"),
    "CATHERINE" = c("CATHY", "KATE", "KATIE", "KIT", "KITTY"),
    "KATHERINE" = c("KATHY", "KATE", "KATIE", "KIT", "KITTY"),
    "JENNIFER" = c("JENNY", "JEN", "JENN"),
    "PATRICIA" = c("PAT", "PATTY", "TRICIA", "TRISH"),
    "BARBARA" = c("BARB", "BARBIE", "BABS"),
    "SUSAN" = c("SUE", "SUZY", "SUSIE"),
    "JESSICA" = c("JESS", "JESSIE"),
    "SARAH" = c("SALLY", "SADIE"),
    "REBECCA" = c("BECKY", "BECCA"),
    "MICHELLE" = c("SHELLY", "SHELLEY", "MICH"),
    "KIMBERLY" = c("KIM", "KIMMY"),
    "AMANDA" = c("MANDY", "MANDA"),
    "STEPHANIE" = c("STEPH", "STEFFI"),
    "CHRISTINA" = c("CHRIS", "CHRISTY", "TINA"),
    "CHRISTINE" = c("CHRIS", "CHRISTY", "TINA"),
    "DEBORAH" = c("DEB", "DEBBIE", "DEBBY"),
    "RACHEL" = c("RAE", "RACH"),
    "MELISSA" = c("MISSY", "MEL", "LISSA"),
    "VICTORIA" = c("VICKY", "TORI", "VIC"),
    "ALEXANDRA" = c("ALEX", "SANDY", "LEXIE"),
    "SAMANTHA" = c("SAM", "SAMMY"),
    "CAROLYN" = c("CAROL", "CARRIE", "LYNN"),
    "JACQUELINE" = c("JACKIE", "JACQUE"),
    "DOROTHY" = c("DOT", "DOTTY", "DOTTIE"),

    # Gender-neutral
    "LESLIE" = c("LES"),
    "ROBIN" = c("ROB", "ROBBY")
  )
}

#' Check if two names are nickname variants
#'
#' @param name1 `character`: first name
#' @param name2 `character`: first name
#' @return Logical TRUE if nickname match
#' @export
are_nickname_variants <- function(name1, name2) {
  # BUG FIX #7 (2026-01-28): Use normalize_string() instead of toupper(trimws())
  name1_clean <- normalize_string(name1)
  name2_clean <- normalize_string(name2)

  if (name1_clean == name2_clean) {
    return(TRUE)  # Exact match
  }

  nickname_map <- get_nickname_map()

  # Check if name1 is formal and name2 is nickname
  if (name1_clean %in% names(nickname_map)) {
    if (name2_clean %in% nickname_map[[name1_clean]]) {
      return(TRUE)
    }
  }

  # Check if name2 is formal and name1 is nickname
  if (name2_clean %in% names(nickname_map)) {
    if (name1_clean %in% nickname_map[[name2_clean]]) {
      return(TRUE)
    }
  }

  # Check if both are nicknames of the same formal name
  for (formal_name in names(nickname_map)) {
    nicknames <- c(formal_name, nickname_map[[formal_name]])
    if (name1_clean %in% nicknames && name2_clean %in% nicknames) {
      return(TRUE)
    }
  }

  return(FALSE)
}

#' Score name match with nickname flexibility
#'
#' @param abog_first `character`: ABOG first name
#' @param npi_first `character`: NPI first name
#' @return Numeric score (0-30 points)
#' @export
score_name_with_nicknames <- function(abog_first, npi_first) {
  # BUG FIX #7 (2026-01-28): Use normalize_string() instead of toupper(trimws())
  abog_clean <- normalize_string(abog_first)
  npi_clean <- normalize_string(npi_first)

  if (abog_clean == npi_clean) {
    # Exact match
    return(30)
  }

  if (are_nickname_variants(abog_clean, npi_clean)) {
    # Nickname match - very strong signal
    return(25)
  }

  # No match
  return(0)
}

#' Handle hyphenated last names
#'
#' Physicians may use full hyphenated name or just one component
#' Example: "Smith-Jones" vs "Smith" vs "Jones"
#'
#' @param name1 `character`: last name
#' @param name2 `character`: last name
#' @return List with match status and explanation
#' @export
compare_hyphenated_names <- function(name1, name2) {
  # BUG FIX #7 (2026-01-28): Use normalize_string() instead of toupper(trimws())
  name1_clean <- normalize_string(name1)
  name2_clean <- normalize_string(name2)

  if (name1_clean == name2_clean) {
    return(list(
      match = "exact",
      score = 30,
      explanation = "Exact last name match"
    ))
  }

  # Check if either name is hyphenated
  name1_has_hyphen <- grepl("-", name1_clean)
  name2_has_hyphen <- grepl("-", name2_clean)

  if (name1_has_hyphen && !name2_has_hyphen) {
    # name1 is "Smith-Jones", name2 is "Smith" or "Jones"?
    components <- strsplit(name1_clean, "-")[[1]]
    if (name2_clean %in% components) {
      return(list(
        match = "partial_hyphen",
        score = 25,
        explanation = sprintf("Hyphenated name '%s' contains '%s'", name1_clean, name2_clean)
      ))
    }
  }

  if (!name1_has_hyphen && name2_has_hyphen) {
    # name2 is "Smith-Jones", name1 is "Smith" or "Jones"?
    components <- strsplit(name2_clean, "-")[[1]]
    if (name1_clean %in% components) {
      return(list(
        match = "partial_hyphen",
        score = 25,
        explanation = sprintf("Hyphenated name '%s' contains '%s'", name2_clean, name1_clean)
      ))
    }
  }

  if (name1_has_hyphen && name2_has_hyphen) {
    # Both hyphenated - check if any components overlap
    components1 <- strsplit(name1_clean, "-")[[1]]
    components2 <- strsplit(name2_clean, "-")[[1]]
    overlap <- intersect(components1, components2)

    if (length(overlap) > 0) {
      return(list(
        match = "hyphen_overlap",
        score = 20,
        explanation = sprintf("Both hyphenated with overlap: %s", paste(overlap, collapse = ", "))
      ))
    }
  }

  # No match
  return(list(
    match = "none",
    score = 0,
    explanation = "No last name match"
  ))
}

#' Handle maiden name scenarios
#'
#' Women physicians may change their last name due to marriage
#' This is hard to detect without external data, but we can flag
#' suspicious cases for manual review
#'
#' @param abog_name `list`: with first, middle, last
#' @param npi_name `list`: with first, middle, last
#' @return List with maiden name assessment
#' @export
assess_potential_maiden_name <- function(abog_name, npi_name) {
  # If first name and middle initial match exactly, but last name differs,
  # this could be a maiden name situation (especially for women)

  # BUG FIX #7 (2026-01-28): Pre-normalize strings once instead of repeated toupper(trimws())
  # Old code called toupper(trimws()) 4 times in 2 lines (major performance issue)
  abog_first_clean <- normalize_string(abog_name$first)
  npi_first_clean <- normalize_string(npi_name$first)
  abog_last_clean <- normalize_string(abog_name$last)
  npi_last_clean <- normalize_string(npi_name$last)

  first_match <- abog_first_clean == npi_first_clean
  last_match <- abog_last_clean == npi_last_clean

  if (!first_match || last_match) {
    # Either first name doesn't match, or last name already matches
    # Not a maiden name scenario
    return(list(
      is_potential_maiden_name = FALSE,
      confidence = 0,
      note = "Not applicable"
    ))
  }

  # First name matches, last name doesn't
  # Check middle name
  abog_middle_clean <- normalize_string(abog_name$middle)
  npi_middle_clean <- normalize_string(npi_name$middle)

  abog_has_middle <- !is.na(abog_middle_clean) && abog_middle_clean != ""
  npi_has_middle <- !is.na(npi_middle_clean) && npi_middle_clean != ""

  if (abog_has_middle && npi_has_middle) {
    abog_middle_init <- substr(abog_middle_clean, 1, 1)
    npi_middle_init <- substr(npi_middle_clean, 1, 1)

    if (abog_middle_init == npi_middle_init) {
      # First name exact, middle initial exact, last name different
      # High probability of maiden name
      return(list(
        is_potential_maiden_name = TRUE,
        confidence = 0.8,
        note = sprintf(
          "First + middle match (%s %s.), last name differs (%s vs %s) - possible maiden name",
          abog_name$first, abog_middle_init, abog_name$last, npi_name$last
        )
      ))
    }
  }

  # First name match, last name differs, but can't verify middle
  # Moderate probability
  return(list(
    is_potential_maiden_name = TRUE,
    confidence = 0.4,
    note = sprintf(
      "First name matches (%s), last name differs (%s vs %s) - possible maiden name but less certain",
      abog_name$first, abog_name$last, npi_name$last
    )
  ))
}

#' Handle international name variations
#'
#' International names may have variations in spelling, diacritics, etc.
#' Examples: Jose vs José, Mueller vs Müller
#'
#' @param name1 `character`: name
#' @param name2 `character`: name
#' @return List with international name assessment
#' @export
compare_international_names <- function(name1, name2) {
  # Handle NA/empty inputs
  if (is.na(name1) || is.na(name2) || name1 == "" || name2 == "") {
    return(list(
      match = FALSE,
      score = 0,
      explanation = "Missing name data"
    ))
  }

  # Remove diacritics and accents using iconv
  name1_ascii <- tryCatch({
    iconv(name1, from = "UTF-8", to = "ASCII//TRANSLIT")
  }, error = function(e) {
    # If iconv fails, use original
    name1
  })

  name2_ascii <- tryCatch({
    iconv(name2, from = "UTF-8", to = "ASCII//TRANSLIT")
  }, error = function(e) {
    # If iconv fails, use original
    name2
  })

  # Handle NULL returns from iconv
  if (is.null(name1_ascii)) name1_ascii <- name1
  if (is.null(name2_ascii)) name2_ascii <- name2

  # BUG FIX #7 (2026-01-28): Use normalize_string() instead of toupper(trimws())
  name1_clean <- normalize_string(name1_ascii)
  name2_clean <- normalize_string(name2_ascii)

  if (name1_clean == name2_clean) {
    return(list(
      match = TRUE,
      score = 30,
      explanation = "Match after removing diacritics"
    ))
  }

  # Common international variations
  variations <- list(
    # German umlauts
    c("UE", "Ü"), c("AE", "Ä"), c("OE", "Ö"),
    # Spanish
    c("N", "Ñ"),
    # French
    c("C", "Ç")
  )

  for (var in variations) {
    name1_var <- gsub(var[1], var[2], name1_clean)
    name2_var <- gsub(var[1], var[2], name2_clean)

    if (name1_var == name2_clean || name1_clean == name2_var) {
      return(list(
        match = TRUE,
        score = 28,
        explanation = sprintf("Match with common variation (%s/%s)", var[1], var[2])
      ))
    }
  }

  # No match
  return(list(
    match = FALSE,
    score = 0,
    explanation = "No international variation match"
  ))
}

#' Calculate comprehensive name parsing bonus
#'
#' Combines all enhanced name parsing features
#'
#' @param abog_name `list`: with first, middle, last
#' @param npi_name `list`: with first, middle, last
#' @return Numeric bonus points
#' @export
calculate_name_parsing_bonus <- function(abog_name, npi_name) {
  total_bonus <- 0

  # Component 1: Nickname matching
  nickname_score <- score_name_with_nicknames(abog_name$first, npi_name$first)
  if (nickname_score >= 25 && nickname_score < 30) {
    # Nickname match (not exact) - add small bonus
    total_bonus <- total_bonus + 5
  }

  # Component 2: Hyphenated last names
  hyphen_result <- compare_hyphenated_names(abog_name$last, npi_name$last)
  if (hyphen_result$match == "partial_hyphen") {
    # Partial hyphen match - add bonus
    total_bonus <- total_bonus + 10
  } else if (hyphen_result$match == "hyphen_overlap") {
    # Hyphen overlap - smaller bonus
    total_bonus <- total_bonus + 5
  }

  # Component 3: International name variations
  int_result <- compare_international_names(abog_name$first, npi_name$first)
  if (int_result$match && int_result$score < 30) {
    # International variation match - add bonus
    total_bonus <- total_bonus + 8
  }

  return(total_bonus)
}

#' Get detailed name parsing assessment for logging
#'
#' @param abog_name `list`: with first, middle, last
#' @param npi_name `list`: with first, middle, last
#' @return List with detailed assessment
#' @export
assess_name_variations <- function(abog_name, npi_name) {
  nickname_score <- score_name_with_nicknames(abog_name$first, npi_name$first)
  hyphen_result <- compare_hyphenated_names(abog_name$last, npi_name$last)
  int_result <- compare_international_names(abog_name$first, npi_name$first)
  maiden_result <- assess_potential_maiden_name(abog_name, npi_name)

  total_bonus <- calculate_name_parsing_bonus(abog_name, npi_name)

  flags <- c()
  if (nickname_score >= 25 && nickname_score < 30) {
    flags <- c(flags, "nickname_variant")
  }
  if (hyphen_result$match != "none") {
    flags <- c(flags, "hyphenated_name")
  }
  if (int_result$match) {
    flags <- c(flags, "international_variant")
  }
  if (maiden_result$is_potential_maiden_name) {
    flags <- c(flags, "possible_maiden_name")
  }

  return(list(
    nickname = list(
      score = nickname_score,
      is_variant = nickname_score >= 25 && nickname_score < 30
    ),
    hyphenated = hyphen_result,
    international = int_result,
    maiden_name = maiden_result,
    total_bonus = total_bonus,
    flags = flags,
    summary = if (length(flags) > 0) {
      sprintf("Name parsing bonus: %+d (%s)", total_bonus, paste(flags, collapse = ", "))
    } else {
      sprintf("Name parsing bonus: %+d (standard match)", total_bonus)
    }
  ))
}
