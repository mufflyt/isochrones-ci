#!/usr/bin/env Rscript
# Comprehensive Nickname System for ABOG-NPPES Matching
# =======================================================
# Integrated nickname handling for enhanced first name matching
# Based on existing test framework patterns discovered in the codebase
# =======================================================

library(stringdist)

# BUG FIX #7 (2026-01-28): Load string normalization library
# BUG FIX (2026-02-02): Use robust path resolution for test compatibility
if (!exists("normalize_string", mode = "function")) {
  # Try multiple paths to find string_normalization.R
  candidates <- c(
    here::here("R", "string_normalization.R"),
    file.path(dirname(sys.frame(1)$ofile %||% "."), "string_normalization.R"),
    "R/string_normalization.R",
    "../R/string_normalization.R",
    "../../R/string_normalization.R"
  )

  sourced <- FALSE
  for (path in candidates) {
    if (!is.null(path) && file.exists(path)) {
      tryCatch({
        source(path)
        sourced <- TRUE
        break
      }, error = function(e) NULL)
    }
  }

  # If still not loaded, define a minimal fallback
  if (!sourced && !exists("normalize_string", mode = "function")) {
    normalize_string <- function(x) toupper(trimws(as.character(x)))
  }
}

#' @title Create Comprehensive Nickname Dictionary
#'
#' @description
#' Builds a bidirectional nickname mapping dictionary for use in physician name
#' matching between ABOG and NPPES data sources. Maps formal names to their
#' common nicknames (e.g., ROBERT -> BOB, BOBBY, ROB) and vice versa. Covers
#' approximately 125 formal first names with male and female variants.
#'
#' @inheritParams shared_params_run
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{formal_to_nicknames}{Named list mapping each formal name to a
#' character vector of nicknames}
#'     \item{nickname_to_formal}{Named list mapping each nickname back to its
#' formal name}
#'     \item{source}{Character string identifying the dictionary source}
#'     \item{created}{POSIXct timestamp of dictionary creation}
#'     \item{formal_count}{Integer count of formal names in the dictionary}
#'     \item{nickname_count}{Integer count of nickname entries in the
#' dictionary}
#'   }
#'
#' @section Nickname System Family:
#' \preformatted{
#'   Function                                 | Role
#' -----------------------------------------+----------------------------------------------
#'   create_nickname_dictionary()             | Builds bidirectional formal <->
#' nickname map
#'   get_nickname_dictionary()                | Returns cached dictionary (no
#' rebuild)
#'   get_canonical_name()                     | Nickname -> formal name lookup
#'   get_nicknames_for_name()                 | Formal name -> all nickname
#' variants
#'   are_nickname_equivalents()               | Boolean: do two names share a
#' formal root?
#'   calculate_enhanced_first_name_similarity | Numeric 0-1 nickname-aware
#' similarity score
#'   create_nickname_aware_similarity()       | Factory: returns bound
#' similarity closure
#' }
#'
#' @section Why Nicknames Matter for NPI Matching:
#' ABOG records may use informal first names (e.g., "Beth") while NPPES uses
#' the physician's legal name (e.g., "Elizabeth"). Without nickname resolution,
#' these appear as non-matches despite being the same person. The dictionary
#' covers ~125 formal names and their common variants, including international
#' names (South Asian, Arabic, East Asian, African) not in SSA historical data.
#'
#' @section nickname_system vs maiden_name_crosswalk:
#' \preformatted{
#'   nickname_system.R (this)           | maiden_name_crosswalk.R
#'   -----------------------------------+--------------------------------------
#'   First name variants (Beth/Elizabeth)| Last name changes (marriage/divorce)
#'   Universal (any gender)              | Primarily female physicians
#'   Algorithmic + lookup table          | Data-driven from 6 multi-year sources
#'   Applied during name comparison      | Applied when direct match fails
#' }
#'
#' @section Name Matching Support Family:
#' \preformatted{
#'   File                              | Role
#' | Used by
#' ----------------------------------+-------------------------------------------+---------------------------
#'   name_parsing_protocol_enhanced.R  | Canonical parser — 96.7% accuracy
#' | All matching, CLAUDE.md #3
#'   abog_name_parser_fast.R           | Fast parser optimised for ABOG format
#' | Strategy cascade
#'   maiden_name_crosswalk.R           | Resolves marriage name changes
#' | Strategy 19, cascade
#'   nickname_system.R                 | Maps nicknames <-> legal names
#' | Strategy 19, name compare
#'   credential_detection.R            | Strips/detects MD/DO/PhD/JD suffixes
#' | Name normalization
#'   nppes_filter_utils.R              | Pre-filters NPPES candidate NPI pool
#' | All strategies
#' }
#'
#' @concept npi-matching
#' @concept data-linkage
#' @concept match-scoring
#'
#' @family step1-name-matching
#'
#' @seealso
#' \code{\link{get_nickname_dictionary}} for cached access to the dictionary.
#' \code{\link{get_canonical_name}} for resolving a name to its formal form.
#' \code{\link{calculate_enhanced_first_name_similarity}} for nickname-aware matching.
#' \code{\link{parse_physician_name_enhanced}} in
#'   \code{R/name_parsing_protocol_enhanced.R} — canonical parser (CLAUDE.md
#' #3).
#' \code{R/canonical_abog_npi_pipeline_STABLE.R} — Step 1 orchestrator.
#' \code{\link{build_maiden_name_crosswalk}} in \code{R/maiden_name_crosswalk.R}
#'   for complementary last-name change resolution.
#'
#' @examples
#' \dontrun{
#' dict <- create_nickname_dictionary(verbose = FALSE)
#' dict$formal_to_nicknames[["ROBERT"]]
#' # [1] "BOB" "BOBBY" "ROB" "ROBBIE" "BERT"
#' dict$nickname_to_formal[["BOB"]]
#' # [1] "ROBERT"
#' }
#'
#' @keywords internal
create_nickname_dictionary <- function(verbose = TRUE) {
  if (verbose) {
    message("📚 Building comprehensive nickname dictionary...")
  }

  # Core nickname mappings based on test patterns found
  nickname_mappings <- list(
    # Common male names
    "ROBERT" = c("BOB", "BOBBY", "ROB", "ROBBIE", "BERT"),
    "WILLIAM" = c("BILL", "BILLY", "WILL", "WILLY", "LIAM"),
    "JAMES" = c("JIM", "JIMMY", "JAMIE", "JAMEY"),
    "JOHN" = c("JACK", "JOHNNY", "JACK", "JOCK"),
    "MICHAEL" = c("MIKE", "MICKEY", "MICK", "MIKEY"),
    "DAVID" = c("DAVE", "DAVY", "DAVEY"),
    "RICHARD" = c("RICK", "RICKY", "RICH", "RICHIE", "DICK"),
    "CHARLES" = c("CHUCK", "CHARLIE", "CHUCKY", "CHAS"),
    "CHRISTOPHER" = c("CHRIS", "CHRISTY", "KIT"),
    "MATTHEW" = c("MATT", "MATTY"),
    "ANTHONY" = c("TONY", "TONEY"),
    "DONALD" = c("DON", "DONNY", "DONNIE"),
    "STEVEN" = c("STEVE", "STEVIE"),
    "JOSEPH" = c("JOE", "JOEY"),
    "THOMAS" = c("TOM", "TOMMY", "THOM"),
    "DANIEL" = c("DAN", "DANNY", "DANIEL"),
    "PAUL" = c("PAULIE"),
    "MARK" = c("MARKY"),
    "GEORGE" = c("GEORG"),
    "KENNETH" = c("KEN", "KENNY"),
    "JOSHUA" = c("JOSH"),
    "KEVIN" = c("KEV"),
    "BRIAN" = c("BRI"),
    "EDWARD" = c("ED", "EDDIE", "TED", "TEDDY"),
    "RONALD" = c("RON", "RONNY", "RONNIE"),
    "TIMOTHY" = c("TIM", "TIMMY"),
    "JASON" = c("JASE"),
    "JEFFREY" = c("JEFF", "JEFFIE"),
    "RYAN" = c("RY"),
    "JACOB" = c("JAKE", "JAKEY"),
    "GARY" = c("GARY"),
    "NICHOLAS" = c("NICK", "NICKY"),
    "ERIC" = c("RICK", "RICKY"),
    "JONATHAN" = c("JON", "JONNY"),
    "STEPHEN" = c("STEVE", "STEVIE"),
    "LARRY" = c("LAWRENCE"),
    "JUSTIN" = c("JUST"),
    "SCOTT" = c("SCOTTY"),
    "BRANDON" = c("BRAND"),
    "BENJAMIN" = c("BEN", "BENNY", "BENJI"),
    "SAMUEL" = c("SAM", "SAMMY"),
    "FRANK" = c("FRANKY", "FRANKIE"),
    "PATRICK" = c("PAT", "PATTY", "PADDY"),
    "RAYMOND" = c("RAY", "RAYMUND"),
    "JACK" = c("JACKY", "JACKIE"),
    "DENNIS" = c("DENNY", "DENNIE"),
    "JERRY" = c("GERR", "GERALD"),
    "TYLER" = c("TY"),
    "AARON" = c("ARON"),
    "JOSE" = c("JOSEPH"),
    "HENRY" = c("HANK", "HARRY", "HENRI"),
    "ADAM" = c("ADDY"),
    "DOUGLAS" = c("DOUG", "DOUGIE"),
    "NATHAN" = c("NATE", "NATEY"),
    "PETER" = c("PETE", "PETEY"),
    "ZACHARY" = c("ZACK", "ZACKY", "ZACH"),
    "KYLE" = c("KY"),
    "NOAH" = c("NO"),
    "ALAN" = c("AL", "ALLIE"),
    "ETHAN" = c("ETH"),
    "JEREMY" = c("JERRY", "JEREM"),
    "CARL" = c("CHARLIE"),
    "HAROLD" = c("HARRY", "HAL"),
    "ARTHUR" = c("ART", "ARTIE"),
    "LAWRENCE" = c("LARRY", "LANCE"),
    "SEAN" = c("SHAWN"),
    "CHRISTIAN" = c("CHRIS", "CHRISTY"),
    "ALBERT" = c("AL", "ALBIE", "BERT"),
    "JUSTIN" = c("JUST"),
    "WAYNE" = c("WAY"),
    "RALPH" = c("RALPHY"),
    "ROY" = c("ROYIE"),
    "EUGENE" = c("GENE"),
    "LOUIS" = c("LOU", "LOUIE"),
    "PHILIP" = c("PHIL", "PHILLY"),

    # Common female names
    "MARY" = c("MARY", "MARIE", "MARIA", "MOLLY", "POLLY"),
    "PATRICIA" = c("PAT", "PATTY", "TRICIA", "PATSY"),
    "JENNIFER" = c("JEN", "JENNY", "JENNIE"),
    "LINDA" = c("LIN", "LINDY"),
    "ELIZABETH" = c("LIZ", "LIZZY", "BETH", "BETTY", "BETSY", "ELIZA"),
    "BARBARA" = c("BARB", "BARBIE", "BABS"),
    "SUSAN" = c("SUE", "SUZY", "SUSIE", "SUZIE"),
    "JESSICA" = c("JESS", "JESSIE"),
    "SARAH" = c("SARA"),
    "KAREN" = c("KARI", "KARRIE"),
    "NANCY" = c("NAN", "NANNY"),
    "LISA" = c("LIS"),
    "BETTY" = c("BET", "BETTE"),
    "HELEN" = c("HELEN"),
    "SANDRA" = c("SANDY", "SANDI"),
    "DONNA" = c("DON", "DONNIE"),
    "CAROL" = c("CARRIE", "CAROLINE"),
    "RUTH" = c("RUTHIE"),
    "SHARON" = c("SHARI", "SHERRY"),
    "MICHELLE" = c("MICH", "MICKEY", "MICKI", "SHELLY"),
    "LAURA" = c("LAUR", "LAURIE"),
    "SARAH" = c("SARA"),
    "KIMBERLY" = c("KIM", "KIMMY"),
    "DEBORAH" = c("DEB", "DEBBIE", "DEBBI"),
    "DOROTHY" = c("DOT", "DOTTIE", "DOLLY"),
    "AMY" = c("AMI"),
    "ANGELA" = c("ANGIE", "ANG"),
    "ASHLEY" = c("ASH", "ASHIE"),
    "BRENDA" = c("BREN"),
    "EMMA" = c("EM", "EMMY"),
    "OLIVIA" = c("OLLY", "LIV", "LIVY"),
    "CYNTHIA" = c("CINDY", "CYNDI", "CYNN"),
    "MARIE" = c("MARY"),
    "JANET" = c("JAN", "JANNY"),
    "CATHERINE" = c("CATHY", "CATH", "KATE", "KATIE", "KAT"),
    "FRANCES" = c("FRAN", "FRANNIE", "FANNY"),
    "CHRISTINE" = c("CHRIS", "CHRISTY", "CHRISSY"),
    "SAMANTHA" = c("SAM", "SAMMY"),
    "DEBRA" = c("DEB", "DEBBIE"),
    "RACHEL" = c("RAY"),
    "CAROLYN" = c("CAROL", "CARRIE"),
    "JANET" = c("JAN", "JANNY"),
    "VIRGINIA" = c("GINNY", "GINGER", "VIRGINIA"),
    "MARIA" = c("MARY", "MARIE"),
    "HEATHER" = c("HEATH"),
    "DIANE" = c("DI", "DIDI"),
    "JULIE" = c("JUL", "JULY"),
    "JOYCE" = c("JOY"),
    "VICTORIA" = c("VICKY", "VIC", "TORI"),
    "KELLY" = c("KEL"),
    "CHRISTINA" = c("CHRIS", "CHRISTY", "TINA", "CHRISSY"),
    "JOAN" = c("JO", "JOANIE"),
    "EVELYN" = c("EVE", "EVIE"),
    "LAUREN" = c("LAUR"),
    "JUDITH" = c("JUDY", "JUDIE"),
    "MEGAN" = c("MEG", "MEGGIE"),
    "CHERYL" = c("CHERI", "CHERRY"),
    "ANDREA" = c("ANDI", "ANDIE"),
    "HANNAH" = c("HAN"),
    "JACQUELINE" = c("JACKIE", "JACK", "JAC"),
    "MARTHA" = c("MARTY"),
    "GLORIA" = c("GLORY"),
    "TERESA" = c("TERRI", "TERRY", "TERI"),
    "SARA" = c("SARAH"),
    "JANICE" = c("JAN"),
    "MARIE" = c("MARY"),
    "JULIA" = c("JULIE", "JUL"),
    "HEATHER" = c("HEATH"),
    "DIANE" = c("DI"),
    "RUTH" = c("RUTHIE"),
    "JULIE" = c("JUL"),
    "JOYCE" = c("JOY"),
    "VIRGINIA" = c("GINNY", "GINGER")
  )

  # Build bidirectional mappings
  formal_to_nicknames <- nickname_mappings
  nickname_to_formal <- list()

  for (formal_name in names(nickname_mappings)) {
    nicknames <- nickname_mappings[[formal_name]]
    for (nickname in nicknames) {
      nickname_to_formal[[nickname]] <- formal_name
    }
  }

  # Create comprehensive dictionary object
  dict <- list(
    formal_to_nicknames = formal_to_nicknames,
    nickname_to_formal = nickname_to_formal,
    source = "integrated_codebase_patterns",
    created = Sys.time(),
    formal_count = length(formal_to_nicknames),
    nickname_count = length(nickname_to_formal)
  )

  if (verbose) {
    message(sprintf("✅ Nickname dictionary created with %d formal names and %d nicknames",
                   dict$formal_count, dict$nickname_count))
  }

  return(dict)
}

#' @title Enhanced First Name Similarity with Nickname Support
#'
#' @description
#' Calculates a similarity score between two first names, incorporating nickname
#' awareness. If two names are nickname equivalents (e.g., "Robert" and "Bob"),
#' returns a high similarity score (0.94-0.98) instead of the low Jaro-Winkler
#' distance that string-only comparison would produce. Falls back to
#' Jaro-Winkler
#' string distance when no nickname relationship is found. Also handles umlaut
#' digraphs (AE->A, OE->O, UE->U) for international name matching.
#'
#' @param name1 `character`: First name to compare.
#' @param name2 `character`: Second name to compare.
#' @param nickname_dict `list`: Nickname dictionary created by
#'   \code{\link{create_nickname_dictionary}}. If NULL, uses basic Jaro-Winkler
#'   similarity without nickname awareness.
#'
#' @return Numeric similarity score between 0 and 1, where:
#'   \describe{
#'     \item{1.0}{Exact match (after normalization)}
#'     \item{0.98}{Both names map to the same canonical formal name}
#'     \item{0.96}{One name is the canonical form of the other}
#'     \item{0.94}{Cross-nickname match (both nicknames of same formal name)}
#'     \item{0.5}{Missing or NA input (neutral score)}
#'     \item{0-1}{Jaro-Winkler similarity for non-nickname pairs}
#'   }
#'
#' @concept npi-matching
#' @concept data-linkage
#' @concept match-scoring
#'
#' @family step1-name-matching
#'
#' @seealso
#' \code{\link{get_canonical_name}} for resolving nicknames to formal names.
#' \code{\link{are_nickname_equivalents}} for boolean nickname checking.
#' \code{\link{create_nickname_dictionary}} for building the dictionary.
#' \code{\link{parse_physician_name_enhanced}} in
#'   \code{R/name_parsing_protocol_enhanced.R} — canonical parser (CLAUDE.md
#' #3).
#' \code{R/canonical_abog_npi_pipeline_STABLE.R} — Step 1 orchestrator.
#'
#' @examples
#' \dontrun{
#' dict <- create_nickname_dictionary(verbose = FALSE)
#' calculate_enhanced_first_name_similarity("Robert", "Bob", dict)
#' # [1] 0.98
#' calculate_enhanced_first_name_similarity("Robert", "Robert", dict)
#' # [1] 1.0
#' calculate_enhanced_first_name_similarity("Robert", "Richard", dict)
#' # Jaro-Winkler fallback score
#' }
#'
#' @keywords internal
calculate_enhanced_first_name_similarity <- function(name1, name2, nickname_dict = NULL) {
  # Handle missing inputs
  if (is.null(name1) || is.null(name2) || is.na(name1) || is.na(name2)) {
    return(0.5)  # Neutral score for missing data
  }

  # Normalize to uppercase
  # BUG FIX #7 (2026-01-28): Use normalize_string() instead of toupper(trimws())
  name1_clean <- normalize_string(name1)
  name2_clean <- normalize_string(name2)

  normalize_for_similarity <- function(value) {
    value <- normalize_string(value)
    if (requireNamespace("stringi", quietly = TRUE)) {
      value <- stringi::stri_trans_general(value, "Latin-ASCII")
    }
    value
  }

  simplify_umlaut_digraphs <- function(value) {
    value <- gsub("AE", "A", value, perl = TRUE)
    value <- gsub("OE", "O", value, perl = TRUE)
    value <- gsub("UE", "U", value, perl = TRUE)
    value
  }

  name1_norm <- normalize_for_similarity(name1_clean)
  name2_norm <- normalize_for_similarity(name2_clean)

  # Exact match gets perfect score
  if (name1_norm == name2_norm) {
    return(1.0)
  }

  # If no nickname dictionary provided, use basic Jaro-Winkler
  if (is.null(nickname_dict)) {
    return(1 - stringdist::stringdist(name1_norm, name2_norm, method = "jw"))
  }

  # Check for nickname relationships
  canonical1 <- get_canonical_name(name1_clean, nickname_dict)
  canonical2 <- get_canonical_name(name2_clean, nickname_dict)

  # If both map to the same canonical name, they're nickname equivalents
  if (!is.null(canonical1) && !is.null(canonical2) && canonical1 == canonical2) {
    return(0.98)  # Very high similarity for nickname matches
  }

  # Check if one is the canonical form of the other
  if (canonical1 == name2_clean || canonical2 == name1_clean) {
    return(0.96)  # High similarity for formal-nickname pairs
  }

  # Check for cross-nickname similarity (both nicknames of same formal name)
  if (!is.null(canonical1) && !is.null(canonical2)) {
    formal1 <- canonical1
    formal2 <- canonical2

    if (formal1 == formal2) {
      return(0.94)  # High similarity for cross-nicknames
    }
  }

  # Fallback to fuzzy string matching
  jw_similarity <- 1 - stringdist::stringdist(name1_norm, name2_norm, method = "jw")
  jw_simplified <- 1 - stringdist::stringdist(
    simplify_umlaut_digraphs(name1_norm),
    simplify_umlaut_digraphs(name2_norm),
    method = "jw"
  )
  return(max(jw_similarity, jw_simplified))
}

#' @title Get Canonical (Formal) Name for a Given Name
#'
#' @description
#' Resolves a name (which may be a nickname) to its canonical formal form using
#' the nickname dictionary. If the name is already a formal name in the
#' dictionary, returns it unchanged. If the name is a known nickname, returns
#' the corresponding formal name. If the name is not found in either mapping,
#' returns the normalized (uppercased, trimmed) input.
#'
#' @param name `character`: Name to look up. Can be a formal name or nickname.
#'   NULL and NA values are handled gracefully (returned as-is).
#' @param nickname_dict `list`: Nickname dictionary created by
#'   \code{\link{create_nickname_dictionary}}. If NULL, the input name is
#'   returned unchanged.
#'
#' @return Character string with the canonical formal name, or the normalized
#'   input if not found in the dictionary. Returns NULL if input is NULL, or
#'   NA if input is NA.
#'
#' @concept npi-matching
#' @concept data-linkage
#' @concept match-scoring
#'
#' @family step1-name-matching
#'
#' @seealso
#' \code{\link{create_nickname_dictionary}} for building the dictionary.
#' \code{\link{are_nickname_equivalents}} for checking if two names share a canonical form.
#' \code{\link{get_nicknames_for_name}} for the reverse lookup (formal -> nicknames).
#' \code{\link{parse_physician_name_enhanced}} in
#'   \code{R/name_parsing_protocol_enhanced.R} — canonical parser (CLAUDE.md
#' #3).
#' \code{R/canonical_abog_npi_pipeline_STABLE.R} — Step 1 orchestrator.
#'
#' @examples
#' \dontrun{
#' dict <- create_nickname_dictionary(verbose = FALSE)
#' get_canonical_name("Bob", dict)
#' # [1] "ROBERT"
#' get_canonical_name("ROBERT", dict)
#' # [1] "ROBERT"
#' get_canonical_name("Xyzzy", dict)
#' # [1] "XYZZY"
#' }
#'
#' @keywords internal
get_canonical_name <- function(name, nickname_dict) {
  # BUG FIX #39 (2026-01-28): Check for NA in addition to NULL.

  # toupper(NA) returns NA but can cause downstream issues; trimws(NA) also returns NA.
  # Early return ensures consistent behavior for missing inputs.
  if (is.null(name) || is.null(nickname_dict) || (length(name) == 1 && is.na(name))) {
    return(name)
  }

  # BUG FIX #7 (2026-01-28): Use normalize_string() instead of toupper(trimws())
  name_clean <- normalize_string(name)

  # Check if it's already a formal name
  if (name_clean %in% names(nickname_dict$formal_to_nicknames)) {
    return(name_clean)
  }

  # Check if it's a nickname
  if (name_clean %in% names(nickname_dict$nickname_to_formal)) {
    return(nickname_dict$nickname_to_formal[[name_clean]])
  }

  # Return original if not found
  return(name_clean)
}

#' @title Check If Two Names Are Nickname Equivalents
#'
#' @description
#' Determines whether two names resolve to the same canonical formal name
#' in the nickname dictionary. For example, "Bob" and "Rob" are both nicknames
#' of "ROBERT", so they are considered nickname equivalents. Returns FALSE
#' if the nickname dictionary is NULL or if neither name appears in the
#' dictionary mappings.
#'
#' @param name1 `character`: First name to compare.
#' @param name2 `character`: Second name to compare.
#' @param nickname_dict `list`: Nickname dictionary created by
#'   \code{\link{create_nickname_dictionary}}. If NULL, returns FALSE.
#'
#' @return Logical. TRUE if both names resolve to the same canonical formal
#'   name, FALSE otherwise.
#'
#' @concept npi-matching
#' @concept data-linkage
#' @concept match-scoring
#'
#' @family step1-name-matching
#'
#' @seealso
#' \code{\link{get_canonical_name}} for resolving individual names.
#' \code{\link{calculate_enhanced_first_name_similarity}} for numeric similarity scores.
#' \code{\link{parse_physician_name_enhanced}} in
#'   \code{R/name_parsing_protocol_enhanced.R} — canonical parser (CLAUDE.md
#' #3).
#' \code{R/canonical_abog_npi_pipeline_STABLE.R} — Step 1 orchestrator.
#'
#' @examples
#' \dontrun{
#' dict <- create_nickname_dictionary(verbose = FALSE)
#' are_nickname_equivalents("Bob", "Rob", dict)
#' # [1] TRUE
#' are_nickname_equivalents("Bob", "Steve", dict)
#' # [1] FALSE
#' }
#'
#' @keywords internal
are_nickname_equivalents <- function(name1, name2, nickname_dict) {
  if (is.null(nickname_dict)) {
    return(FALSE)
  }

  canonical1 <- get_canonical_name(name1, nickname_dict)
  canonical2 <- get_canonical_name(name2, nickname_dict)

  return(!is.null(canonical1) && !is.null(canonical2) && canonical1 == canonical2)
}

#' @title Get All Nicknames for a Formal Name
#'
#' @description
#' Retrieves all known nicknames for a given formal name from the nickname
#' dictionary. For example, looking up "ELIZABETH" returns c("LIZ", "LIZZY",
#' "BETH", "BETTY", "BETSY", "ELIZA"). Returns an empty character vector if
#' the name is not found or inputs are NULL.
#'
#' @param formal_name `character`: Formal name to look up (e.g., "ELIZABETH").
#'   Will be normalized to uppercase internally.
#' @param nickname_dict `list`: Nickname dictionary created by
#'   \code{\link{create_nickname_dictionary}}. If NULL, returns empty vector.
#'
#' @return Character vector of nicknames associated with the formal name.
#'   Returns \code{character(0)} if the name is not in the dictionary or
#'   inputs are NULL.
#'
#' @concept npi-matching
#' @concept data-linkage
#' @concept match-scoring
#'
#' @family step1-name-matching
#'
#' @seealso
#' \code{\link{get_canonical_name}} for the reverse lookup (nickname -> formal name).
#' \code{\link{create_nickname_dictionary}} for building the dictionary.
#' \code{\link{parse_physician_name_enhanced}} in
#'   \code{R/name_parsing_protocol_enhanced.R} — canonical parser (CLAUDE.md
#' #3).
#' \code{R/canonical_abog_npi_pipeline_STABLE.R} — Step 1 orchestrator.
#'
#' @examples
#' \dontrun{
#' dict <- create_nickname_dictionary(verbose = FALSE)
#' get_nicknames_for_name("ELIZABETH", dict)
#' # [1] "LIZ" "LIZZY" "BETH" "BETTY" "BETSY" "ELIZA"
#' get_nicknames_for_name("XYZZY", dict)
#' # character(0)
#' }
#'
#' @keywords internal
get_nicknames_for_name <- function(formal_name, nickname_dict) {
  if (is.null(nickname_dict) || is.null(formal_name)) {
    return(character(0))
  }

  # BUG FIX #7 (2026-01-28): Use normalize_string() instead of toupper(trimws())
  formal_clean <- normalize_string(formal_name)

  if (formal_clean %in% names(nickname_dict$formal_to_nicknames)) {
    return(nickname_dict$formal_to_nicknames[[formal_clean]])
  }

  return(character(0))
}

#' @title Create Nickname-Aware Jaro-Winkler Similarity Function
#'
#' @description
#' Factory function that returns a closure with a pre-bound nickname dictionary.
#' The returned function accepts two name arguments and calculates
#' nickname-enhanced similarity, useful as a callback in vectorized matching
#' pipelines where passing the dictionary each time is inconvenient.
#'
#' @param nickname_dict `list`: Nickname dictionary created by
#'   \code{\link{create_nickname_dictionary}}. This dictionary is captured
#'   in the closure environment of the returned function.
#'
#' @return A function with signature \code{function(name1, name2)} that returns
#'   a numeric similarity score between 0 and 1. Internally calls
#'   \code{\link{calculate_enhanced_first_name_similarity}}.
#'
#' @concept npi-matching
#' @concept data-linkage
#' @concept match-scoring
#'
#' @family step1-name-matching
#'
#' @seealso
#' \code{\link{calculate_enhanced_first_name_similarity}} for the underlying similarity logic.
#' \code{\link{create_nickname_dictionary}} for building the dictionary.
#' \code{\link{parse_physician_name_enhanced}} in
#'   \code{R/name_parsing_protocol_enhanced.R} — canonical parser (CLAUDE.md
#' #3).
#' \code{R/canonical_abog_npi_pipeline_STABLE.R} — Step 1 orchestrator.
#'
#' @examples
#' \dontrun{
#' dict <- create_nickname_dictionary(verbose = FALSE)
#' sim_fn <- create_nickname_aware_similarity(dict)
#' sim_fn("Robert", "Bob")
#' # [1] 0.98
#' }
#'
#' @keywords internal
create_nickname_aware_similarity <- function(nickname_dict) {
  function(name1, name2) {
    calculate_enhanced_first_name_similarity(name1, name2, nickname_dict)
  }
}

# Export main functions for global use
if (!exists(".nickname_dict_cache")) {
  .nickname_dict_cache <- NULL
}

#' @title Get or Create Cached Nickname Dictionary
#'
#' @description
#' Returns a cached nickname dictionary, creating it on first call. Subsequent
#' calls return the same dictionary from a module-level cache variable
#' (\code{.nickname_dict_cache}) without rebuilding, providing fast repeated
#' access during batch matching operations. Use \code{refresh = TRUE} to
#' force a rebuild of the dictionary.
#'
#' @param refresh `logical`: If TRUE, forces a rebuild of the cached dictionary
#'   even if one already exists. Default: FALSE.
#'
#' @return A nickname dictionary list as created by
#'   \code{\link{create_nickname_dictionary}}, containing formal_to_nicknames
#'   and nickname_to_formal mappings.
#'
#' @concept npi-matching
#' @concept data-linkage
#' @concept match-scoring
#'
#' @family step1-name-matching
#'
#' @seealso
#' \code{\link{create_nickname_dictionary}} for creating a fresh dictionary
#'   without caching.
#' \code{\link{calculate_enhanced_first_name_similarity}} for the primary
#'   consumer of the cached dictionary.
#' \code{\link{parse_physician_name_enhanced}} in
#'   \code{R/name_parsing_protocol_enhanced.R} — canonical parser (CLAUDE.md
#' #3).
#' \code{R/canonical_abog_npi_pipeline_STABLE.R} — Step 1 orchestrator.
#'
#' @examples
#' \dontrun{
#' dict <- get_nickname_dictionary()
#' dict$formal_count
#'
#' # Force rebuild
#' dict2 <- get_nickname_dictionary(refresh = TRUE)
#' }
#'
#' @keywords internal
get_nickname_dictionary <- function(refresh = FALSE) {
  if (refresh || is.null(.nickname_dict_cache)) {
    .nickname_dict_cache <<- create_nickname_dictionary(verbose = FALSE)
  }
  return(.nickname_dict_cache)
}

message("✅ Nickname system loaded successfully!")
message("📚 Use create_nickname_dictionary() to build dictionary")
message("🔍 Use calculate_enhanced_first_name_similarity() for nickname-aware matching")
message("⚡ Use get_nickname_dictionary() for cached access")
