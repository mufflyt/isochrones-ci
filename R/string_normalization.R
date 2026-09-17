#!/usr/bin/env Rscript
# =============================================================================
# String Normalization Functions - Bug #7 Fix
# =============================================================================
# Purpose: Provide pre-normalization functions to eliminate repeated
#          toupper(trimws()) calls that create temp strings in hot paths
# Author: Tyler Muffly & Claude Code
# Date: 2026-01-28
# =============================================================================

#' @title Normalize a String for Name Matching
#'
#' @description
#' Applies uppercase conversion and leading/trailing whitespace removal in a
#' single pass, with optional advanced normalization (apostrophe removal,
#' internal space removal) for compound-name variants.
#'
#' @inheritParams shared_params_data
#' @inheritParams shared_params_col_names
#' @param x `character vector`: Strings to normalize.
#' @param remove_apostrophes `logical`: Remove apostrophes before matching
#'   (e.g., O'Brien -> OBRIEN). Default FALSE.
#' @param remove_internal_spaces `logical`: Remove internal whitespace runs
#'   (e.g., Van Der -> VANDER). Default FALSE.
#'
#' @return Character vector of normalized strings, same length as \code{x}.
#'   NULL input returns \code{character(0)}.
#'
#' @details
#' Normalization steps applied in order:
#' \preformatted{
#'  Step | Operation               | Always? | Example
#'  -----|-------------------------|---------|-------------------------
#'  1    | NUL byte strip          | YES     | "JOH\\x00N" -> "JOH N"
#'  2    | stri_trans_nfc()        | YES     | "Garc\u00EDa" (NFD) ->
#' "Garc\u00EDa" (NFC)
#'  3    | German romanization     | YES     | "M\u00FCller" -> "MUELLER"
#'  4    | stri_trans_general()    | YES     | "Garc\u00EDa" -> "Garcia"
#' (Latin-ASCII)
#'  5    | toupper() + trimws()    | YES     | "  John  " -> "JOHN"
#'  6    | gsub("'", "")           | optional| "O'BRIEN" -> "OBRIEN"
#'  7    | gsub("\\s+", "")        | optional| "VAN DER" -> "VANDER"
#' }
#'
#' SCIENTIFIC INTEGRITY FIX (2026-05-21): Mandatory ASCII normalization
#' prevents silent join failures for names containing Unicode non-breaking
#' spaces (U+00A0), en-dashes (U+2013), or accented characters.
#'
#' GERMAN ROMANIZATION FIX (2026-06-15): German umlauts and eszett are
#' romanized to digraphs (\u00FC -> UE, \u00F6 -> OE, \u00E4 -> AE, \u00DF -> SS)
#' BEFORE the Latin-ASCII strip. Without this step the strip degrades them
#' to single letters (\u00FC -> u, \u00DF -> s), which silently breaks
#' German-surname joins against any registry that stores the romanized
#' form (NPPES often does, ABMS does via canonical_abms_npi_matching's
#' normalize_text). This brings normalize_string() into parity with that
#' canonical normalizer; see Hall of Shame: "M\u00FCller" / "MUELLER".
#'
#' This function is the foundation for pre-processing before
#' \code{parse_physician_name_enhanced()} from
#' \code{R/name_parsing_protocol_enhanced.R} (96.7% accuracy, CLAUDE.md #3).
#' Normalize once at the start of a matching function; do not repeat
#' \code{toupper(trimws())} in inner loops.
#'
#' @section Pipeline Position:
#' \preformatted{
#'  Called at:  Step 1 ABOG-NPI matching (strategy pre-processing)
#'  Called by:  normalize_name_columns(), normalize_physician_names()
#'  Before:     any string comparison or join on name fields
#'  Not called: in Step 2+ (geocoding, census, accessibility steps)
#' }
#'
#' @section Function Family:
#' \preformatted{
#'  Function                       | Role
#'  -------------------------------|--------------------------------------------
#'  normalize_string()             | Core single-string normalizer (this fn)
#'  normalize_name_columns()       | Apply to named df columns, add _norm suffix
#'  extract_first_initial()        | Safe first-character extraction
#'  normalize_physician_names()    | Full pipeline: first/last/middle + initials
#'  needs_normalization()          | Detect if a column has
#' mixed-case/whitespace
#' }
#'
#' @examples
#' \dontrun{
#' # Basic normalization
#' normalize_string("  John Smith  ")
#' # [1] "JOHN SMITH"
#'
#' # Apostrophe removal for matching
#' normalize_string("O'Brien", remove_apostrophes = TRUE)
#' # [1] "OBRIEN"
#'
#' # Compound name collapse
#' normalize_string("Van Der Berg", remove_internal_spaces = TRUE)
#' # [1] "VANDERBERG"
#' }
#'
#' @seealso
#' \code{\link{normalize_physician_names}} for the complete physician-name
#' workflow used in the ABOG-NPI cascade,
#' \code{R/name_parsing_protocol_enhanced.R} for the downstream
#' \code{parse_physician_name_enhanced()} function (CLAUDE.md #3),
#' \code{R/abog_name_parser_fast.R} for ABOG-specific fast parsing.
#'
#' @family step1-name-matching
#' @concept match-scoring
#' @concept npi-matching
#' @export
normalize_string <- function(x,
                             remove_apostrophes = FALSE,
                             remove_internal_spaces = FALSE) {
  # Handle NULL/NA safely
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }

  # Strip embedded NUL bytes (U+0000) first. PCRE treats \0 as a string
  # terminator, so a NUL byte in the input silently truncates downstream
  # regex passes ("Smith\0Jones" -> "SMITHJONES"). Replace with a space
  # so the surrounding name fragments stay joinable as separate words.
  # Mirrors normalize_text() in canonical_abms_npi_matching for parity.
  x <- gsub("\\x00", " ", x, perl = TRUE)

  # SCIENTIFIC INTEGRITY FIX (2026-05-21):
  # Use stringi for robust Unicode handling and ASCII transliteration.
  # This prevents matches failing due to NFC/NFD drift or accented chars.
  if (requireNamespace("stringi", quietly = TRUE)) {
    # 1. NFC normalization (canonical decomposition/recomposition)
    x <- stringi::stri_trans_nfc(x)
    # 2. German romanization BEFORE Latin-ASCII strip.
    #    Latin-ASCII degrades ü/ö/ä/ß to u/o/a/s, losing parity with
    #    canonical_abms_npi_matching::normalize_text() which produces
    #    UE/OE/AE/SS. Romanize first; the strip then sees ASCII already.
    x <- gsub("\u00FC", "UE", x, fixed = TRUE)  # u-umlaut
    x <- gsub("\u00DC", "UE", x, fixed = TRUE)  # U-umlaut
    x <- gsub("\u00F6", "OE", x, fixed = TRUE)  # o-umlaut
    x <- gsub("\u00D6", "OE", x, fixed = TRUE)  # O-umlaut
    x <- gsub("\u00E4", "AE", x, fixed = TRUE)  # a-umlaut
    x <- gsub("\u00C4", "AE", x, fixed = TRUE)  # A-umlaut
    x <- gsub("\u00DF", "SS", x, fixed = TRUE)  # eszett
    # 3. Latin-to-ASCII transliteration (García -> Garcia)
    #    Also handles en-dashes, non-breaking spaces, etc.
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  } else {
    # Fallback to base: handles basic ASCII trim/upper only.
    # High-precision matching requires stringi.
    warning("[string_normalization] 'stringi' not found; Unicode names may fail to match.")
  }

  # Standard cleanup
  result <- toupper(trimws(x))

  # Optional advanced normalization
  if (remove_apostrophes) {
    result <- gsub("'", "", result, fixed = TRUE)
  }

  if (remove_internal_spaces) {
    result <- gsub("\\s+", "", result)
  }

  result
}

#' Canonical Name Join Key
#'
#' \code{normalize_string()} trims the ends but deliberately leaves internal
#' whitespace alone, because some callers need "VAN  DER BERG" preserved. Name
#' JOIN KEYS need the opposite: "John  Smith" and "John Smith" are one person
#' and must produce one key.
#'
#' Every hand-rolled name normaliser in this repo has been
#' \code{toupper(trimws(gsub("\\s+", " ", x)))} -- correct about whitespace,
#' silently wrong about Unicode. This is that function with the transliteration
#' restored, and it exists so those call sites have something to migrate TO
#' rather than each growing its own partial fix.
#'
#' Differs from the toupper-only version ONLY on non-ASCII input; identical on
#' every ASCII string. See tests/testthat/test-normalize-name-key.R.
#'
#' @param x `character`: Names to normalize.
#' @return `character`: Upper-cased, transliterated, whitespace-collapsed. NA in,
#'   NA out; zero-length in, zero-length out.
#'
#' @family step1-name-matching
#' @concept npi-matching
#' @export
normalize_name_key <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  gsub("\\s+", " ", normalize_string(x))
}

#' Normalize Name Columns in a Data Frame
#'
#' Applies \code{normalize_string()} to one or more named columns in a data
#' frame, creating new columns with a \code{"_norm"} suffix (or custom suffix).
#' Original columns are preserved.
#'
#' @param remove_apostrophes `logical`: Remove apostrophes. Default FALSE.
#' @param remove_internal_spaces `logical`: Remove internal spaces. Default
#'   FALSE.
#' @param suffix `character`: Suffix appended to each column name to form the
#'   normalized column name. Default \code{"_norm"}.
#'
#' @return Data frame with all original columns plus new normalized columns.
#'
#' @details
#' Column naming:
#' \preformatted{
#'  Original column | Normalized column (default suffix)
#'  ----------------|------------------------------------
#'  first           | first_norm
#'  last            | last_norm
#'  middle          | middle_norm
#' }
#'
#' Best practice: call this ONCE at the top of a matching function, then
#' use the \code{_norm} columns in all comparisons:
#' \preformatted{
#'  GOOD: df$last_norm == other$last_norm
#'  BAD:  toupper(trimws(df$last)) == toupper(trimws(other$last))
#' }
#'
#' Stops with an informative error if any requested column is absent from
#' \code{df}.
#'
#' @examples
#' \dontrun{
#' physicians <- data.frame(
#'   first = c("  John ", "Mary", "O'Brien"),
#'   last  = c("Smith  ", "Johnson", "Van Der Berg")
#' )
#'
#' # Basic normalization
#' physicians_norm <- normalize_name_columns(physicians, c("first", "last"))
#' # Adds: first_norm, last_norm
#'
#' # Advanced normalization for matching across compound names
#' physicians_norm <- normalize_name_columns(
#'   physicians, c("first", "last"),
#'   remove_apostrophes = TRUE,
#'   remove_internal_spaces = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link{normalize_string}} for the per-element normalizer,
#' \code{\link{normalize_physician_names}} for the full physician workflow.
#'
#' @family step1-name-matching
#' @concept match-scoring
#' @concept npi-matching
#' @export
normalize_name_columns <- function(df,
                                   cols,
                                   remove_apostrophes = FALSE,
                                   remove_internal_spaces = FALSE,
                                   suffix = "_norm") {
  # Validate input
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Create normalized columns
  for (col in cols) {
    norm_col <- paste0(col, suffix)
    df[[norm_col]] <- normalize_string(
      df[[col]],
      remove_apostrophes = remove_apostrophes,
      remove_internal_spaces = remove_internal_spaces
    )
  }

  df
}

#' Extract First Initial from a Normalized String
#'
#' Safely extracts the first character (initial) from a string, handling
#' NA, empty strings, and whitespace-only strings gracefully. Returns
#' NA for degenerate inputs rather than an empty string.
#'
#' @param x `character vector`: Strings from which to extract first initials.
#'
#' @return Character vector of first initials (uppercase, 1 character each),
#'   or NA_character_ for NA/empty inputs. NULL input returns
#'   \code{character(0)}.
#'
#' @details
#' Comparison of input handling:
#' \preformatted{
#'  Input          | Output  | Reason
#'  ---------------|---------|-------------------------------------
#'  "Robert"       | "R"     | Normal case
#'  "  mary  "     | "M"     | trimws + toupper applied first
#'  NA             | NA      | Propagate missing value
#'  ""             | NA      | Empty string yields no initial
#'  "  "           | NA      | Whitespace-only yields no initial
#' }
#'
#' Typical use: middle-name initial comparison, where only the first
#' letter matters for matching (e.g., ABOG "J" vs NPPES "John").
#'
#' @examples
#' \dontrun{
#' extract_first_initial("Robert")    # "R"
#' extract_first_initial("  mary  ")  # "M"
#' extract_first_initial(NA)          # NA
#' extract_first_initial("")          # NA
#' }
#'
#' @seealso
#' \code{\link{normalize_physician_names}} which calls this for
#' \code{first_initial} and \code{middle_initial} columns.
#'
#' @family step1-name-matching
#' @concept match-scoring
#' @export
extract_first_initial <- function(x) {
  # Handle NULL/NA
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }

  # SCIENTIFIC INTEGRITY FIX (2026-08-10, ruling D9).
  #
  # This used toupper(trimws(x)) and then gsub("[^A-Z]", "") -- which DELETES
  # an accented first letter instead of folding it. "Angela" spelled with an
  # acute A became "NGELA", so the initial was N. First-initial blocking is a
  # LINKAGE KEY, so candidate pairs were never generated for those people and
  # the failure fell entirely on non-Anglo names.
  #
  # The file directly below documents Latin-ASCII parity as critical for
  # matching, and normalize_string() has folded accents since 2026-05-21 --
  # but this function, the blocking key, never got the same treatment. The two
  # sat one screen apart and disagreed.
  #
  # Delegating to normalize_string() makes parity structural rather than
  # coincidental: NFC, German romanization (ue/oe/ae/ss) and Latin-ASCII all
  # apply once, in one place, so the initial cannot drift from the name it is
  # taken from. Verified against the SQL side, which uses strip_accents().
  normalized <- normalize_string(x)

  # Strip non-letters AFTER transliteration so values like "(B)", ".A" or "-K"
  # return "B" / "A" / "K" rather than "(" / "." / "-".
  letters_only <- gsub("[^A-Z]", "", normalized)

  # Extract first character, return NA for empty / letter-free strings
  result <- ifelse(
    is.na(letters_only) | letters_only == "",
    NA_character_,
    substr(letters_only, 1, 1)
  )

  result
}

#' SQL Expression for NPPES Name Normalization (Latin-ASCII Parity)
#'
#' @description
#' Returns a SQL expression that normalizes an NPPES name column to match
#' R-side \code{normalize_string()} output. Critical for ABOG-NPI matching:
#' ABOG names pass through \code{normalize_string()} which calls
#' \code{stringi::stri_trans_general(., "Latin-ASCII")}, folding "García"
#' to "GARCIA". If the NPPES side uses only \code{UPPER(TRIM(x))}, accented
#' names produce silent join failures (R yields "GARCIA", SQL yields
#' "GARCÍA").
#'
#' The fix uses DuckDB's built-in \code{strip_accents()} function
#' (available since DuckDB v0.9). This is the SQL-side equivalent of the
#' R \code{Latin-ASCII} transliteration: it removes combining marks via
#' Unicode NFD decomposition.
#'
#' @param col `character(1)`: SQL column expression (qualified or bare)
#'   to normalize. Examples: \code{"n.last_name"}, \code{"last_name"},
#'   \code{"COALESCE(p.last_name, '')"}.
#'
#' @return Character. A SQL expression string. Typical usage in a sprintf
#'   template:
#' \preformatted{
#'   sprintf("WHERE %s = a.last_clean", sql_npi_name("n.last_name"))
#' }
#'
#' @section R/SQL Parity:
#' For a name string \code{x}, the following are equivalent:
#' \preformatted{
#'   R   : normalize_string(x)                       -> "GARCIA"
#'   SQL : strip_accents(UPPER(TRIM(x)))             -> "GARCIA"
#' }
#' Both fold Latin accents (à á â ã ä å, è é ê ë, ì í î ï, ò ó ô õ ö ø,
#' ù ú û ü, ñ, ç, ý ÿ and uppercase) to their ASCII base letter.
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#'   query <- sprintf(
#'     "SELECT %s AS clean FROM (SELECT 'García' AS x)",
#'     sql_npi_name("x")
#'   )
#'   DBI::dbGetQuery(con, query)
#'   #   clean
#'   # 1 GARCIA
#' }
#'
#' @family step1-name-matching
#' @concept sql-helpers
#' @concept npi-matching
#' @export
sql_npi_name <- function(col) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("sql_npi_name() requires a non-empty single-string column expression",
         call. = FALSE)
  }
  sprintf("strip_accents(UPPER(TRIM(%s)))", col)
}

#' Bulk Normalize ABOG/NPPES Physician Name Data for Matching
#'
#' The primary normalization function for the ABOG-NPI matching pipeline.
#' Normalizes first, last, and optional middle name columns into canonical
#' clean forms and extracts first initials — all in a single call.
#'
#' @param first_col `character`: Name of the first-name column. Default
#'   \code{"first_name"}.
#' @param last_col `character`: Name of the last-name column. Default
#'   \code{"last_name"}.
#' @param middle_col `character`: Name of the middle-name column. Default
#'   \code{"middle_name"}. If absent from \code{df}, \code{middle_clean}
#'   and \code{middle_initial} are filled with \code{NA_character_}.
#' @param advanced_norm `logical`: Apply apostrophe and internal-space removal
#'   in addition to basic uppercase + trim. Use \code{TRUE} for matching
#'   against NPPES data which may encode compound names differently. Default
#'   FALSE.
#'
#' @return Data frame with all original columns plus:
#' \preformatted{
#'  New column     | Content                              | Type
#'  ---------------|--------------------------------------|----------
#'  first_clean    | Normalized first name                | character
#'  last_clean     | Normalized last name                 | character
#'  middle_clean   | Normalized middle name (or NA)       | character
#'  first_initial  | First character of first_clean       | character
#'  middle_initial | First character of middle_clean (NA) | character
#' }
#'
#' @details
#' This function uses the standardized column names (\code{first_clean},
#' \code{last_clean}, \code{middle_clean}) throughout the codebase.
#' Matching strategies in \code{canonical_abog_npi_pipeline_STABLE.R}
#' expect these column names.
#'
#' Relationship to name parser standard (CLAUDE.md #3):
#' \preformatted{
#'  normalize_physician_names()         | parse_physician_name_enhanced()
#'  ------------------------------------|-----------------------------------
#'  Pre-processing: uppercase + trim    | Full parse: splits compound names,
#'  Fast: vectorized, no dict lookup    |   handles suffixes (Jr., III, MD)
#'  Used in matching comparisons        | Used for NPI registry lookups
#'  Call FIRST in matching workflow     | Call on raw ABOG input data
#' }
#'
#' @examples
#' \dontrun{
#' # Standard pipeline usage
#' abog_norm <- normalize_physician_names(abog_raw)
#'
#' # Then use normalized columns in all comparisons:
#' # GOOD:  abog_norm$first_clean == nppes_norm$first_clean
#' # BAD:   toupper(trimws(abog_raw$first_name)) == toupper(trimws(...))
#'
#' # Advanced normalization for compound-name matching
#' abog_norm_adv <- normalize_physician_names(abog_raw, advanced_norm = TRUE)
#' # O'Brien -> OBRIEN, Van Der Zee -> VANDERZEE
#' }
#'
#' @seealso
#' \code{\link{normalize_string}} for the per-element primitive,
#' \code{\link{extract_first_initial}} for the initial extraction,
#' \code{\link{needs_normalization}} to pre-check if a column is already
#' normalized,
#' \code{R/name_parsing_protocol_enhanced.R} for
#' \code{parse_physician_name_enhanced()} (CLAUDE.md #3, 96.7% accuracy),
#' \code{R/abog_name_parser_fast.R} for ABOG-specific fast parsing.
#'
#' @family step1-name-matching
#' @concept match-scoring
#' @concept npi-matching
#' @export
normalize_physician_names <- function(df,
                                      first_col = "first_name",
                                      last_col = "last_name",
                                      middle_col = "middle_name",
                                      advanced_norm = FALSE) {
  # Validate columns exist
  required <- c(first_col, last_col)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }

  # Normalize first and last names
  df$first_clean <- normalize_string(
    df[[first_col]],
    remove_apostrophes = advanced_norm,
    remove_internal_spaces = advanced_norm
  )

  df$last_clean <- normalize_string(
    df[[last_col]],
    remove_apostrophes = advanced_norm,
    remove_internal_spaces = advanced_norm
  )

  # Normalize middle name if present
  if (middle_col %in% names(df)) {
    df$middle_clean <- normalize_string(
      df[[middle_col]],
      remove_apostrophes = advanced_norm,
      remove_internal_spaces = advanced_norm
    )
  } else {
    df$middle_clean <- NA_character_
  }

  # Extract initials
  df$first_initial <- extract_first_initial(df$first_clean)
  df$middle_initial <- extract_first_initial(df$middle_clean)

  df
}

# =============================================================================
# Helper function: Check if normalization is needed
# =============================================================================

#' Check If a Column Needs Normalization
#'
#' Detects whether a character vector contains mixed-case text, leading or
#' trailing whitespace, or other indicators that \code{normalize_string()}
#' has not yet been applied.
#'
#' @param x `character vector`: Column to inspect.
#'
#' @return Logical scalar. TRUE if normalization is needed; FALSE if the
#'   vector appears to already be normalized (all-uppercase, no surrounding
#'   whitespace) or if \code{x} is NULL, empty, or all-NA.
#'
#' @details
#' Detection criteria:
#' \preformatted{
#'  Check               | Triggers TRUE
#'  --------------------|---------------------------------------------
#'  Mixed case          | any(x != toupper(x)) across non-NA values
#'  Surrounding spaces  | any(x != trimws(x)) across non-NA values
#' }
#'
#' Typical use: assertion/guard at the start of a function that expects
#' normalized input:
#' \preformatted{
#'  if (needs_normalization(df$last_name)) {
#'    stop("last_name column is not normalized — call
#' normalize_physician_names() first")
#'  }
#' }
#'
#' @examples
#' \dontrun{
#' needs_normalization(c("John", "MARY"))        # TRUE  — mixed case
#' needs_normalization(c("  JOHN  ", "MARY"))    # TRUE  — leading/trailing
#' spaces
#' needs_normalization(c("JOHN", "MARY"))        # FALSE — already normalized
#' needs_normalization(c(NA, NA))                # FALSE — all-NA, no check
#' needed
#' }
#'
#' @seealso
#' \code{\link{normalize_string}} to fix the detected issue,
#' \code{\link{normalize_physician_names}} for the full workflow.
#'
#' @family step1-name-matching
#' @concept match-scoring
#' @export
needs_normalization <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(FALSE)
  }

  # Remove NAs for checking
  x_valid <- x[!is.na(x)]

  # Check for mixed case
  has_mixed_case <- any(x_valid != toupper(x_valid))

  # Check for whitespace
  has_whitespace <- any(x_valid != trimws(x_valid))

  has_mixed_case || has_whitespace
}
