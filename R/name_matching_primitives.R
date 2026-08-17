#' @title Canonical person-name matching primitives
#'
#' @description
#' Shared mechanics for deciding whether two person-name records describe the
#' same human. These exist so that every caller -- the Healthgrades verifier,
#' ABOG/NPPES matching, and downstream projects such as the AMCB midwifery
#' linkage -- compares names one way instead of each carrying its own.
#'
#' @section Two modes, chosen by the caller:
#' Given-name comparison is NOT one rule. It depends on whether the source
#' preserves name ORDER:
#'
#' \preformatted{
#'   structured <-> structured   (registry fields <-> registry fields)
#'       -> mode = "positional_ie"
#'
#'   unreliable free text <-> structured   (repository author string <-> record)
#'       -> mode = "any_token", with position retained as evidence
#' }
#'
#' **Mode is a property of the source contract, not of an individual person's
#' name.** These functions never inspect a string and guess which mode to use.
#' A library that sniffs "does this look like free text?" has buried an
#' unauditable scientific decision in a heuristic; the caller knows what it
#' holds and must say so.
#'
#' @section Evidence for two modes:
#' An external linkage experiment (AMCB midwifery project, 2026-08-16) ran both
#' rules over the same 35,038 repository author strings against 22,309
#' certificants, holding everything except candidate generation constant.
#' `any_token` produced a 19% permutation collision proxy and 165 ambiguous
#' projects; `positional_ie` produced 13% and 82. The 103-row difference was 23
#' middle-name-only matches, 67 ambiguity resolutions, and 13 high-evidence
#' links only `any_token` could reach -- people who publish under a middle name.
#'
#' Those percentages are PERMUTATION COLLISION PROXIES. They are not
#' false-positive rates, precision, sensitivity, specificity or accuracy: no
#' adjudicated truth set exists for those links. Neither mode dominated, which
#' is why both are kept.
#'
#' @section What is reused rather than reimplemented:
#' Normalization is `normalize_name_key()` (`string_normalization.R`), which
#' folds Unicode, transliterates Latin to ASCII, and collapses whitespace.
#' Nickname equivalence is `are_nickname_variants()`
#' (`enhanced_name_parsing.R`). Nothing here re-derives either.
#'
#' @family name-matching
#' @name name_matching_primitives
NULL

# Normalization happens INSIDE every primitive. A caller must not be able to
# recreate a case-sensitivity bug by passing raw input: an upstream tokenizer
# that split on [^A-Z'] returned zero tokens for mixed-case input and silently
# matched nobody, which is a contract that cannot live in a comment.
.nm_key <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character(0))
  out <- normalize_name_key(as.character(x))
  # Apostrophes are DELETED, never treated as separators. Sources disagree on
  # them -- one holds "O'Connor", another "Oconnor" -- and splitting yields the
  # fragment "CONNOR", which then fails to equal "OCONNOR" and rejects a correct
  # match. Deleting makes both sides "OCONNOR".
  out <- gsub("['\u2019`]", "", out)
  out[is.na(x)] <- NA_character_
  out
}

.nm_blank <- function(x) is.na(x) | !nzchar(trimws(x))

#' Surname particles
#'
#' Components that are grammatical connectives rather than family names. A
#' particle ALONE is not identity evidence: "Van" against "van Erven" and "La"
#' against "de la Cruz" would otherwise be compatible on component subset,
#' recreating a candidate-collision mechanism inside the canonical layer.
#'
#' No existing particle list was found in this repository (only prose mentions
#' in the parser pipelines), so this is the definition. Deliberately
#' conservative: adding a real surname here would silently weaken matching.
#' @family name-matching
#' @keywords internal
NAME_SURNAME_PARTICLES <- c(
  "VAN", "VON", "DER", "DEN", "DE", "DEL", "DELLA", "DELA", "DI", "DA", "DU",
  "DO", "DOS", "DAS", "LA", "LE", "LO", "LOS", "LAS", "AF", "AV", "BIN", "IBN",
  "AL", "TER", "TEN", "OP", "ZU", "ST"
)

# Identity comparison must never fabricate pairings. Equal lengths, or scalar
# broadcast, or an error -- silent rep_len() recycling would compare person 1
# against person 3 and report the result as though it had been asked for.
.nm_pair_len <- function(a, b, what = c("a", "b")) {
  la <- length(a); lb <- length(b)
  if (la == lb) return(la)
  # A zero-length vector is not a scalar. Pairing it with anything non-empty is
  # a caller error, not a broadcast of nothing across everything.
  if (la == 0L || lb == 0L) {
    stop(sprintf(paste0("%s and %s: one input is empty (%d) and the other is ",
                        "not (%d). Refusing to recycle identity vectors."),
                 what[1], what[2], min(la, lb), max(la, lb)), call. = FALSE)
  }
  if (la == 1L || lb == 1L) return(max(la, lb))
  stop(sprintf(paste0("%s and %s must be the same length, or one must be ",
                      "length 1 for broadcasting; got %d and %d. Refusing to ",
                      "recycle identity vectors."), what[1], what[2], la, lb),
       call. = FALSE)
}

.nm_side_len <- function(first, middle, side = "first/middle") {
  if (is.null(middle)) return(invisible(TRUE))
  lf <- length(first); lm <- length(middle)
  # ASYMMETRIC ON PURPOSE. `first` defines how many person records there are,
  # because name_given_tokens() sizes its output from it. A middle vector may
  # therefore be the same length (one per record) or length 1 (one value
  # broadcast across records) -- but a LONGER middle vector against a single
  # first name is not broadcasting, it is silent truncation: middle
  # c("Ann","Jane","Louise") against first "Mary" would keep ANN and discard two
  # records.
  if (lf == lm || lm == 1L) return(invisible(TRUE))
  stop(sprintf(paste0("%s: middle name vector (%d) does not correspond to the ",
                      "first name vector (%d), which defines the record count. ",
                      "Refusing to recycle within a person record."),
               side, lm, lf), call. = FALSE)
}

#' Split a surname into comparable whole components
#'
#' @description
#' Returns the normalized whole-word components of a surname. Splitting on any
#' non-letter means "Barlow-Reed", "Barlow Reed" and "BARLOW  REED" produce the
#' same components, so hyphenation differences between two sources cannot cause
#' a miss.
#'
#' Particles (`van`, `de`, `du`, ...) are RETAINED as components rather than
#' stripped. They carry identity information, and because comparison is by
#' subset (see [names_have_compatible_surname()]) a source that omits the
#' particle still matches one that keeps it.
#'
#' @param x `character`: surname, possibly compound or hyphenated.
#' @return `list` of `character` component vectors, one per input element.
#'   Components shorter than 2 characters are dropped.
#' @family name-matching
#' @examples
#' name_surname_components("Barlow-Reed")   # list(c("BARLOW", "REED"))
#' name_surname_components("van Erven")     # list(c("VAN", "ERVEN"))
#' @export
name_surname_components <- function(x) {
  k <- .nm_key(x)
  lapply(k, function(s) {
    if (is.na(s) || !nzchar(s)) return(character(0))
    t <- strsplit(s, "[^A-Za-z]+")[[1]]
    t <- toupper(t[nzchar(t)])
    unique(t[nchar(t) >= 2L])
  })
}

#' Full given-name tokens, initials excluded
#'
#' @description
#' Tokens of length >= 2 drawn from the given and middle name fields. Initials
#' are excluded ON PURPOSE: "W." is compatible with every W and identifies
#' nobody, so it must never enter a set that is compared by intersection.
#' [name_leading_given()] retains them for the one comparison where an initial
#' does carry information.
#'
#' @param first,middle `character`: given and middle name fields. `middle` may
#'   be `NULL`.
#' @return `list` of `character` token vectors, one per input element.
#' @family name-matching
#' @export
name_given_tokens <- function(first, middle = NULL) {
  .nm_side_len(first, middle)
  f <- .nm_key(first)
  m <- if (is.null(middle)) rep("", length(f)) else rep_len(.nm_key(middle), length(f))
  m[is.na(m)] <- ""
  f2 <- f; f2[is.na(f2)] <- ""
  both <- trimws(paste(f2, m))
  lapply(both, function(s) {
    t <- strsplit(s, "[^A-Za-z]+")[[1]]
    t <- toupper(t[nzchar(t)])
    unique(t[nchar(t) >= 2L])
  })
}

#' The leading given name, initials retained
#'
#' @description
#' The first token of the given-name field, INCLUDING a single-letter initial.
#' This is the one place an initial must survive: "Dowdle, S. Addreina" has
#' leading given `S`, and dropping it would leave `ADDREINA` -- the middle name
#' -- masquerading as the first, which is exactly the collision the positional
#' mode exists to prevent.
#'
#' @param first `character`: the given-name field.
#' @return `character` of the same length; `NA` where absent.
#' @family name-matching
#' @export
name_leading_given <- function(first) {
  k <- .nm_key(first)
  vapply(k, function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_character_)
    t <- strsplit(s, "[^A-Za-z]+")[[1]]
    t <- t[nzchar(t)]
    if (length(t) == 0L) NA_character_ else toupper(t[1])
  }, character(1), USE.NAMES = FALSE)
}

.nm_is_initial <- function(x) !is.na(x) & nchar(x) == 1L & grepl("^[A-Za-z]$", x)

#' Do two surnames describe the same family name?
#'
#' @description
#' Component subset in either direction. This admits the legitimate variation
#' between sources -- `Nelson` against `Nelson-Becker`, `Dyer` against
#' `Dyer Hill` -- while rejecting the containment accidents that a substring
#' test allows.
#'
#' **Never substring containment.** `str_detect(a, fixed(b))` accepts
#' `Anderson` inside `Sanderson`, `Williams` inside `Williamson` and `Martin`
#' inside `Martinez`, each of which attributes one person's record to another.
#' Those differ WITHIN a component; subset differs BY a component.
#'
#' @param a,b `character`: two surnames.
#' @return `logical` of the recycled length. `NA` or empty on either side
#'   returns `FALSE` -- absence is never evidence.
#' @family name-matching
#' @examples
#' names_have_compatible_surname("Nelson", "Nelson-Becker")  # TRUE
#' names_have_compatible_surname("Anderson", "Sanderson")    # FALSE
#' @export
names_have_compatible_surname <- function(a, b) {
  name_surname_match_type(a, b) != "none"
}

#' How two surnames correspond, as an evidence type
#'
#' @description
#' The mechanics of surname comparison, reported as a TYPE rather than
#' collapsed to a Boolean, so a caller can weight exact identity above weaker
#' component-subset evidence. [names_have_compatible_surname()] is the Boolean
#' convenience wrapper (`!= "none"`).
#'
#' \describe{
#'   \item{`exact`}{normalized strings identical}
#'   \item{`separator_equivalent`}{same components, different separators --
#'     "Barlow-Reed" against "Barlow Reed"}
#'   \item{`concatenated_equivalent`}{one side dropped the separator entirely --
#'     "Abu-Ghazaleh" against "Abughazaleh". EXACT equality of the joined
#'     components, never containment, so ANDERSON cannot reach SANDERSON}
#'   \item{`component_subset`}{one component set nests in the other --
#'     "Nelson" in "Nelson-Becker", "Dyer" in "Dyer Hill". Requires at least one
#'     NON-PARTICLE component on the nesting side}
#'   \item{`none`}{no correspondence, or absent input}
#' }
#'
#' @param a,b `character`: two surnames. Equal lengths, or one of length 1.
#' @return `character` of the recycled length.
#' @family name-matching
#' @examples
#' name_surname_match_type("Nelson", "Nelson-Becker")  # "component_subset"
#' name_surname_match_type("Van", "van Erven")         # "none" (particle only)
#' @export
name_surname_match_type <- function(a, b) {
  n <- .nm_pair_len(a, b, c("a", "b"))
  ka <- rep_len(.nm_key(a), n); kb <- rep_len(.nm_key(b), n)
  ca <- name_surname_components(a); cb <- name_surname_components(b)
  ca <- rep_len(ca, n); cb <- rep_len(cb, n)

  vapply(seq_len(n), function(i) {
    x <- ca[[i]]; y <- cb[[i]]
    if (length(x) == 0L || length(y) == 0L) return("none")
    if (!is.na(ka[i]) && !is.na(kb[i]) && identical(ka[i], kb[i])) return("exact")
    if (setequal(x, y)) return("separator_equivalent")
    # Concatenation requires MORE THAN ONE component on the joined side.
    # With a single component paste(x, collapse = "") is just x, so the test
    # degenerates into plain membership -- i.e. subset -- and would report
    # "Van" against "van Erven" as concatenated equivalence, bypassing the
    # particle guard below.
    if ((length(x) > 1L && paste(x, collapse = "") %in% y) ||
        (length(y) > 1L && paste(y, collapse = "") %in% x)) {
      return("concatenated_equivalent")
    }
    # Component subset, but a particle alone is not identity evidence.
    nests <- function(small, big) {
      all(small %in% big) &&
        any(!small %in% NAME_SURNAME_PARTICLES)
    }
    if (nests(y, x) || nests(x, y)) return("component_subset")
    "none"
  }, character(1))
}

#' Which positions matched, under `any_token`
#'
#' @description
#' Retained as evidence so a caller can weight a shared FIRST name above a
#' shared MIDDLE name. A middle-name-only agreement is weak -- common middle
#' names collide across unrelated people -- and must remain distinguishable so
#' it is never silently promoted to identification.
#'
#' @inheritParams names_have_compatible_given
#' @return `character`: `"both_leading"`, `"one_leading"`, `"neither_leading"`,
#'   or `NA` when no full token is shared.
#' @family name-matching
#' @export
name_given_match_position <- function(a_first, b_first, a_middle = NULL,
                                      b_middle = NULL) {
  n <- .nm_pair_len(a_first, b_first, c("a_first", "b_first"))
  ta <- name_given_tokens(a_first, a_middle)
  tb <- name_given_tokens(b_first, b_middle)
  la <- name_leading_given(a_first)
  lb <- name_leading_given(b_first)
  ta <- rep_len(ta, n); tb <- rep_len(tb, n)
  la <- rep_len(la, n); lb <- rep_len(lb, n)
  vapply(seq_len(n), function(i) {
    shared <- intersect(ta[[i]], tb[[i]])
    if (length(shared) == 0L) return(NA_character_)
    a_lead <- !is.na(la[i]) && la[i] %in% shared
    b_lead <- !is.na(lb[i]) && lb[i] %in% shared
    if (a_lead && b_lead) "both_leading"
    else if (a_lead || b_lead) "one_leading"
    else "neither_leading"
  }, character(1))
}

#' Are two given names compatible?
#'
#' @description
#' Two modes; the caller picks. See [name_matching_primitives] for the source
#' contract that determines which.
#'
#' @section mode = "positional_ie":
#' For structured-to-structured sources, where field order is reliable. The
#' LEADING given names are compared, and agree when any of these holds:
#' \itemize{
#'   \item they are equal after normalization;
#'   \item they are nickname variants of one another
#'         ([are_nickname_variants()]), evaluated on FULL tokens only -- an
#'         initial is never a nickname;
#'   \item **initial expansion**: one side is a single alphabetic character
#'         \emph{I} and the other is a full token of >= 2 characters beginning
#'         with \emph{I}.
#' }
#'
#' Invariants:
#' \itemize{
#'   \item an initial NEVER identifies a person on its own -- initial expansion
#'         requires a full token on the opposite side;
#'   \item two initials on both sides (`S.` vs `S.`) do NOT admit a pair, even
#'         when they agree;
#'   \item a missing or empty given name on either side yields `FALSE`.
#' }
#'
#' @section mode = "any_token":
#' For unreliable free text against a structured record, where order cannot be
#' trusted -- an author byline may be reordered, credential-laden, or published
#' under a middle name. Any shared FULL token admits a candidate. Position is
#' not discarded: call [name_given_match_position()] alongside and carry the
#' result, because a middle-only match must stay distinguishable.
#'
#' @param a_first,b_first `character`: given-name fields.
#' @param a_middle,b_middle `character`: middle-name fields; may be `NULL`.
#' @param mode `character(1)`: `"positional_ie"` or `"any_token"`. **Required** --
#'   omitting it is an error, never a default.
#' @return `logical` of the recycled length.
#' @family name-matching
#' @examples
#' names_have_compatible_given("Chad", "C", mode = "positional_ie")   # TRUE
#' names_have_compatible_given("C", "S", mode = "positional_ie")      # FALSE
#' @export
names_have_compatible_given <- function(a_first, b_first, a_middle = NULL,
                                        b_middle = NULL,
                                        mode) {
  # NO DEFAULT. `mode = c("positional_ie","any_token")` plus match.arg() would
  # silently select positional_ie when the argument is omitted, which violates
  # the whole contract: mode is a property of the SOURCE, and a caller that did
  # not think about it must not be handed one by accident.
  if (missing(mode)) {
    stop("mode must be explicitly specified: positional_ie or any_token",
         call. = FALSE)
  }
  mode <- match.arg(mode, c("positional_ie", "any_token"))
  n <- .nm_pair_len(a_first, b_first, c("a_first", "b_first"))

  if (identical(mode, "any_token")) {
    ta <- name_given_tokens(a_first, a_middle)
    tb <- name_given_tokens(b_first, b_middle)
    ta <- rep_len(ta, n); tb <- rep_len(tb, n)
    return(vapply(seq_len(n),
                  function(i) length(intersect(ta[[i]], tb[[i]])) > 0L,
                  logical(1)))
  }

  la <- rep_len(name_leading_given(a_first), n)
  lb <- rep_len(name_leading_given(b_first), n)
  vapply(seq_len(n), function(i) {
    x <- la[i]; y <- lb[i]
    if (.nm_blank(x) || .nm_blank(y)) return(FALSE)
    ix <- .nm_is_initial(x); iy <- .nm_is_initial(y)
    # Two initials never identify a person, even when they agree.
    if (ix && iy) return(FALSE)
    if (identical(x, y)) return(TRUE)
    if (ix) return(substr(y, 1, 1) == x)
    if (iy) return(substr(x, 1, 1) == y)
    # Full tokens on both sides: nickname equivalence is the canonical rule.
    isTRUE(tryCatch(are_nickname_variants(x, y), error = function(e) FALSE))
  }, logical(1))
}
