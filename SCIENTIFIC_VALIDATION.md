# What this suite actually proves

Every row below is a claim the tests make. The last column says what a failure
would mean — because a red build that nobody can interpret gets ignored, and an
ignored check is worse than no check.

The final section is the more important one: **what this suite does not prove.**

---

## Claims

| Claim | Test | Method | Failure means |
|---|---|---|---|
| Tracts reachable in 30 min are a subset of those reachable in 60, and 60 of 120 | `test-mathematical-invariants.R` :: coverage monotonicity | set inclusion over ≥25 random worlds | threshold or routing regression |
| Reachable population never falls as the band widens | invariants :: population monotonicity | monotone sequence over 4 bands | band mix-up or comparison flipped |
| Removing a provider cannot increase raw coverage | invariants :: provider monotonicity | drop-one over random worlds | provider set is not being honoured |
| Allocation conserves the total exactly | invariants + cross-check :: conservation | `sum(alloc) == total` | mass created or lost in allocation |
| Zero-weight units still receive a row, and receive exactly 0 | invariants :: conservation | element check | units silently dropped before normalising |
| `0`, `NA`, `NaN`, `Inf`, empty geometry and "no reachable provider" stay distinct | invariants :: zero semantics | explicit case table | undefined values collapsing into a confident 0 |
| A provider serving zero population has an **undefined** ratio, not `Inf` | adversarial | tiny fixture | `Inf` poisoning every downstream sum |
| Supply is not double-counted | invariants :: supply bound | Σ Aₜ·Popₜ ≤ Σ supply | catchment overlap counted twice |
| Production `safe_divide` refuses `x/0` and `0/0` | cross-check | reference comparison | a rate becoming `Inf` three joins later |
| Production Wilson CI matches the published formula | cross-check | independent Wilson implementation, random x and n | interval arithmetic regression |
| Production MOE 90→95 factor is the z-score ratio | cross-check | `qnorm(.975)/qnorm(.95)`, tol 1e-3 | wrong z, e.g. 1.96/1.65 |
| Production identifier canonicalisation matches the documented rules | cross-check | reference comparison over 8 shapes | identifiers silently dropped or mangled |
| RUCA metro/nonmetro boundary sits between 3 and 4 | cross-check | partition comparison | every micropolitan tract reclassified |
| Canonical bands are still 30/60/120/180 | production contract | equality with `CANONICAL_BANDS` | production changed the band set; fixtures are stale |
| The primary band is a member of the canonical set | production contract | membership + seconds/minutes agreement | headline band the pipeline does not generate |
| CONUS and non-contiguous sets are disjoint; 49 units | production contract | set algebra | scope regression (AK/HI/PR leaking in) |
| Published prevalence values match Wu 2014 Table 1 exactly | production contract | value-by-value transcription | transcription error in a source table |
| "Any PFD" ≥ each individual disorder | production contract | set-inclusion constraint | impossible prevalence table |
| Answers do not depend on row order | metamorphic | shuffle providers, tracts, travel times; compare **by name** | hidden state or order dependence |
| Answers do not depend on identifier text | metamorphic | bijective relabel that reverses sort order | code relying on identifiers sorting a particular way |
| Answers do not depend on chunk size | metamorphic | chunk 1, 5, 10, 100 vs unchunked | step-1 denominators computed per chunk |
| Scaling all populations by k scales accessibility by 1/k | metamorphic | dimensional analysis | a population term entering where it should not |
| Duplicate providers at identical coordinates both count | metamorphic + adversarial | tiny fixture | silent coordinate-based deduplication halving supply |
| A tract exactly **at** the threshold is inside it | adversarial | boundary fixture | `<` where the specification says `<=` |
| Geometry is valid, non-empty, and carries a known CRS | geography | `st_is_valid`, `st_is_empty`, `st_crs` | invalid or CRS-less geometry entering the pipeline |
| Identifiers stay attached to their own geometry through a reprojection | geography | point-in-own-polygon after transform | join reordering during reprojection |
| Coordinate order is not silently reversed | geography | CONUS sign pattern | classic lon/lat swap |
| Area is computed in a projected CRS | geography | plausibility band on m² | area taken in degrees |
| Band discs nest: 30 ⊂ 60 ⊂ 120 ⊂ 180 | geography | `st_difference` area ratio, tol 1e-6 | containment check broken |
| Same seed reproduces bit-identically | reproducibility | repeated execution, `identical()` | nondeterminism |
| Generating a world does not perturb the caller's RNG | reproducibility | `.Random.seed` comparison | "reproducible" being quietly false under reordering |
| Committed fixtures still regenerate from their seed | reproducibility + nightly | regenerate and diff | a hand-edited, no-longer-reproducible fixture |
| The golden E2E result is unchanged | end-to-end | structured per-band and per-tract comparison | any scientific regression reaching the output |
| The suite can actually detect wrong science | mutation sabotage | 12 controlled mutants, 12 probes | the validation suite has a hole |

---

## Deliberately **not** asserted, and why

**Accessibility is not monotone in the threshold.** Under 2SFCA a larger
threshold enlarges every provider's catchment, enlarging the step-1 denominator
and *shrinking* each ratio. The committed golden fixture shows exactly this:
summed accessibility rises from 30 to 120 minutes and then **falls** at 180.
Coverage is monotone; accessibility is not. Asserting otherwise would generate
permanent false alarms.

**Removing a provider can raise another tract's 2SFCA score**, because the
removed provider's catchment population leaves its competitors' denominators.
Provider monotonicity is therefore asserted for *coverage* only.

**Prevalence does not rise with age for every disorder.** The suite originally
asserted that it did. It is false: in Wu 2014, pelvic organ prolapse *declines*
from 0.047 (65-79) to 0.040 (80+). Production was right and the harness was
wrong. The assertion is now an exact transcription check plus the real
constraint that "any PFD" ≥ each component.

**Production's allocator is not proportional at small totals.** Measured
2026-08-16 against `mufflyaccess`:

| total | worst deviation from the exact share | states receiving anything |
|---|---|---|
| 1 | 0.88 | 1 of 49 |
| 7 | **5.17** (California gets 6; its exact share is 0.83) | 2 of 49 |
| 49 | 3.22 | 29 of 49 |
| 1,000 | 2.00 | 49 of 49 |
| 1,306 | 0.48 | 49 of 49 |
| 100,000 | 1.16 | 49 of 49 |

Mass is conserved exactly in every case, so nothing is lost or invented, and the
real operating scale (~1,300) is well behaved. Whether the small-total behaviour
is an intended greedy rule or a rounding defect is a question for the production
maintainer; the harness records the numbers and declines to guess. The
proportionality assertion is therefore an **empirical regression bound** (2.5
units) fitted to observed behaviour of an unspecified rounding rule — it would
catch a drift to 50 units off, and would not catch a drift to 2.4. This is stated
in the test itself, not just here.

---

## What this suite does **not** prove

Stated plainly, because a validation document that only lists successes is
marketing.

1. **Nothing about `mufflyt/isochrones`.** That repository is private; a public
   harness holds no credential for it. Routing, Valhalla isochrone generation,
   the E2SFCA pipeline, tract harmonisation, the retirement classifiers and the
   manuscript path are all untested here.
2. **Nothing about real data.** Every fixture is synthetic. A defect that only
   appears at national scale, on real geography, or on the actual provider
   distribution will not be seen.
3. **Nothing about routing.** The travel-time model is distance ÷ speed with a
   circuity factor. It is not a router and does not pretend to be. Real
   isochrones are not discs.
4. **Nothing about performance**, memory, or the parallel execution paths
   production actually uses.
5. **Nothing about the parts of `mufflyaccess` not listed above** — 120 exports
   exist; roughly a dozen are checked here.
6. **The reference could be wrong.** It is independent, not infallible. Where the
   reference and production agreed and both were wrong, this suite would say
   nothing. The mutation layer bounds this: 12 plausible errors are demonstrably
   caught, which is evidence, not proof.
7. **Passing does not mean the science is right.** It means these specific claims
   held, against these fixtures, at that production commit.

---

## Mutation testing: evidence the suite bites

12 controlled mutants, each a plausible mistake rather than a nonsense one. All
12 are killed; the killing probe is named for each.

| Mutant | Killed by |
|---|---|
| threshold `<=` → `<` | `threshold_boundary` |
| 30 and 60 bands swapped | `coverage_monotone`, `band_share_identity` |
| undefined ratio coerced to 0 | `undefined_ratio_is_na`, `ratio_has_denominator` |
| step-1 denominator dropped | `undefined_ratio_is_na`, `ratio_has_denominator` |
| population-weighted share → unweighted mean | `band_share_identity` |
| allocator drops zero-weight units, loses mass | `allocation_conserves` |
| Wald interval not clamped to [0,1] | `ci_within_unit_interval` |
| MOE conversion uses 1.96/1.65 | `moe_factor_exact` |
| RUCA boundary moved to ≤4 | `rurality_boundary` |
| `x/0` allowed through as `Inf` | `divide_by_zero_is_na` |
| short identifiers rejected instead of zero-padded | `short_id_is_padded` |
| containment asserted in the wrong direction | `containment_direction` |

A sanity test first requires **every probe to pass on unmutated code**, so a
"kill" cannot be a broken probe. Mutants are restored via `on.exit`, and a test
asserts that restoration happens even when the probe throws — a leaked mutant
would silently corrupt every later test.
