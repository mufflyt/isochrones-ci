# Fixtures

Everything here is **synthetic**, generated from a recorded seed. Nothing is
derived from a real person, provider, address, or census tract. This repository
is public and these files are redistributable under its MIT licence.

See `manifest.yml` for the version, seed, and source declaration.

| File | What it is | Why it exists |
|---|---|---|
| `inputs/providers.csv` | 8 synthetic providers: id, synthetic 10-digit id, lon/lat, supply | the supply side. Includes one deliberately **isolated** provider (unreachable within any canonical band) and two at **identical coordinates** (the exact-tie case) |
| `inputs/tracts.csv` | 25 synthetic tracts: id, lon/lat, population | the demand side. Includes two **zero-population** tracts, so zero-vs-undefined semantics have something to bite on |
| `inputs/travel_times.csv` | the complete 8 × 25 provider→tract matrix, in minutes | a complete matrix, so a missing pair is a defect rather than a modelling choice |
| `expected/golden_by_band.csv` | per-band summary: tracts reached, population share, providers used, accessibility sum/mean/max, zero-access count | the end-to-end golden result, **structured** so a failure names the claim that broke rather than saying "the blob changed" |
| `expected/golden_tracts_60.csv` | per-tract accessibility at the primary 60-minute band | catches reordering, which aggregate totals cannot see |

## Regenerating

```bash
Rscript scripts/regenerate-fixtures.R            # show what would change; exit 1
Rscript scripts/regenerate-fixtures.R --accept   # write it
```

Never rewritten silently. A golden file that updates itself records nothing.

## Source and licence

| field | value |
|---|---|
| origin | `R/fixture_helpers.R::make_world()` |
| seed | 20260816 |
| licence | MIT (this repository) |
| external data incorporated | none |
| redistributable | yes |
| PHI / patient-level data | none |
| real NPIs, names, addresses | none |

The geography is a stylised grid at Colorado-ish coordinates so projected
distances are realistic. No polygon here is a real census tract.

Provider identifiers are 9-prefixed and synthetic;
`tests/testthat/test-contracts.R` asserts that property, so a hand-edit that
pasted a real NPI fails CI.

## Size

Capped at 1 MB in total, enforced by a test. The moment fixtures grow past that,
failures stop being reproducible in your head and this stops being a harness.
