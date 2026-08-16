# isochrones-ci

**A public CI and scientific-validation harness. Not the isochrone pipeline.**

This repository is a small, independent verifier. It installs a specified commit
of the production code, runs it against tiny synthetic fixtures, and checks the
answers against an independently written reference implementation.

> This repository is a public CI and scientific-validation harness. It is not the
> canonical implementation of the isochrone pipeline and should not contain
> production datasets or independently maintained production algorithms.

Production features belong in the canonical repository. If you find yourself
adding a feature here, you are in the wrong repo.

---

## Why it exists

A test suite that lives inside the code it tests shares that code's blind spots.
If a helper is subtly wrong, both the implementation and its tests use the wrong
helper, and the suite certifies the wrong answer with total confidence.

So this repository:

- writes its own **reference implementation**, from the published equations,
  which never calls production;
- runs on **synthetic worlds** small enough to hold in your head;
- tries hard to make production give a wrong answer, and reports when it cannot;
- proves it can fail, by running the same assertions against **deliberately
  broken code** and requiring them to go red.

The goal is not a green badge. It is:

> A small, independently understandable test universe tried hard to make the
> production implementation give the wrong scientific answer, and it could not.

---

## What it tests, and what it cannot

**Tested:** `mufflyt/mufflyaccess` — the public cross-repo single source of truth
that `isochrones`, `cliff` and `twostep` all read their canonical constants from.
Canonical drive-time bands, CONUS scope, census denominators, MOE→CI arithmetic,
zero-safe division, national→state allocation, RUCA rurality, identifier
canonicalisation, published prevalence tables.

**Not tested, and this is a real limitation:** `mufflyt/isochrones` itself is a
**private** repository. A public harness holds no credential for it, and giving
one to a workflow that runs pull-request code would be a security defect far
worse than the coverage gap. So the routing engine, the Valhalla isochrone
generation, the E2SFCA pipeline and the manuscript path are **outside** what this
harness can reach. The manual workflow refuses a private target explicitly rather
than half-working.

See [`SCIENTIFIC_VALIDATION.md`](SCIENTIFIC_VALIDATION.md) for exactly which
claims the suite proves and which it does not.

---

## How the production commit is chosen

Every run resolves and **records** a SHA before installing anything. A result
that cannot name the commit it tested is not reproducible.

| Workflow | Repo | Ref |
|---|---|---|
| `pr.yml` | `mufflyt/mufflyaccess` | `main`, resolved to a SHA at run time |
| `nightly.yml` | `mufflyt/mufflyaccess` | `main`, resolved to a SHA at run time |
| `manual.yml` | your input | your input (branch, tag or raw SHA) |

The resolved SHA appears in the job summary, in `artifacts/summary.txt`, and for
manual runs in `artifacts/run-parameters.txt`.

---

## The three tiers

### PR (`pr.yml`) — the fast science gate

Push to `main` and every pull request. ~2-3 minutes. Deterministic tests only:
contracts, production contract, mathematical invariants, reference cross-check,
end-to-end golden fixture, reproducibility, geometry. 590 assertions.

### Nightly (`nightly.yml`) — the serious battery

08:20 UTC daily = **02:20 America/Denver** in MDT (UTC-6), 01:20 in MST (UTC-7).
The `:20` offset keeps this repo out of the same scheduling queue as everything
else on the account. Also available via `workflow_dispatch`.

Everything the PR gate runs, plus 200 randomized worlds, the adversarial corpus,
the metamorphic layer, and the mutation/sabotage tests — across R `release` and
`oldrel-1`. It additionally checks that the committed fixtures still regenerate
from their seed, and that the suite left the working tree unmodified.

### Manual (`manual.yml`) — test a requested commit

`workflow_dispatch` with inputs: `production_repo`, `production_ref`,
`random_seed`, `random_iterations`, `run_mutation_tests`, `r_version`,
`retain_artifacts_days`.

---

## Reproducing a CI failure locally

Everything CI runs, you can run. There is no CI-only path.

```bash
git clone https://github.com/mufflyt/isochrones-ci
cd isochrones-ci

# install the SAME production commit the failing run reported
Rscript -e 'remotes::install_github("mufflyt/mufflyaccess@<SHA FROM THE SUMMARY>")'

# the fast gate
Rscript scripts/run-ci.R --tier=pr

# the full battery, with the seed and world count from the failing run
HARNESS_SEED=<seed> HARNESS_RANDOM_WORLDS=<n> Rscript scripts/run-ci.R

# and confirm the run was complete, not merely non-erroring
Rscript scripts/verify-test-accounting.R
```

**For a randomized failure**, the seed is printed in the job summary and saved in
`artifacts/summary.txt`. Rerun with that `HARNESS_SEED` and you get the identical
world. Every generator is a pure function of its seed, and
`test-reproducibility.R` asserts that, including that generating a world does not
perturb the caller's RNG stream.

**Download the artifacts** from the failed run (Actions → the run → Artifacts).
They contain `summary.txt`, `test-results.csv`, `test-accounting.txt`,
`SUITE_COMPLETED`, `sessionInfo.txt`, `installed-packages.csv`, and where
relevant `mutation-report.csv` and `allocation-observation.csv`.

---

## What counts as a scientific failure

Not every red build is the same thing, and the workflows keep these apart:

| Class | Looks like | Means |
|---|---|---|
| Environment | `setup-r-dependencies` fails | the runner, not the science |
| Installation | "production package failed to install" | the production commit does not build |
| Execution | an R error inside a test | the harness or production crashed |
| **Scientific disagreement** | a cross-check, invariant or golden assertion fails | **production computes something different from the equations** |
| Incomplete | `SUITE_COMPLETED` absent, or planned files did not run | the run must not be read as a pass either way |

Only the fourth is a scientific failure. The last is the one this repository
cares most about being unable to hide.

---

## Regenerating fixtures

```bash
Rscript scripts/regenerate-fixtures.R            # report what WOULD change, exit 1
Rscript scripts/regenerate-fixtures.R --accept   # write, after you read the diff
```

Golden outputs are never rewritten silently. A golden file that updates itself
whenever the code changes records nothing, because it can never disagree. The
nightly workflow fails if the committed fixtures no longer regenerate from their
recorded seed.

---

## Public-data and privacy policy

Everything in `fixtures/` is **synthetic**, generated from a recorded seed by
`R/fixture_helpers.R`. There is:

- no PHI and no patient-level data of any kind;
- no real physician, no real NPI, no real name, no real practice address;
- no licensed or non-redistributable dataset;
- no credential, token, API key or private URL;
- no `.Renviron`.

Provider identifiers are synthetic and 9-prefixed, and `test-contracts.R`
asserts that property so a hand-edit that pasted a real NPI would fail CI.
Geography is a stylised grid at Colorado-ish coordinates: realistic distances,
no real census tract.

---

## Adding a regression test after finding a bug

1. Reduce it to the **smallest** world that reproduces it.
2. Add it to `tests/testthat/test-adversarial.R` with a comment saying what
   broke and why the shape matters.
3. If the bug is a plausible *class* of error, add a matching mutant to
   `R/mutants.R` and a probe to `test-mutation-sabotage.R`, so the suite proves
   it would catch that class again.
4. If a golden value legitimately changes, regenerate with `--accept` and put the
   before/after numbers in the commit message.

See [`CONTRIBUTING.md`](CONTRIBUTING.md).

---

## Deliberately out of scope

No Shiny app. No website. No production data pipeline. No manuscript materials.
No general feature development. No large outputs. No duplicated business logic
except the clearly-labelled reference implementation in `R/reference_helpers.R`,
which exists solely to disagree with production.

Total fixture size is capped at 1 MB by a test, on purpose.
