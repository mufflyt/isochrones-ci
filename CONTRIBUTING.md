# Contributing

This repository stays small on purpose. The bar for adding anything is: **does
it increase the chance of catching a wrong scientific answer?**

## What belongs here

- a new invariant, metamorphic property, or adversarial fixture;
- a regression fixture for a bug that was actually found;
- a new controlled mutant, when it represents a plausible class of error;
- a reference implementation of a metric production computes.

## What does not

Production features, real datasets, anything over 1 MB, a Shiny app, a website,
manuscript materials. `tests/testthat/test-contracts.R` enforces the size cap.

## After finding a real bug

1. Reduce it to the smallest world that reproduces it, and add that to
   `tests/testthat/test-adversarial.R`. Say in a comment what broke and why the
   shape matters — a fixture without a reason becomes unmaintainable.
2. If the bug is an instance of a *class*, add a mutant to `R/mutants.R` and a
   probe to `test-mutation-sabotage.R`. The mutant must be a plausible edit:
   "return 42" proves nothing, "`<` instead of `<=`" proves a lot.
3. Run the suite and confirm the new probe passes on clean code AND fails under
   the mutant. The sanity test enforces the first half; you should check the
   second by hand once.

## Rules that are not negotiable

**A guard that cannot fail is worse than no guard**, because it converts "nobody
looked" into "we verified it". Every scan has a tripwire that fails when it
inspects nothing. Every gate has a negative control.

**`require_sf()` fails; it does not skip.** Geometry correctness is a stated
claim. A green run with the geometry tests skipped asserts something that was
not checked.

**Golden files are never rewritten silently.** `scripts/regenerate-fixtures.R`
reports a diff and exits non-zero; only `--accept` writes. Put the before/after
numbers in the commit message.

**The reference must never call production.** If `R/reference_helpers.R` ever
imports the thing it is checking, the cross-check certifies agreement between a
function and itself.

**Skips are recorded.** Use `skip_recorded()`, not bare `skip()`, so the job
summary can say how much a run did not check.

## Running everything locally

```bash
Rscript scripts/run-ci.R --tier=pr      # fast gate
Rscript scripts/run-ci.R                # full battery
Rscript scripts/verify-test-accounting.R
Rscript scripts/regenerate-fixtures.R   # fixture drift check
```
