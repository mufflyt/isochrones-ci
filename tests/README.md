# Tests

Run them through the CI entry point, which is the same path CI uses:

```bash
Rscript scripts/run-ci.R --tier=pr   # fast gate
Rscript scripts/run-ci.R             # full battery
```

There is no `tests/testthat.R`. This repository is **not an installable
package** -- `DESCRIPTION` exists so that `r-lib/actions/setup-r-dependencies`
can resolve dependencies, and `R CMD check` is deliberately not claimed. The
helpers in `R/` are plain scripts sourced by `helper-harness.R`, so a reviewer
can read them without knowing anything about package namespaces.
