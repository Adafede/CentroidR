# CentroidR Agent Guide

Operational reference for coding agents. Human-facing docs (`README.qmd`,
`README.md`, `CONTRIBUTING.md`, `NEWS.md`, `R/`, `man/`, `CITATION.cff`,
`codemeta.json`) must never mention or link to `AGENTS.md` or `.agents/`.

## Commands

Run checks cheapest-first:

- Format code: `air format .`
- Check formatting: `air format . --check`
- Static checks: `jarl check .`
- Lint package:
  `Rscript -e 'l <- lintr::lint_package(); print(l); stopifnot(length(l) == 0L)'`
- Single test file:
  `Rscript -e 'res <- tinytest::run_test_file("inst/tinytest/<file>.R"); print(res); stopifnot(all(as.logical(res)))'`
- Full test suite:
  `Rscript -e 'res <- tinytest::run_test_dir("inst/tinytest"); print(res); stopifnot(all(as.logical(res)))'`
- Coverage (>= 95% enforced in CI):
  `Rscript -e 'cov <- covr::package_coverage(); print(cov); stopifnot(covr::percent_coverage(cov) >= 95)'`
- Regenerate roxygen docs: `Rscript -e 'roxygen2::roxygenise()'`
- Render README: `quarto render README.qmd --to gfm`
- Spell check:
  `Rscript -e 's <- spelling::spell_check_package(); print(s); stopifnot(nrow(s) == 0L)'`
- Pre-commit hooks: `prek run --all-files`
- Full package check:
  `R CMD build --no-manual . && R CMD check --no-manual CentroidR_*.tar.gz`
  (remove built `.tar.gz` and `CentroidR.Rcheck/` afterwards)

## Repository Map

- `R/CentroidR-package.R`: package docs and `.onLoad()` calling
  `mzR::pwiz.version()`.
- `R/centroid_one_file.R`: exported `centroid_one_file()`, spectrum batching,
  peak grouping, 1D Morse-Smale valley splitting, centroid/intensity estimators,
  Gauss-Seidel deconvolution, internal logger, and `.fix_xml()`.
- `inst/scripts/centroiding.R`: zero-dependency CLI (`run_cli()`,
  `parse_cli_args()`, `centroid_one_file_cli()`).
- `inst/Dockerfile`: installs `CentroidR` and copies
  `inst/scripts/centroiding.R` to `/home/bioc-user/centroiding.R`.
- `inst/tinytest/`: `tinytest` suite + `helper-logging.R`; `tests/tinytest.R`
  invokes `tinytest::test_package("CentroidR")`.
- `README.qmd` -> `README.md`; roxygen -> `man/*.Rd`. Never edit generated
  outputs by hand.

## Constraints and Conventions

- `Suggests` in `DESCRIPTION` contains ONLY `tinytest`. Never use `testthat`,
  `xml2`, `pkgload`, or `lintr` in `R/` or `inst/tinytest/`.
- `.air.toml` (`line-width = 80`, 2-space indent) is canonical; `.lintr` sets
  `indentation_linter = NULL` so `air` and `lintr` never conflict.
- Keep `.Rbuildignore` and `inst/WORDLIST` sorted in `C` locale
  (`LC_ALL=C sort -u -o <file> <file>`).
- Never use `return()` as the final expression in a function/block (fails
  `lintr` `return_linter`).
- Subprocess CLI tests read the installed package for
  `CentroidR::centroid_one_file()`; run `R CMD INSTALL --no-multiarch .` before
  `tinytest::run_test_file("inst/tinytest/test-centroid-cli.R")` when testing
  `R/` changes via CLI.

## Definition of Done

Before finishing any change:

1. `air format . --check`, `jarl check .`, and `lintr::lint_package()` report 0
   issues.
2. Relevant `tinytest` files and full suite pass (`all(as.logical(res))`),
   coverage remains >= 95%, and `R CMD check --no-manual` ends with `Status: OK`
   (`0 errors | 0 warnings | 0 notes`).
3. If public R API or defaults changed, `inst/scripts/centroiding.R`,
   `man/*.Rd`, `README.qmd`/`README.md`, and `NEWS.md` are synchronized.

## Deep Reference

- Mathematical, I/O, logging, and CLI invariants:
  [`.agents/invariants.md`](.agents/invariants.md)
- Modification workflows: [`.agents/playbooks.md`](.agents/playbooks.md)
