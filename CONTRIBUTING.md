# Contributing to CentroidR

Thank you for contributing to `CentroidR`.

## Prerequisites

- **R**: `>= 4.5.0`
- **Package dependencies** (from `DESCRIPTION`):
  - Bioconductor: `BiocParallel`, `MsCoreUtils`, `mzR`, `Spectra`
  - Testing: `tinytest`
- **Development tools** (recommended for local verification):
  - R packages: `roxygen2`, `covr`, `lintr`, `spelling`, `cffr`, `grateful`, `knitr`
  - CLI tools: `air`, `jarl`, `quarto`, `prek`

Install R dependencies for local development:

```r
install.packages(c("BiocManager", "tinytest", "roxygen2", "covr", "lintr", "spelling"))
BiocManager::install(c("BiocParallel", "MsCoreUtils", "mzR", "Spectra"))
```

## Development workflow

### Run tests

Run the full `tinytest` suite:

```r
res <- tinytest::run_test_dir("inst/tinytest")
stopifnot(all(as.logical(res)))
```

Run a single test file:

```r
res <- tinytest::run_test_file("inst/tinytest/test-centroid-math-algorithms.R")
stopifnot(all(as.logical(res)))
```

Note that `tinytest` is the only package in `Suggests`; do not add `testthat`, `xml2`, or other test-only dependencies without updating `DESCRIPTION`.

### Check test coverage

CI enforces a 95% coverage floor (`covr`). Run locally with:

```r
cov <- covr::package_coverage()
print(cov)
stopifnot(covr::percent_coverage(cov) >= 95)
```

### Format and lint

Format R code with `air`, run static checks with `jarl` and `lintr`, and validate pre-commit hooks with `prek`:

```bash
air format .
air format . --check
jarl check .
Rscript -e 'lints <- lintr::lint_package(); print(lints); stopifnot(length(lints) == 0L)'
prek run --all-files
```

`air` is the canonical code formatter, and `.lintr` disables `indentation_linter` to avoid conflicting with `air`.

### Update documentation

Never edit `man/*.Rd` or `README.md` by hand; regenerate them from source:

```bash
Rscript -e 'roxygen2::roxygenise()'
quarto render README.qmd --to gfm
Rscript -e 's <- spelling::spell_check_package(); print(s); stopifnot(nrow(s) == 0L)'
```

### Run R CMD check

Verify a clean package build with zero errors and zero warnings:

```bash
R CMD build --no-manual .
R CMD check --no-manual CentroidR_*.tar.gz
```

## Submitting changes

1. Keep each pull request focused on one logical change.
2. Add regression tests in `inst/tinytest/` for bug fixes or new behavior.
3. Confirm `tinytest`, `lintr`, `air format . --check`, `jarl check .`, and `R CMD check` pass before opening the pull request.
