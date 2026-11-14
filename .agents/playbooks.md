# Task Playbooks

## 1. Change Centroiding Parameters, Math, or Estimators

1. Update validation and math in `R/centroid_one_file.R`. Check `.validate_*()`
   guards, edge cases (length < 3, zeros, negative values, `NA`), and keep
   roxygen `@param` / `@details` accurate.
2. Keep `inst/scripts/centroiding.R` in lockstep:
   - `print_cli_help()` (wrap lines `<= 80` chars to keep `lintr` happy)
   - `parse_cli_args()` (`int_keys` / `num_keys` / `lgl_keys` / `chr_keys`)
   - `centroid_one_file_cli()` argument passthrough
3. Add targeted unit and boundary tests in
   `inst/tinytest/test-centroid-math-algorithms.R` and CLI tests in
   `inst/tinytest/test-centroid-cli.R`.
4. Verify in order:
   - `air format . && air format . --check`
   - `jarl check .`
   - `Rscript -e 'l <- lintr::lint_package(); print(l); stopifnot(length(l) == 0L)'`
   - `R CMD INSTALL --no-multiarch .`
   - `Rscript -e 'res <- tinytest::run_test_file("inst/tinytest/test-centroid-math-algorithms.R"); print(res); stopifnot(all(as.logical(res)))'`
   - `Rscript -e 'res <- tinytest::run_test_dir("inst/tinytest"); print(res); stopifnot(all(as.logical(res)))'`
5. Run `Rscript -e 'roxygen2::roxygenise()'` if public signatures or docs
   changed, and document user-facing changes in `NEWS.md`.

## 2. Change mzML I/O, Batching, Metadata, or `.fix_xml()`

1. Check `.agents/invariants.md` for self-overwrite protection, MS3+ passthrough
   order, zero-value metadata preservation, temp dir isolation, XML attribute
   escaping, and chunked streaming.
2. Add or update tests in `inst/tinytest/test-centroid-keep-empty.R` or
   `inst/tinytest/test-centroid-pipeline.R` (remembering
   `centroidr_reset_logging()` after `centroid_one_file()`).
3. Avoid quoting XML strings in single quotes (`'<mzML>'`) without embedded
   double quotes---`lintr` enforces `quotes_linter` double quotes.
4. Run targeted tests, then `covr::package_coverage()` (`>= 95%`, target
   `100%`).

## 3. Update Documentation and Package Metadata

1. Source files only:
   - API docs: edit roxygen comments in `R/`, then run
     `Rscript -e 'roxygen2::roxygenise()'`.
   - README: edit `README.qmd`, run `R CMD INSTALL --no-multiarch .`, then run
     `quarto render README.qmd --to gfm` (remove stray `README.html` /
     `README_files` if generated).
   - Citation: edit `.github/workflows/update-citation-cff.yaml` keys if
     metadata keys change, and regenerate `CITATION.cff` via
     `cffr::cff_write(...)`.
2. Run formatting and checks:
   - `prek run --all-files` (re-run once if `panache-format` reformats
     Markdown/QMD; if `README.qmd` changed, re-render `README.md` cleanly before
     finishing).
   - `Rscript -e 's <- spelling::spell_check_package(); print(s); stopifnot(nrow(s) == 0L)'`
     (add only real technical terms to `inst/WORDLIST` and keep sorted with
     `LC_ALL=C sort -u`).
   - Confirm zero references to `AGENTS.md` or `.agents/` across human docs
     (`README*`, `CONTRIBUTING.md`, `NEWS.md`, `R/`, `man/`, `CITATION.cff`,
     `codemeta.json`).
