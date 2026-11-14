# Architectural Invariants

Do not violate these invariants when modifying `CentroidR`.

## Numerical and Algorithmic (`R/centroid_one_file.R`)

- **Gaussian amplitude vs apex above baseline**: `.fit_gaussian_peak()`
  separates net Gaussian amplitude `amp` from continuous baseline-shifted peak
  height `apex = amp + base_applied` (where
  `base_applied = min(edge_left, edge_right)` when `baseline_subtract = TRUE`,
  else `0`). Analytical `area = amp * sigma * sqrt(2 * pi)` uses net `amp`, and
  `.deconvolve_subpeaks()` subtracts net `amp * exp(...)` tails. Never
  substitute `apex` for `amp`.
- **Deconvolution routing**: In `.peaks_combine()`, `deconvolve = TRUE` applies
  2-pass Gauss-Seidel tail subtraction only when a grouped peak splits into
  `length(segments) > 1L` segments. Unsplit singletons must route through
  `.peak_group_mzs()` and `.peak_group_intensities()`, honoring all
  `centroid_method` (`"weighted"`, `"gaussian"`, `"func"`) and
  `intensity_method` (`"func"`, `"gaussian_apex"`, `"gaussian_area"`,
  `"trapezoid"`) combinations.
- **Tail-subtracted trapezoid integral and clamping**: Under `deconvolve = TRUE`
  and `intensity_method = "trapezoid"`, integrate the tail-subtracted
  intensities `ints_corr`, clamped below at `0` (not `1e-6`).
- **Non-negative intensity weights**: `centroid_one_file()` requires
  `is.numeric(intensity_exponent)`, length 1, `!is.na()`, and `>= 0`.
  `.intensity_weights()` uses `pmax(0, ints)^exponent` so negative intensities
  never produce `NaN`.
- **1D Morse-Smale prominence splitting**: `.split_peak_group()` filters points
  against `min_datapoints` using non-zero intensities (`ints > 0`), finds local
  maxima with half-window `hws`, and accepts a split only when the minimum
  valley relative to flanking basin barriers satisfies
  `valley / barrier <= valley_ratio`.
- **Valid `min_datapoints_*` and `valley_hws`**: Both must be integerish scalars
  `>= 1L` (fractions such as `1.5` must raise an error, never truncate
  silently).

## Spectra, I/O, and XML (`R/centroid_one_file.R`)

- **No self-overwrite**: After verifying `file.exists(file)`,
  `centroid_one_file()` compares `normalizePath(outf, mustWork = FALSE)` with
  `normalizePath(file, mustWork = FALSE)`; if identical, log an error and return
  `FALSE` without touching the input file.
- **Existing output skip**: If `file.exists(outf)` (and `outf != file`), skip
  processing cleanly and return `TRUE`.
- **Metadata preservation**: Never drop spectrum metadata columns whose values
  equal `0` (for example `rtime = 0` or `polarity = 0`). When ensuring required
  downstream export columns (`basePeakIntensity`, `collisionEnergy`,
  `electronBeamEnergy`, `isolationWindowLowerOffset`,
  `isolationWindowUpperOffset`, plus `polarity = 0L` fallback), initialize
  missing numeric columns with `NA_real_`.
- **MS3+ and non-MS1/MS2 scan preservation**: `.process_spectra()` centroids MS1
  (`msLevel == 1L`) and MS2 (`msLevel == 2L`) separately with their own
  tolerances while preserving spectra of any other `msLevel` (MS3+ or `NA`)
  unmodified, retaining every scan's original chronological index
  (`spectra$.CentroidR_idx`) and original `centroided` value. Only centroided
  MS1/MS2 scans receive `centroided = TRUE` in `.process_spectra_batches()`.
- **Batch temp isolation & single-batch fast path**:
  `.process_spectra_batches()` writes per-batch temp files inside
  `tempfile("tmp_", tmpdir = outd)` with
  `on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)`. Single-batch
  (`n_batches == 1L`) exports write once directly to a sibling temp file of
  `outf` rather than re-reading and re-exporting.
- **XML escaping and streaming**: `.fix_xml()` escapes `&`, `<`, `>`, `"`, `'`
  via `.xml_escape_attr()` when inserting `basename(file_path)` into
  `<run id="...">`, streams in `65536`-line chunks rather than `readLines()` all
  at once, and swaps the file via `.replace_file()` checking `file.rename()`.

## Logging and Package Hooks

- **No startup side effects**: `R/CentroidR-package.R` defines `.onLoad()` only;
  do not re-add `.onAttach()`.
- **Logger restoration**: `.centroidr_log` lives in an internal `local()`
  environment with stdout default `.appender_stdout`. `centroid_one_file()`
  resets the logger on exit (`on.exit()`), *after* the `tryCatch()` error
  handler logs failures to `centroiding.log`. `.process_spectra_batches()`
  restores the logger conditionally only when called standalone with
  `.appender_stdout` active. Tests calling `centroid_one_file()` must call
  `centroidr_reset_logging()` from `helper-logging.R` in case of assertions
  inspecting logs or cleaning temp directories.

## CLI (`inst/scripts/centroiding.R`)

- **Testability guard**: Top-level execution is guarded by
  `if (sys.nframe() == 0L)` so `tinytest` can `source()` and unit-test
  `parse_cli_args()` and `run_cli()`.
- **Exit statuses and strict parsing**:
  - `0L`: `--help` or successful processing.
  - `1L`: missing file, missing/empty directory, or `centroid_one_file()`
    returning `FALSE`/erroring.
  - `2L`: invalid flags, missing flag value, mutually exclusive
    `--file`/`--directory` error, missing `--pattern`/`--replacement`, or
    malformed integer/numeric/boolean values.
- Reject non-integer strings for integer flags, `NA`/`Inf`/blank for numeric
  flags, and anything outside `TRUE`/`T`/`FALSE`/`F` (case-insensitive) for
  boolean flags.

## Tests and CI

- **Dual working-directory support**: All `inst/tinytest/test-*.R` files load
  `helper-logging.R` with `if (file.exists("helper-logging.R"))` else
  `"inst/tinytest/helper-logging.R"`, and `test-centroid-cli.R` resolves
  `inst/scripts/centroiding.R` across both working tree and `R CMD check`
  (`../scripts/centroiding.R` / `system.file(...)`).
- **No external dev/test packages**: Tests run under `R CMD check` with only
  `DESCRIPTION` packages installed (`tinytest` in `Suggests`).
- **CI gates**: `R-CMD-check.yaml` fails on `"warning"`; `test-coverage.yaml`
  fails on any `lintr` issue or `covr` coverage `< 95%`; `codecov.yml` sets
  `informational: false`.
