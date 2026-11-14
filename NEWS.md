# CentroidR

## CentroidR 0.0.0.9005

- Replace immediate-neighbor valley scoring in `.split_peak_group()` with a 1D
  Morse-Smale topological prominence (watershed basin) saddle ratio so small
  noise ripples inside a valley no longer prevent splitting two well-separated
  major peaks.
- Add `centroid_method = c("weighted", "gaussian", "func")` implementing an
  apex-centered weighted log-quadratic (Guo-Caruana) Gaussian estimator that
  eliminates sampling-phase (pixel-locking) and asymmetric truncation $m/z$
  bias, with automatic fallback on $< 3$ points or non-concave profiles.
- Add
  `intensity_method = c("func", "gaussian_apex", "gaussian_area", "trapezoid")`
  to support continuous log-quadratic apex interpolation (eliminating
  picket-fence intensity attenuation), analytical Gaussian peak area
  ($A\sigma\sqrt{2\pi}$), and numerical trapezoidal integration.
- Add `deconvolve = TRUE` to perform 2-pass Gauss-Seidel Gaussian tail
  subtraction across split multiplet sub-peaks before estimating centroids and
  abundances.
- Add `domain_scaling = c("legacy", "linear", "tof", "orbitrap", "fticr")` and
  `mz_ref` for exact local Jacobian mass-analyzer spacing laws
  ($\Delta m \propto m^\alpha$ with $\alpha \in \{0, 0.5, 1.5, 2.0\}$) directly
  in $m/z$ space without $2 \times$ ppm distortion or sensitivity to `min(mz)`.
- Add `valley_ratio`, `valley_hws`, and `baseline_subtract` parameters to
  `centroid_one_file()` and the CLI (`inst/scripts/centroiding.R`) for local
  edge-baseline subtraction and fine-grained valley splitting control.
- Fix `.fit_gaussian_peak()` and `.deconvolve_subpeaks()` so net Gaussian
  amplitude and continuous apex above baseline remain distinct, preventing
  baseline-inflated Gaussian areas and deconvolved tail subtraction.
- Route unsplit peak groups cleanly and support all
  `centroid_method`/`intensity_method` combinations under `deconvolve = TRUE`;
  enforce `intensity_exponent >= 0` and clamp negative intensities in weight and
  trapezoidal calculations.
- Refuse in-place self-overwrite when `pattern`/`replacement` resolves `outf` to
  the input `file` (returning `FALSE`), preserve valid zero-valued spectrum
  metadata (such as `rtime = 0`), preserve MS3+ and missing-level scans
  unmodified in original acquisition order, isolate batch temporary files per
  run, skip redundant single-batch re-export, and escape XML attribute values in
  `.fix_xml()`.
- Remove package startup banner (`.onAttach()`).
- Enforce non-zero CLI exit codes (`1` on runtime/input failure, `2` on invalid
  options or usage) and strict option parsing in `inst/scripts/centroiding.R`.

## CentroidR 0.0.0.9004

- Drop external `logger` and `optparse` dependencies in favour of
  zero-dependency base R logging and CLI argument parsing.
- Vectorize peak-group local-maxima screening, intensity weighting, and metadata
  aggregation (`list2DF()`), skip empty MS levels in `.process_spectra()`, pass
  `BiocParallel::SerialParam()` consistently, and avoid redundant re-export on
  single-batch files.
- Add EDAM ontology annotations (`topic_3172`, `topic_0153`, `topic_0154`,
  `topic_3370`, `operation_3215`, `operation_3214`, `operation_3695`,
  `data_0943`, `data_2536`, `format_3244`) across `DESCRIPTION`,
  `codemeta.json`, `CITATION.cff`, documentation, and container metadata.
- Fix centroid m/z/intensity alignment bug.
- Add regression tests for the reported m/z 164.0528 case.
- Reduce centroid allocation overhead for faster, leaner processing.
- Split dense profile peaks at local valleys so close peaks centroid separately.
  (#52)
- `pattern` in `centroid_one_file()` is now honoured as the regular expression
  the documentation describes, instead of being matched literally.
- Fix the command line script, which recursed infinitely on every run because
  its null-coalescing operator called itself.
- The output file is now published with an atomic rename, so a run that fails
  part way through no longer leaves a truncated file that later runs would
  mistake for finished work.
- `min_datapoints_ms1` and `min_datapoints_ms2` must now be whole numbers of at
  least 1. Zero and negative thresholds used to be accepted and centroided peak
  groups that held no data at all, and a fractional threshold such as 1.5 can
  never be reached.
- `centroid_one_file()` now returns the logger to the console when it finishes.
  A finished run no longer keeps capturing messages that other code logs later,
  and a log file whose directory has since been deleted no longer turns every
  subsequent logging call in the session into an error.

## CentroidR 0.0.0.9003

- Update package metadata for version 0.0.0.9003 and dependencies.

## CentroidR 0.0.0.9002

- Fix NaN values in mzML files originating from .wiff2 (#6).

--------------------------------------------------------------------------------

## CentroidR 0.0.0.9001

- Fixed NaN in mzMLs originating from .wiff2 (#6), see
  <https://github.com/sneumann/mzR/issues/307>

## CentroidR 0.0.0.9000

- Initial version.
