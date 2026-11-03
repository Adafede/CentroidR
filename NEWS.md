# CentroidR

## CentroidR 0.0.9004

- Drop external `logger` and `optparse` dependencies in favour of
  zero-dependency base R logging and CLI argument parsing.
- Vectorize peak-group local-maxima screening, intensity weighting, and metadata
  aggregation (`list2DF()`), skip empty MS levels in `.process_spectra()`, pass
  `BiocParallel::SerialParam()` consistently, and avoid redundant re-export on
  single-batch files.
- Add EDAM ontology annotations (`topic_3172`, `topic_3370`, `topic_0154`,
  `operation_3215`, `data_0943`, `format_3244`) across `DESCRIPTION`,
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

## CentroidR 0.0.9003

- Update package metadata for version 0.0.9003 and dependencies.

## CentroidR 0.0.9002

- Fix NaN values in mzML files originating from .wiff2 (#6).

--------------------------------------------------------------------------------

# CentroidR 0.0.9001

- Fixed NaN in mzMLs originating from .wiff2 (#6), see
  <https://github.com/sneumann/mzR/issues/307>

# CentroidR 0.0.9000

- Initial version.
