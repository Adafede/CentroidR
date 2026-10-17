# CentroidR

## CentroidR 0.0.9004

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
- `min_datapoints_ms1` and `min_datapoints_ms2` must now be at least 1. Zero and
  negative thresholds used to be accepted and centroided peak groups that held
  no data at all.

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
