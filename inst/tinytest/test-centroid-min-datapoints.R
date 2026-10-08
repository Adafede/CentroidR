## Tests for the min_datapoints_* thresholds in centroid_one_file().
##
## A peak group is only centroided when it contains at least
## `min_datapoints_ms*` points with a non-zero intensity. Groups below the
## threshold get an intensity of 0, are removed by the intensity filter, and the
## affected spectrum is then restored from the original data by .keep_empty.
##
## This exercises the full pipeline end to end, so it also covers the batch
## loop, the MS level routing and the mzML export.

library(tinytest)

source("helper-logging.R")

write_profile_mzml <- function(mzs, intensities, tag) {
  infile <- tempfile(pattern = paste0("profile_", tag, "_"), fileext = ".mzML")
  Spectra::export(
    Spectra::Spectra(data.frame(
      msLevel = rep(1L, length(mzs)),
      polarity = 0L,
      rtime = seq_along(mzs),
      mz = I(mzs),
      intensity = I(intensities)
    )),
    file = infile,
    backend = Spectra::MsBackendMzR()
  )
  infile
}

read_peaks <- function(file) {
  Spectra::peaksData(Spectra::Spectra(file, backend = Spectra::MsBackendMzR()))
}

centroid <- function(mzs, intensities, tag, ...) {
  infile <- write_profile_mzml(mzs, intensities, tag)
  outfile <- sub("profile_", "centroided_", infile, fixed = TRUE)
  on.exit(unlink(c(infile, outfile)), add = TRUE)
  expect_true(
    CentroidR::centroid_one_file(
      file = infile,
      pattern = "profile_",
      replacement = "centroided_",
      ...
    ),
    info = "centroiding should succeed"
  )
  pk <- read_peaks(outfile)
  centroidr_reset_logging()
  pk
}

## ---------------------------------------------------------------------------
## Below the threshold the group is not centroided and the original points return
## ---------------------------------------------------------------------------
## Spectrum 1 holds a 3-point peak group, spectrum 2 a 5-point one. Both pairs
## are 0.0005 Da apart, well inside any default tolerance, so the only thing
## that separates them is the min_datapoints threshold.

mzs <- list(
  c(100, 100.0005, 100.001),
  c(200, 200.0005, 200.001, 200.0015, 200.002)
)
ints <- list(
  c(10, 50, 20),
  c(10, 50, 20, 30, 40)
)

## The default MS1 threshold of 5: the 3-point group falls short, so it is left
## uncentroided and its original three points come back via .keep_empty, while
## the 5-point group is centroided into a single peak.
below <- centroid(mzs, ints, tag = "below", min_datapoints_ms1 = 5L, min_datapoints_ms2 = 5L)
expect_equal(length(below), 2L)

expect_equal(nrow(below[[1]]), 3L)
expect_equal(as.numeric(below[[1]][, "mz"]), c(100, 100.0005, 100.001))
expect_equal(as.numeric(below[[1]][, "intensity"]), c(10, 50, 20))

expect_equal(nrow(below[[2]]), 1L)
expect_true(abs(below[[2]][1, "mz"] - 200.0011) < 1e-3)
expect_equal(as.numeric(below[[2]][1, "intensity"]), 50)

## Lowering the threshold to 3 lets the short group through, so BOTH spectra are
## centroided to a single peak with the apex intensity.
at <- centroid(mzs, ints, tag = "at", min_datapoints_ms1 = 3L, min_datapoints_ms2 = 3L)
expect_equal(length(at), 2L)
expect_equal(nrow(at[[1]]), 1L)
expect_true(abs(at[[1]][1, "mz"] - 100.0005) < 1e-3)
expect_equal(as.numeric(at[[1]][1, "intensity"]), 50)
expect_equal(nrow(at[[2]]), 1L)
expect_true(abs(at[[2]][1, "mz"] - 200.0011) < 1e-3)
expect_equal(as.numeric(at[[2]][1, "intensity"]), 50)

## A threshold of 1 centroids everything.
loose <- centroid(mzs, ints, tag = "loose", min_datapoints_ms1 = 1L, min_datapoints_ms2 = 1L)
expect_equal(nrow(loose[[1]]), 1L)
expect_equal(nrow(loose[[2]]), 1L)

## A threshold above every group leaves both spectra uncentroided.
tight <- centroid(mzs, ints, tag = "tight", min_datapoints_ms1 = 99L, min_datapoints_ms2 = 99L)
expect_equal(nrow(tight[[1]]), 3L)
expect_equal(nrow(tight[[2]]), 5L)

## The threshold therefore decides whether a peak group is reported as one
## centroided peak or as its original unresolved points.
expect_equal(nrow(at[[1]]), 1L)
expect_equal(nrow(below[[1]]), 3L)
expect_true(nrow(at[[1]]) < nrow(below[[1]]))