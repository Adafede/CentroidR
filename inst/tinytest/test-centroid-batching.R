## Tests for .process_spectra_batches: the batching loop that splits a large
## Spectra object into chunks, centroides each chunk separately, and joins the
## exported chunks back together.
##
## The internal batch_size argument defaults to 4096, which is far too large to
## exercise the chunk boundaries in a test, so these tests call the internal
## function directly with a small batch size. Every chunk boundary is then a real
## boundary, which is where an off-by-one in the chunk index would show up as a
## duplicated or dropped spectrum.

library(tinytest)

.process_spectra_batches <- getFromNamespace(
  ".process_spectra_batches",
  "CentroidR"
)

## setup_logger() installs a file appender pointing into the output directory
## and never removes it, so once that directory is deleted any later logging
## would fail with "cannot open the connection". Reset the logger to the console
## right after each run, while the output directory still exists.
source("helper-logging.R")

make_spectra <- function(n) {
  Spectra::Spectra(data.frame(
    msLevel = 1L,
    polarity = 0L,
    rtime = seq_len(n),
    mz = I(lapply(seq_len(n), function(i) c(100 * i, 100 * i + 0.0005))),
    intensity = I(lapply(seq_len(n), function(i) c(10, 50)))
  ))
}

run_batches <- function(spectra, batch_size, tag) {
  outd <- tempfile(paste0("batch_", tag, "_"))
  dir.create(outd)
  outf <- file.path(outd, "centroided.mzML")
  ## Read the result inside the helper: the temporary directory is removed on
  ## exit, so it must not still be needed once this function returns.
  on.exit(unlink(outd, recursive = TRUE), add = TRUE)
  expect_true(
    .process_spectra_batches(
      spectra = spectra,
      outf = outf,
      outd = outd,
      min_datapoints_ms1 = 1L,
      min_datapoints_ms2 = 1L,
      mz_tol_da_ms1 = 0.01,
      mz_tol_da_ms2 = 0.01,
      mz_tol_ppm_ms1 = 0,
      mz_tol_ppm_ms2 = 0,
      mz_fun_ms1 = base::mean,
      mz_fun_ms2 = base::mean,
      int_fun_ms1 = base::max,
      int_fun_ms2 = base::max,
      mz_weighted = TRUE,
      time_domain = FALSE,
      intensity_exponent = 3,
      batch_size = batch_size
    ),
    info = "batched processing should succeed"
  )
  expect_true(file.exists(outf))
  ## Force the peak data now: MsBackendMzR reads lazily, so anything not
  ## materialised here would fail once the temporary directory is removed.
  centroidr_reset_logging()
  out <- Spectra::Spectra(outf, backend = Spectra::MsBackendMzR())
  peaks <- Spectra::peaksData(out)
  list(
    rtime = Spectra::rtime(out),
    msLevel = Spectra::msLevel(out),
    centroided = Spectra::spectraData(out)$centroided,
    peaks = peaks,
    tmp_removed = !dir.exists(file.path(outd, "tmp"))
  )
}

## ---------------------------------------------------------------------------
## Every spectrum survives the batching, exactly once
## ---------------------------------------------------------------------------

## 5 spectra with batch_size 2 gives chunk boundaries after spectra 2 and 4,
## with a final short chunk holding spectrum 5.
n <- 5
res <- run_batches(make_spectra(n), batch_size = 2L, tag = "b2")
sp <- res$peaks

expect_equal(length(sp), n)
expect_equal(res$rtime, as.numeric(seq_len(n)))
expect_equal(res$msLevel, rep(1L, n))

## Each spectrum's two 0.0005 Da apart points are centroided into one peak at
## the expected apex intensity, so no spectrum was processed twice (which would
## show as a duplicate) or skipped.
for (i in seq_len(n)) {
  pk <- sp[[i]]
  expect_equal(nrow(pk), 1L, info = paste("spectrum", i, "should be one peak"))
  expect_true(abs(pk[1, "mz"] - (100 * i + 0.0005)) < 1e-5)
  expect_equal(as.numeric(pk[1, "intensity"]), 50)
}

## A batch size larger than the input still processes everything.
single <- run_batches(make_spectra(3), batch_size = 100L, tag = "big")
expect_equal(length(single$peaks), 3L)
expect_equal(single$rtime, c(1, 2, 3))

## A batch size of 1 puts a boundary between every pair of spectra.
ones <- run_batches(make_spectra(4), batch_size = 1L, tag = "b1")
expect_equal(length(ones$peaks), 4L)
expect_equal(ones$rtime, c(1, 2, 3, 4))
for (i in 1:4) {
  expect_equal(nrow(ones$peaks[[i]]), 1L)
}

## A batch size that divides the input exactly has no ragged final chunk.
exact <- run_batches(make_spectra(4), batch_size = 2L, tag = "exact")
expect_equal(length(exact$peaks), 4L)
expect_equal(exact$rtime, c(1, 2, 3, 4))

## The result does not depend on how the input was chunked.
for (i in seq_len(3)) {
  expect_equal(
    as.numeric(exact$peaks[[i]][, "mz"]),
    as.numeric(ones$peaks[[i]][, "mz"])
  )
}

## ---------------------------------------------------------------------------
## Bookkeeping around the temporary chunk directory
## ---------------------------------------------------------------------------

## Processed spectra are flagged as centroided so downstream consumers can tell.
expect_true(all(res$centroided))

## The per-chunk temporary directory is cleaned up on exit.
## No top level on.exit() here: in tinytest's evaluation context it can fire
## before the assertions below run, which would make them vacuous.
outd <- tempfile("batch_clean_")
dir.create(outd)
invisible(.process_spectra_batches(
  spectra = make_spectra(3),
  outf = file.path(outd, "out.mzML"),
  outd = outd,
  min_datapoints_ms1 = 1L,
  min_datapoints_ms2 = 1L,
  mz_tol_da_ms1 = 0.01,
  mz_tol_da_ms2 = 0.01,
  mz_tol_ppm_ms1 = 0,
  mz_tol_ppm_ms2 = 0,
  mz_fun_ms1 = base::mean,
  mz_fun_ms2 = base::mean,
  int_fun_ms1 = base::max,
  int_fun_ms2 = base::max,
  mz_weighted = TRUE,
  time_domain = FALSE,
  intensity_exponent = 3,
  batch_size = 2L
))
centroidr_reset_logging()
expect_true(file.exists(file.path(outd, "out.mzML")))
expect_false(dir.exists(file.path(outd, "tmp")))
unlink(outd, recursive = TRUE)

## ---------------------------------------------------------------------------
## MS levels are kept apart across chunk boundaries
## ---------------------------------------------------------------------------

## Interleaved MS1/MS2 input, chunked so the boundaries fall between different
## MS levels. Both levels must be centroided and none lost.
mixed <- Spectra::Spectra(data.frame(
  msLevel = c(2L, 1L, 2L, 1L, 1L, 2L),
  polarity = 0L,
  rtime = c(1, 1.5, 2, 3, 3.5, 4),
  mz = I(list(
    c(500, 500.0005),
    c(100, 100.0005),
    c(600, 600.0005),
    c(200, 200.0005),
    c(300, 300.0005),
    c(700, 700.0005)
  )),
  intensity = I(list(
    c(40, 400),
    c(10, 50),
    c(60, 600),
    c(20, 200),
    c(30, 300),
    c(70, 700)
  ))
))
mixres <- run_batches(mixed, batch_size = 2L, tag = "mix")
mixed_out <- list(
  rtime = mixres$rtime,
  msLevel = mixres$msLevel,
  peaks = mixres$peaks
)

expect_equal(length(mixed_out$peaks), 6L)
expect_equal(sum(mixed_out$msLevel == 1L), 3L)
expect_equal(sum(mixed_out$msLevel == 2L), 3L)
expect_equal(sort(mixed_out$rtime), c(1, 1.5, 2, 3, 3.5, 4))

## Each spectrum keeps its own peaks: match on rtime so the assertion does not
## depend on the MS1-first output ordering.
peaks_at <- function(s, rt) {
  s$peaks[[which(s$rtime == rt)]]
}
expect_true(abs(peaks_at(mixed_out, 1)[1, "mz"] - 500.0005) < 1e-5)
expect_true(abs(peaks_at(mixed_out, 1.5)[1, "mz"] - 100.0005) < 1e-5)
expect_true(abs(peaks_at(mixed_out, 2)[1, "mz"] - 600.0005) < 1e-5)
expect_true(abs(peaks_at(mixed_out, 3)[1, "mz"] - 200.0005) < 1e-5)
expect_true(abs(peaks_at(mixed_out, 3.5)[1, "mz"] - 300.0005) < 1e-5)
expect_true(abs(peaks_at(mixed_out, 4)[1, "mz"] - 700.0005) < 1e-5)
