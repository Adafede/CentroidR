## Tests for .keep_empty and .process_spectra: the post-processing steps that
## restore spectra emptied during centroiding and separate MS1 from MS2.
##
## Centroiding can legitimately reduce a spectrum to zero peaks, e.g. when every
## point sits below the intensity filter. .keep_empty puts the original peaks
## back for exactly those spectra so the spectrum is not silently lost, while
## still discarding zero-intensity peaks.
##
## Note on scope: these tests use single-MS-level inputs for the position-based
## restore assertions. When MS1 and MS2 spectra are interleaved,
## concatenateSpectra() reorders them (all MS1 first, then all MS2) while
## .keep_empty indexes the original spectra by the reordered position, so the
## pairing is only valid when the two orderings coincide.

library(tinytest)

.keep_empty <- getFromNamespace(".keep_empty", "CentroidR")
.process_spectra <- getFromNamespace(".process_spectra", "CentroidR")

make_spectra <- function(mzs, intensities, ms_level = 1L) {
  Spectra::Spectra(data.frame(
    msLevel = rep(ms_level, length(mzs)),
    polarity = 0L,
    rtime = seq_along(mzs),
    mz = I(mzs),
    intensity = I(intensities)
  ))
}

empty_spectra <- function(n, ms_level = 1L) {
  make_spectra(
    rep(list(numeric(0)), n),
    rep(list(numeric(0)), n),
    ms_level
  )
}

## ---------------------------------------------------------------------------
## Spectra emptied by processing get their original peaks back
## ---------------------------------------------------------------------------

orig <- make_spectra(
  list(c(100, 100.001), c(200, 200.001)),
  list(c(1, 10), c(5, 20))
)
emptied <- empty_spectra(2L)

restored <- .keep_empty(orig, emptied)
expect_equal(length(restored), 2L)
expect_equal(nrow(Spectra::peaksData(restored)[[1]]), 2L)
expect_equal(as.numeric(Spectra::peaksData(restored)[[1]][, "mz"]), c(100, 100.001))
expect_equal(as.numeric(Spectra::peaksData(restored)[[1]][, "intensity"]), c(1, 10))
expect_equal(as.numeric(Spectra::peaksData(restored)[[2]][, "mz"]), c(200, 200.001))
expect_equal(as.numeric(Spectra::peaksData(restored)[[2]][, "intensity"]), c(5, 20))

## The restored peaks are the ORIGINAL ones, not reprocessed ones: a group
## whose points are 0.001 Da apart is restored unmerged.
expect_equal(nrow(Spectra::peaksData(restored)[[1]]), 2L)

## ---------------------------------------------------------------------------
## Zero-intensity peaks are dropped from the restored spectrum
## ---------------------------------------------------------------------------
## Restoring is meant to preserve real signal, not to reintroduce the
## zero-intensity points that centroiding discarded.

orig_zero <- make_spectra(
  list(c(100, 100.001, 300)),
  list(c(1, 10, 0))
)
restored_zero <- .keep_empty(orig_zero, empty_spectra(1L))
pk <- Spectra::peaksData(restored_zero)[[1]]
expect_equal(nrow(pk), 2L)
expect_equal(as.numeric(pk[, "mz"]), c(100, 100.001))
expect_true(all(pk[, "intensity"] > 0))

## The surviving peaks keep their exact original m/z values; no centroiding is
## applied to the restored data.
expect_false(any(abs(pk[, "mz"] - 300) < 1e-6))
expect_equal(as.numeric(pk[2, "mz"]), 100.001)

## ---------------------------------------------------------------------------
## Spectra that are not empty are left untouched
## ---------------------------------------------------------------------------

untouched <- .keep_empty(orig, orig)
for (i in 1:2) {
  expect_equal(nrow(Spectra::peaksData(untouched)[[i]]), 2L)
}
expect_equal(
  as.numeric(Spectra::peaksData(untouched)[[1]][, "mz"]),
  c(100, 100.001)
)
expect_equal(
  as.numeric(Spectra::peaksData(untouched)[[2]][, "mz"]),
  c(200, 200.001)
)

## Only the emptied spectra are restored; the others keep their processed peaks.
half <- make_spectra(
  list(c(100, 100.001), c(500, 500.001)),
  list(c(7, 70), c(9, 90))
)
mixed_in <- empty_spectra(1L)
mixed_in@backend@peaksData <- c(
  list(numeric(0)),
  list(data.frame(mz = 555, intensity = 999))
)
mixed_out <- .keep_empty(orig, half)
expect_equal(nrow(Spectra::peaksData(mixed_out)[[1]]), 2L)
expect_equal(nrow(Spectra::peaksData(mixed_out)[[2]]), 2L)
expect_equal(as.numeric(Spectra::peaksData(mixed_out)[[2]][, "mz"]), c(500, 500.001))

## ---------------------------------------------------------------------------
## .process_spectra: grouping is governed by the MS1 tolerance
## ---------------------------------------------------------------------------

max_fun <- function(intensities) {
  if (length(intensities)) max(intensities) else 0
}

process_ms1 <- function(ms, tol_da, tol_ppm = 0) {
  .process_spectra(
    ms,
    mz_tol_da_ms1 = tol_da, mz_tol_da_ms2 = tol_da,
    mz_tol_ppm_ms1 = tol_ppm, mz_tol_ppm_ms2 = tol_ppm,
    custom_int_fun_ms1 = max_fun, custom_int_fun_ms2 = max_fun,
    mz_fun_ms1 = base::mean, mz_fun_ms2 = base::mean,
    mz_weighted = TRUE, time_domain = FALSE
  )
}

ms1 <- make_spectra(
  list(c(100, 100.0005), c(200, 200.0005)),
  list(c(10, 50), c(5, 25))
)

## A 0.01 Da tolerance merges the pair and reports the apex intensity. The
## merged m/z is the intensity-weighted centroid with exponent 3, so it sits
## just below the unweighted midpoint, well inside 1e-5 of 100.0005.
merged <- process_ms1(ms1, tol_da = 0.01)
expect_equal(length(merged), 2L)
expect_equal(Spectra::msLevel(merged), c(1L, 1L))
expect_equal(nrow(Spectra::peaksData(merged)[[1]]), 1L)
expect_true(abs(Spectra::peaksData(merged)[[1]][, "mz"] - 100.0005) < 1e-5)
expect_equal(as.numeric(Spectra::peaksData(merged)[[1]][, "intensity"]), 50)
expect_true(abs(Spectra::peaksData(merged)[[2]][, "mz"] - 200.0005) < 1e-5)
expect_equal(as.numeric(Spectra::peaksData(merged)[[2]][, "intensity"]), 25)

## A tolerance below the 0.0005 Da spacing leaves both points as separate peaks.
unmerged <- process_ms1(ms1, tol_da = 0.0001)
expect_equal(nrow(Spectra::peaksData(unmerged)[[1]]), 2L)
expect_equal(as.numeric(Spectra::peaksData(unmerged)[[1]][, "mz"]), c(100, 100.0005))
expect_equal(as.numeric(Spectra::peaksData(unmerged)[[1]][, "intensity"]), c(10, 50))

## The same is reachable through the ppm tolerance, which is independent of Da.
ppm_merged <- process_ms1(ms1, tol_da = 0, tol_ppm = 50)
expect_equal(nrow(Spectra::peaksData(ppm_merged)[[1]]), 1L)
expect_true(abs(Spectra::peaksData(ppm_merged)[[1]][, "mz"] - 100.0005) < 1e-5)

ppm_split <- process_ms1(ms1, tol_da = 0, tol_ppm = 0.5)
expect_equal(nrow(Spectra::peaksData(ppm_split)[[1]]), 2L)

## ---------------------------------------------------------------------------
## .process_spectra restores a spectrum that centroiding emptied
## ---------------------------------------------------------------------------
## Both points sit far below the intensity filter, so processing removes them;
## the spectrum is then restored from the original data rather than lost.

sub_eps <- make_spectra(
  list(c(200, 200.0005)),
  list(c(1e-18, 2e-18))
)
restored_proc <- process_ms1(sub_eps, tol_da = 0.01)
expect_equal(length(restored_proc), 1L)
expect_equal(nrow(Spectra::peaksData(restored_proc)[[1]]), 2L)
expect_equal(
  as.numeric(Spectra::peaksData(restored_proc)[[1]][, "mz"]),
  c(200, 200.0005)
)

## A spectrum with real signal above the filter is centroided, not restored.
signal <- process_ms1(
  make_spectra(list(c(100, 100.0005)), list(c(10, 50))),
  tol_da = 0.01
)
expect_equal(nrow(Spectra::peaksData(signal)[[1]]), 1L)
expect_true(abs(Spectra::peaksData(signal)[[1]][, "mz"] - 100.0005) < 1e-5)

## ---------------------------------------------------------------------------
## .process_spectra keeps the MS levels apart
## ---------------------------------------------------------------------------
## Each MS level is processed only with its own tolerance and both survive the
## round trip. MS2-only input also avoids the reordering caveat above, so the
## output order matches the input order.

ms2 <- make_spectra(
  list(c(500, 500.0005), c(600, 600.0005)),
  list(c(40, 400), c(60, 600)),
  ms_level = 2L
)
out2 <- .process_spectra(
  ms2,
  mz_tol_da_ms1 = 0.01, mz_tol_da_ms2 = 0.01,
  mz_tol_ppm_ms1 = 0, mz_tol_ppm_ms2 = 0,
  custom_int_fun_ms1 = max_fun, custom_int_fun_ms2 = max_fun,
  mz_fun_ms1 = base::mean, mz_fun_ms2 = base::mean,
  mz_weighted = TRUE, time_domain = FALSE
)
expect_equal(length(out2), 2L)
expect_equal(Spectra::msLevel(out2), c(2L, 2L))
expect_equal(Spectra::rtime(out2), c(1, 2))
expect_equal(nrow(Spectra::peaksData(out2)[[1]]), 1L)
expect_true(abs(Spectra::peaksData(out2)[[1]][, "mz"] - 500.0005) < 1e-5)
expect_equal(as.numeric(Spectra::peaksData(out2)[[1]][, "intensity"]), 400)
expect_true(abs(Spectra::peaksData(out2)[[2]][, "mz"] - 600.0005) < 1e-5)
expect_equal(as.numeric(Spectra::peaksData(out2)[[2]][, "intensity"]), 600)

## An MS2-only input is not treated as MS1: the MS1 tolerance has no effect on
## it, and widening the MS1 tolerance must not change the MS2 result.
out2_wide_ms1 <- .process_spectra(
  ms2,
  mz_tol_da_ms1 = 1000, mz_tol_da_ms2 = 0.01,
  mz_tol_ppm_ms1 = 0, mz_tol_ppm_ms2 = 0,
  custom_int_fun_ms1 = max_fun, custom_int_fun_ms2 = max_fun,
  mz_fun_ms1 = base::mean, mz_fun_ms2 = base::mean,
  mz_weighted = TRUE, time_domain = FALSE
)
expect_equal(nrow(Spectra::peaksData(out2_wide_ms1)[[1]]), 1L)
expect_equal(
  as.numeric(Spectra::peaksData(out2_wide_ms1)[[1]][, "intensity"]),
  400
)

## Widening the MS2 tolerance does change the MS2 result, confirming the
## tolerances are routed per MS level rather than applied globally.
out2_wide_ms2 <- .process_spectra(
  make_spectra(
    list(c(500, 500.0005, 500.001)),
    list(c(10, 400, 20)),
    ms_level = 2L
  ),
  mz_tol_da_ms1 = 0.01, mz_tol_da_ms2 = 1000,
  mz_tol_ppm_ms1 = 0, mz_tol_ppm_ms2 = 0,
  custom_int_fun_ms1 = max_fun, custom_int_fun_ms2 = max_fun,
  mz_fun_ms1 = base::mean, mz_fun_ms2 = base::mean,
  mz_weighted = TRUE, time_domain = FALSE
)
expect_equal(length(out2_wide_ms2), 1L)
expect_equal(nrow(Spectra::peaksData(out2_wide_ms2)[[1]]), 1L)
expect_equal(as.numeric(Spectra::peaksData(out2_wide_ms2)[[1]][, "intensity"]), 400)
## ---------------------------------------------------------------------------
## Spectra are restored by identity, not by position
## ---------------------------------------------------------------------------
## .process_spectra splits the input into an MS1 part and an MS2 part and
## concatenates them MS1 first, so the processed order differs from the
## acquisition order whenever the MS levels interleave. An emptied spectrum must
## still get ITS OWN original peaks back, not those of whichever spectrum landed
## at the same position.

max_fun_mdp <- function(intensities) {
  if (length(intensities)) max(intensities) else 0
}

## Acquisition order: MS1 @1s, MS2 @1.5s, MS1 @2s holding only sub-eps noise,
## MS1 @3s, MS2 @3.5s. The rtime=2s spectrum is the one centroiding empties.
interleaved <- Spectra::Spectra(data.frame(
  msLevel = c(1L, 2L, 1L, 1L, 2L),
  polarity = c(0L, 0L, 0L, 0L, 0L),
  rtime = c(1, 1.5, 2, 3, 3.5),
  mz = I(list(
    c(100, 100.0005), c(500, 500.0005), c(200, 200.0005),
    c(300, 300.0005), c(600, 600.0005)
  )),
  intensity = I(list(
    c(10, 50), c(40, 400), c(1e-18, 2e-18), c(30, 300), c(60, 600)
  ))
))

inter_out <- .process_spectra(
  interleaved,
  mz_tol_da_ms1 = 0.01, mz_tol_da_ms2 = 0.01,
  mz_tol_ppm_ms1 = 0, mz_tol_ppm_ms2 = 0,
  custom_int_fun_ms1 = max_fun_mdp, custom_int_fun_ms2 = max_fun_mdp,
  mz_fun_ms1 = base::mean, mz_fun_ms2 = base::mean,
  mz_weighted = TRUE, time_domain = FALSE
)

## Nothing is lost: every input spectrum is present in the output.
expect_equal(length(inter_out), 5L)
expect_equal(sort(Spectra::rtime(inter_out)), c(1, 1.5, 2, 3, 3.5))
expect_equal(sum(Spectra::msLevel(inter_out) == 1L), 3L)
expect_equal(sum(Spectra::msLevel(inter_out) == 2L), 2L)

## Each output spectrum keeps the peaks of its OWN input spectrum. Match on
## rtime, which is carried through the pipeline, so the assertion is independent
## of the order the spectra come back in.
peaks_at <- function(sp, rt) {
  Spectra::peaksData(sp)[[which(Spectra::rtime(sp) == rt)]]
}
expect_true(abs(peaks_at(inter_out, 1)[1, "mz"] - 100.0005) < 1e-5)
expect_equal(as.numeric(peaks_at(inter_out, 1.5)[, "mz"]), 500.0005)
expect_equal(as.numeric(peaks_at(inter_out, 3)[, "mz"]), 300.0005)
expect_equal(as.numeric(peaks_at(inter_out, 3.5)[, "mz"]), 600.0005)

## The rtime=2s spectrum was emptied because all of its peaks sat below the
## intensity filter, so it is restored from the original data. Crucially it gets
## ITS OWN peaks at m/z 200, not the MS2 peaks at m/z 500 that sit at the same
## position once the output is reordered MS1-first.
rt2 <- peaks_at(inter_out, 2)
expect_equal(nrow(rt2), 2L)
expect_equal(as.numeric(rt2[, "mz"]), c(200, 200.0005))
expect_equal(as.numeric(rt2[, "intensity"]), c(1e-18, 2e-18))
expect_false(any(abs(rt2[, "mz"] - 500) < 1))

## The MS2 peaks appear exactly once in the whole output, on their own spectrum.
all_mz <- sort(unlist(lapply(Spectra::peaksData(inter_out), function(p) p[, "mz"])))
expect_equal(sum(abs(all_mz - 500.0005) < 1e-6), 1L)

## ---------------------------------------------------------------------------
## order_map must describe the processed result
## ---------------------------------------------------------------------------
## .process_spectra() supplies the map that ties each processed spectrum back to
## its input. A map of the wrong length would silently restore the wrong peaks,
## so the length is validated before anything is used.

## The message is asserted as well, because a wrong length map fails either way:
## without the guard it reaches the subsetting and dies on an unrelated
## "subscript out of bounds" instead.
expect_error(
  .keep_empty(orig, empty_spectra(2L), order_map = 1L),
  pattern = "order_map",
  info = "a short order_map must be rejected"
)
expect_error(
  .keep_empty(orig, empty_spectra(2L), order_map = c(1L, 2L, 3L)),
  pattern = "order_map",
  info = "a long order_map must be rejected"
)
expect_error(
  .keep_empty(orig, empty_spectra(2L), order_map = integer(0)),
  pattern = "order_map"
)


## The check only fires when a spectrum actually has to be restored; a consistent
## map is accepted.
expect_equal(
  length(.keep_empty(orig, empty_spectra(2L), order_map = c(1L, 2L))),
  2L
)

## order_map also reorders: passing a reversed map swaps which original peaks
## are used, which is exactly the mapping the pipeline relies on.
reversed <- .keep_empty(orig, empty_spectra(2L), order_map = c(2L, 1L))
expect_equal(as.numeric(Spectra::peaksData(reversed)[[1]][, "mz"]), c(200, 200.001))
expect_equal(as.numeric(Spectra::peaksData(reversed)[[2]][, "mz"]), c(100, 100.001))

## ---------------------------------------------------------------------------
## An original spectrum that is itself empty stays empty
## ---------------------------------------------------------------------------
## Restoring an empty spectrum must not invent peaks for it.

orig_empty_one <- make_spectra(
  list(c(100, 100.001), numeric(0)),
  list(c(1, 10), numeric(0))
)
restored_empty <- .keep_empty(orig_empty_one, empty_spectra(2L))
expect_equal(nrow(Spectra::peaksData(restored_empty)[[1]]), 2L)
expect_equal(nrow(Spectra::peaksData(restored_empty)[[2]]), 0L)
