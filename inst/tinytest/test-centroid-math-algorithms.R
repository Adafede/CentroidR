## Tests for the mathematical algorithms in CentroidR:
## 1. 1D Morse-Smale topological prominence (watershed basin) peak splitting
##    and local baseline subtraction (.split_peak_group)
## 2. Apex-centered weighted log-quadratic (Guo-Caruana) Gaussian centroiding
##    and interpolated apex / area estimation (.fit_gaussian_peak)
## 3. Numerical trapezoidal integration (.trapezoid_area)
## 4. Gauss-Seidel Gaussian mixture tail deconvolution (.deconvolve_subpeaks)
## 5. Exact mass-analyzer local Jacobian domain grouping (.group_mz_domain)
## 6. Chronological MS1/MS2 scan order preservation (.process_spectra)

library(tinytest)

source("helper-logging.R")

.split_peak_group <- getFromNamespace(".split_peak_group", "CentroidR")
.fit_gaussian_peak <- getFromNamespace(".fit_gaussian_peak", "CentroidR")
.trapezoid_area <- getFromNamespace(".trapezoid_area", "CentroidR")
.deconvolve_subpeaks <- getFromNamespace(".deconvolve_subpeaks", "CentroidR")
.group_mz_domain <- getFromNamespace(".group_mz_domain", "CentroidR")
.peaks_combine <- getFromNamespace(".peaks_combine", "CentroidR")
.process_spectra <- getFromNamespace(".process_spectra", "CentroidR")

## ---------------------------------------------------------------------------
## 1. 1D Morse-Smale topological prominence & baseline subtraction
## ---------------------------------------------------------------------------

## Two major peaks (intensity 1000 at indices 3 and 13) separated by a deep
## valley (intensity 10 at indices 6 and 10) with a small noise ripple
## (intensity 15 at index 8) inside the valley.
## Adjacent-maxima logic compares (1000, 15) with valley 10 -> 10/15 = 0.67 > 0.2
## and fails to split. Morse-Smale basin prominence pairs the valley with the
## major peaks (10/1000 = 0.01 <= 0.2) and splits cleanly into two peaks.
ripple_doublet <- c(
  10,
  500,
  1000,
  500,
  50,
  10,
  12,
  15,
  12,
  10,
  50,
  500,
  1000,
  500,
  10
)
ripple_segs <- .split_peak_group(
  seq_along(ripple_doublet),
  ripple_doublet,
  valley_ratio = 0.2,
  hws = 2L
)
expect_equal(length(ripple_segs), 2L)
expect_true(3L %in% ripple_segs[[1]])
expect_true(13L %in% ripple_segs[[2]])

## A shallow noise bump on the flank of Peak 1 (m1=1000, v1=150, m2=160)
## followed by a deep valley (v2=10) before Peak 2 (m3=1000):
## v1 is bounded on the right by the deeper valley v2=10, so its right basin
## maximum is 160 (score 150/160 = 0.9375 > 0.2) and the flank bump is NOT
## falsely split, while the true deep valley v2 splits (10/1000 = 0.01 <= 0.2).
flank_bump <- c(100, 1000, 400, 150, 155, 160, 155, 80, 10, 80, 500, 1000, 100)
flank_segs <- .split_peak_group(
  seq_along(flank_bump),
  flank_bump,
  valley_ratio = 0.2,
  hws = 1L
)
expect_equal(length(flank_segs), 2L)
expect_equal(flank_segs[[1]], 1:9)
expect_equal(flank_segs[[2]], 10:13)

## Baseline subtraction enables splitting a doublet sitting on an elevated
## constant background floor (e.g. background = 200, peaks = 1200, valley = 300:
## unsubtracted ratio is 300/1200 = 0.25 > 0.2; baseline-subtracted ratio is
## (300-200)/(1200-200) = 0.10 <= 0.2).
elevated <- c(200, 700, 1200, 700, 300, 700, 1200, 700, 200)
expect_equal(
  length(.split_peak_group(
    seq_along(elevated),
    elevated,
    valley_ratio = 0.2,
    hws = 2L,
    baseline_subtract = FALSE
  )),
  1L
)
expect_equal(
  length(.split_peak_group(
    seq_along(elevated),
    elevated,
    valley_ratio = 0.2,
    hws = 2L,
    baseline_subtract = TRUE
  )),
  2L
)

## ---------------------------------------------------------------------------
## 2. Weighted Log-Quadratic (Guo-Caruana) Gaussian Centroid & Apex Estimator
## ---------------------------------------------------------------------------

## Exact recovery of continuous Gaussian center and apex across shifted sampling
## phases and asymmetric window truncation (eliminating pixel-locking and
## picket-fence attenuation).
true_mz <- 500.000000
true_amp <- 10000
sigma <- 0.00425
offsets <- (-1:3) + 0.37 # Asymmetric truncation + off-center phase shift
mz_pts <- true_mz + offsets * 0.0042
int_pts <- true_amp * exp(-0.5 * ((mz_pts - true_mz) / sigma)^2)

fit_g <- .fit_gaussian_peak(mz_pts, int_pts, exponent = 3)
expect_true(fit_g$valid)
expect_true(abs(fit_g$mz - true_mz) / true_mz * 1e6 < 1e-6)
expect_true(abs(fit_g$apex - true_amp) / true_amp < 1e-6)
expect_true(abs(fit_g$sigma - sigma) / sigma < 1e-6)
expect_true(
  abs(fit_g$area - (true_amp * sigma * sqrt(2 * pi))) / fit_g$area < 1e-6
)

## Compare against power-weighted mean and discrete max(), which suffer from
## asymmetric truncation bias and picket-fence underestimation on the same data.
mz_weighted_mean <- sum(mz_pts * int_pts^3) / sum(int_pts^3)
expect_true(abs(fit_g$mz - true_mz) < abs(mz_weighted_mean - true_mz) * 1e-4)
expect_true(max(int_pts) < 0.95 * true_amp)
expect_true(abs(fit_g$apex - true_amp) < 1e-4)

## Fallback on < 3 points or non-concave profiles
fit_2pt <- .fit_gaussian_peak(c(100, 100.002), c(10, 50), exponent = 3)
expect_false(fit_2pt$valid)
expect_equal(fit_2pt$mz, stats::weighted.mean(c(100, 100.002), c(10, 50)^3))
expect_equal(fit_2pt$apex, 50)

fit_convex <- .fit_gaussian_peak(
  c(100, 100.001, 100.002),
  c(50, 10, 50),
  exponent = 3
)
expect_false(fit_convex$valid)
expect_equal(fit_convex$mz, 100.001)

## ---------------------------------------------------------------------------
## 3. Trapezoidal Area Integration
## ---------------------------------------------------------------------------

mz_trap <- c(100.000, 100.002, 100.004)
int_trap <- c(10, 50, 20)
expected_area <- 0.5 * (10 + 50) * 0.002 + 0.5 * (50 + 20) * 0.002
expect_equal(.trapezoid_area(mz_trap, int_trap), expected_area)

## Baseline-subtracted trapezoidal area subtracts min(edge) = 10 -> c(0, 40, 10)
expected_area_bg <- 0.5 * (0 + 40) * 0.002 + 0.5 * (40 + 10) * 0.002
expect_equal(
  .trapezoid_area(mz_trap, int_trap, baseline_subtract = TRUE),
  expected_area_bg
)

## ---------------------------------------------------------------------------
## 4. Overlapping Multiplet Gaussian Tail Deconvolution
## ---------------------------------------------------------------------------

m1 <- 500.000000
m2 <- 500.017500
a1 <- 10000
a2 <- 2500
mz_dbl <- m1 + (seq(-4, 13) + 0.25) * 0.0022
int_dbl <- a1 *
  exp(-0.5 * ((mz_dbl - m1) / sigma)^2) +
  a2 * exp(-0.5 * ((mz_dbl - m2) / sigma)^2)

pk_hard <- .peaks_combine(
  cbind(mz = mz_dbl, intensity = int_dbl),
  tolerance = 0.05,
  ppm = 0,
  intensityFun = base::max,
  mzFun = base::mean,
  weighted = TRUE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  timeDomain = FALSE,
  centroid_method = "weighted",
  intensity_method = "func",
  deconvolve = FALSE,
  valley_ratio = 0.50,
  valley_hws = 1L
)

pk_deconv <- .peaks_combine(
  cbind(mz = mz_dbl, intensity = int_dbl),
  tolerance = 0.05,
  ppm = 0,
  intensityFun = base::max,
  mzFun = base::mean,
  weighted = TRUE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  timeDomain = FALSE,
  centroid_method = "gaussian",
  intensity_method = "gaussian_apex",
  deconvolve = TRUE,
  valley_ratio = 0.50,
  valley_hws = 1L
)

expect_equal(nrow(pk_hard), 2L)
expect_equal(nrow(pk_deconv), 2L)
## Deconvolved minor peak is much closer to true m2 and true apex a2 than hard split
err_hard_m2 <- abs(pk_hard[2, "mz"] - m2)
err_deconv_m2 <- abs(pk_deconv[2, "mz"] - m2)
expect_true(err_deconv_m2 < err_hard_m2 * 0.1)
expect_true(abs(pk_deconv[2, "intensity"] - a2) / a2 < 0.005)

## ---------------------------------------------------------------------------
## 5. Exact Mass-Analyzer Local Jacobian Domain Grouping
## ---------------------------------------------------------------------------

## A 7.5 ppm doublet at m/z 800 (0.006 Da apart) under ppm = 5, tolerance = 0:
## Legacy timeDomain = TRUE falsely merges it (due to 2x ppm distortion in sqrt space),
## whereas domain_scaling = "tof" or "linear" keeps the two peaks separate.
mz_800 <- c(800.000, 800.006)
expect_equal(
  length(unique(.group_mz_domain(
    mz_800,
    tolerance = 0,
    ppm = 5,
    timeDomain = TRUE,
    domain_scaling = "legacy"
  ))),
  1L
)
expect_equal(
  length(unique(.group_mz_domain(
    mz_800,
    tolerance = 0,
    ppm = 5,
    domain_scaling = "tof"
  ))),
  2L
)

## Local Jacobian TOF scaling is invariant to adding a low-m/z noise point at m/z 25,
## whereas legacy timeDomain = TRUE changes the high-mass window when min(mz) changes.
mz_high_pair <- c(400.000, 400.006) # 0.006 Da apart
## At m/z = 400 with mz_ref = 100 and tolerance = 0.0035 Da:
## TOF local threshold = 0.0035 * (400 / 100)^0.5 = 0.007 Da >= 0.006 Da -> merges (1 group)
grp_tof_alone <- .group_mz_domain(
  mz_high_pair,
  tolerance = 0.0035,
  ppm = 0,
  domain_scaling = "tof",
  mz_ref = 100
)
grp_tof_with_low <- .group_mz_domain(
  c(25.0, mz_high_pair),
  tolerance = 0.0035,
  ppm = 0,
  domain_scaling = "tof",
  mz_ref = 100
)
expect_equal(length(unique(grp_tof_alone)), 1L)
expect_equal(grp_tof_with_low[2], grp_tof_with_low[3])

## Analyzer scaling exponent hierarchy at m/z = 400 (m / mz_ref = 4) with tolerance = 0.001 Da:
## linear (4^0 = 1 -> 0.001 Da) < tof (4^0.5 = 2 -> 0.002 Da) <
## orbitrap (4^1.5 = 8 -> 0.008 Da) < fticr (4^2 = 16 -> 0.016 Da)
mz_step_005 <- c(400.000, 400.005) # 0.005 Da gap
expect_equal(
  length(unique(.group_mz_domain(
    mz_step_005,
    tolerance = 0.001,
    ppm = 0,
    domain_scaling = "linear",
    mz_ref = 100
  ))),
  2L
)
expect_equal(
  length(unique(.group_mz_domain(
    mz_step_005,
    tolerance = 0.001,
    ppm = 0,
    domain_scaling = "tof",
    mz_ref = 100
  ))),
  2L
)
expect_equal(
  length(unique(.group_mz_domain(
    mz_step_005,
    tolerance = 0.001,
    ppm = 0,
    domain_scaling = "orbitrap",
    mz_ref = 100
  ))),
  1L
)
expect_equal(
  length(unique(.group_mz_domain(
    mz_step_005,
    tolerance = 0.001,
    ppm = 0,
    domain_scaling = "fticr",
    mz_ref = 100
  ))),
  1L
)

## ---------------------------------------------------------------------------
## 6. Chronological Scan Order Preservation in .process_spectra
## ---------------------------------------------------------------------------

interleaved_sp <- Spectra::Spectra(data.frame(
  msLevel = c(1L, 2L, 1L, 2L, 1L),
  polarity = 0L,
  rtime = c(1.0, 1.2, 2.0, 2.4, 3.0),
  mz = I(list(
    c(100, 100.0005),
    c(500, 500.0005),
    c(200, 200.0005),
    c(600, 600.0005),
    c(300, 300.0005)
  )),
  intensity = I(list(
    c(10, 50),
    c(40, 400),
    c(20, 200),
    c(60, 600),
    c(30, 300)
  ))
))

proc_sp <- .process_spectra(
  interleaved_sp,
  mz_tol_da_ms1 = 0.01,
  mz_tol_da_ms2 = 0.01,
  mz_tol_ppm_ms1 = 0,
  mz_tol_ppm_ms2 = 0,
  custom_int_fun_ms1 = base::max,
  custom_int_fun_ms2 = base::max,
  mz_fun_ms1 = base::mean,
  mz_fun_ms2 = base::mean,
  mz_weighted = TRUE,
  time_domain = FALSE
)

expect_equal(Spectra::rtime(proc_sp), c(1.0, 1.2, 2.0, 2.4, 3.0))
expect_equal(Spectra::msLevel(proc_sp), c(1L, 2L, 1L, 2L, 1L))

## ---------------------------------------------------------------------------
## 7. End-to-End centroid_one_file() with Advanced Mathematical Options
## ---------------------------------------------------------------------------

e2e_dir <- tempfile("math_e2e_")
dir.create(e2e_dir)
e2e_in <- file.path(e2e_dir, "profile_test.mzML")
Spectra::export(
  Spectra::Spectra(data.frame(
    msLevel = 1L,
    polarity = 0L,
    rtime = 1.0,
    mz = I(list(mz_pts)),
    intensity = I(list(int_pts))
  )),
  file = e2e_in,
  backend = Spectra::MsBackendMzR()
)
e2e_out <- file.path(e2e_dir, "centroided_test.mzML")

expect_true(
  CentroidR::centroid_one_file(
    file = e2e_in,
    pattern = "profile_",
    replacement = "centroided_",
    min_datapoints_ms1 = 3L,
    mz_tol_da_ms1 = 0.05,
    mz_tol_ppm_ms1 = 10,
    centroid_method = "gaussian",
    intensity_method = "gaussian_apex",
    deconvolve = TRUE,
    valley_ratio = 0.25,
    valley_hws = 2L,
    baseline_subtract = FALSE,
    domain_scaling = "tof",
    mz_ref = 100
  )
)
centroidr_reset_logging()

e2e_peaks <- Spectra::peaksData(
  Spectra::Spectra(e2e_out, backend = Spectra::MsBackendMzR())
)[[1]]
expect_equal(nrow(e2e_peaks), 1L)
expect_true(abs(e2e_peaks[1, "mz"] - true_mz) / true_mz * 1e6 < 1e-4)
expect_true(abs(e2e_peaks[1, "intensity"] - true_amp) / true_amp < 1e-4)

unlink(e2e_dir, recursive = TRUE)
