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

source(
  if (file.exists("helper-logging.R")) {
    "helper-logging.R"
  } else {
    "inst/tinytest/helper-logging.R"
  }
)

.split_peak_group <- getFromNamespace(".split_peak_group", "CentroidR")
.fit_gaussian_peak <- getFromNamespace(".fit_gaussian_peak", "CentroidR")
.trapezoid_area <- getFromNamespace(".trapezoid_area", "CentroidR")
.deconvolve_subpeaks <- getFromNamespace(".deconvolve_subpeaks", "CentroidR")
.group_mz_domain <- getFromNamespace(".group_mz_domain", "CentroidR")
.peak_group_mzs <- getFromNamespace(".peak_group_mzs", "CentroidR")
.peak_group_intensities <- getFromNamespace(
  ".peak_group_intensities",
  "CentroidR"
)
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

## ---------------------------------------------------------------------------
## 8. Edge Cases, Branch Coverage, and Mutation-Killing Boundary Tests
## ---------------------------------------------------------------------------

## 8a. .split_peak_group boundary & mutation tests
## 3-point group with 2 endpoint maxima and an interior valley splits when length(idx) == 3L
expect_equal(
  .split_peak_group(1:3, c(100, 5, 100), valley_ratio = 0.2, hws = 1L),
  list(1:2, 3L)
)

## Asymmetric ripple valleys (v1 < v2 and v1 > v2) inside a deep basin to test
## both k_left and k_right basin boundary loops strictly
ripple_asym_left <- c(10, 1000, 50, 10, 15, 12, 50, 1000, 10) # v1=10 < v2=12
expect_equal(
  length(.split_peak_group(
    seq_along(ripple_asym_left),
    ripple_asym_left,
    valley_ratio = 0.2,
    hws = 1L
  )),
  2L
)
ripple_asym_right <- c(10, 1000, 50, 12, 15, 10, 50, 1000, 10) # v1=12 > v2=10
expect_equal(
  length(.split_peak_group(
    seq_along(ripple_asym_right),
    ripple_asym_right,
    valley_ratio = 0.2,
    hws = 1L
  )),
  2L
)

## Exact boundary: valley / peak_floor == valley_ratio splits (<=), > does not
boundary_doublet <- c(10, 100, 20, 100, 10)
expect_equal(
  length(.split_peak_group(
    seq_along(boundary_doublet),
    boundary_doublet,
    valley_ratio = 0.2,
    hws = 1L
  )),
  2L
)
expect_equal(
  length(.split_peak_group(
    seq_along(boundary_doublet),
    boundary_doublet,
    valley_ratio = 0.1999,
    hws = 1L
  )),
  1L
)

## All-zero / non-positive maxima (peak_floor <= 0) do not split
zero_with_neg_valley <- c(-5, 0, -10, 0, -5)
expect_equal(
  .split_peak_group(
    seq_along(zero_with_neg_valley),
    zero_with_neg_valley,
    valley_ratio = 0.5,
    hws = 1L
  ),
  list(seq_along(zero_with_neg_valley))
)

## 8b. .fit_gaussian_peak edge cases & mutation tests
## Empty input (n == 0L)
fit_empty <- .fit_gaussian_peak(numeric(0), numeric(0))
expect_false(fit_empty$valid)
expect_true(is.na(fit_empty$mz))
expect_equal(fit_empty$apex, 0)
expect_equal(fit_empty$sigma, 0)
expect_equal(fit_empty$area, 0)

## Single point (n == 1L -> sigma == 0)
fit_1pt <- .fit_gaussian_peak(100.5, 42)
expect_false(fit_1pt$valid)
expect_equal(fit_1pt$mz, 100.5)
expect_equal(fit_1pt$apex, 42)
expect_equal(fit_1pt$sigma, 0)
expect_equal(fit_1pt$area, 42)

## Sharp peak where flanks are < 5% of apex (1 / 1000 = 0.001 < 0.05),
## forcing the length(core) < 3L neighborhood expansion (left_i:right_i)
mz_sharp <- c(200.000, 200.002, 200.004)
int_sharp <- c(1, 1000, 1)
fit_sharp <- .fit_gaussian_peak(mz_sharp, int_sharp, exponent = 2)
expect_true(fit_sharp$valid)
expect_equal(fit_sharp$mz, 200.002)
expect_equal(fit_sharp$apex, 1000)

## Baseline subtraction inside .fit_gaussian_peak restores baseline floor to apex
bg_level <- 150
int_pts_bg <- int_pts + bg_level
fit_g_bg <- .fit_gaussian_peak(
  mz_pts,
  int_pts_bg,
  exponent = 3,
  baseline_subtract = TRUE
)
expect_true(fit_g_bg$valid)
expect_true(abs(fit_g_bg$mz - fit_g$mz) < 1e-4)
expect_true(abs(fit_g_bg$apex - (true_amp + bg_level)) < 50)

## All-zero intensities in .fit_gaussian_peak (w_sum == 0 fallback to mzFun)
fit_zeros <- .fit_gaussian_peak(
  c(100, 101, 102),
  c(0, 0, 0),
  mzFun = stats::median
)
expect_false(fit_zeros$valid)
expect_equal(fit_zeros$mz, 101)
expect_equal(fit_zeros$apex, 0)

## Monotonic 3-point profile where concave vertex falls outside [min(m_c), max(m_c)]
fit_mono <- .fit_gaussian_peak(
  c(100.000, 100.002, 100.004),
  c(100, 72, 50),
  exponent = 1
)
expect_false(fit_mono$valid)

## 8c. .trapezoid_area edge cases & mutation tests
expect_equal(.trapezoid_area(numeric(0), numeric(0)), 0)
expect_equal(.trapezoid_area(100, 35), 35)
expect_equal(.trapezoid_area(100, -10), 0)

## Unsorted m/z input produces identical area to sorted input
expect_equal(
  .trapezoid_area(c(100.004, 100.000, 100.002), c(20, 10, 50)),
  expected_area
)

## Degenerate identical m/z values (area == 0 with positive intensity falls back to max)
expect_equal(.trapezoid_area(c(100, 100, 100), c(10, 40, 20)), 40)
expect_equal(.trapezoid_area(c(100, 100, 100), c(0, 0, 0)), 0)

## 8d. .deconvolve_subpeaks edge cases & fallback sigma estimators
## Single segment (k_n < 2L) returns initial fit directly
single_dec <- .deconvolve_subpeaks(
  list(seq_along(mz_pts)),
  mz_pts,
  int_pts,
  exponent = 3
)
expect_equal(length(single_dec), 1L)
expect_equal(single_dec[[1]]$mz, fit_g$mz)

## Two-segment split where segment 1 has only 2 points (!fits[[1]]$valid)
mz_short_seg <- c(100.000, 100.002, 100.004, 100.006, 100.008, 100.010)
int_short_seg <- c(20, 100, 80, 70, 200, 50)
dec_short <- .deconvolve_subpeaks(
  list(1:2, 3:6),
  mz_short_seg,
  int_short_seg,
  exponent = 2
)
expect_equal(length(dec_short), 2L)
raw_fit_seg2 <- .fit_gaussian_peak(
  mz_short_seg[3:6],
  int_short_seg[3:6],
  exponent = 2
)
expect_true(dec_short[[2]]$mz > raw_fit_seg2$mz)

## Two-segment split where segment 1 has a single point (fits[[1]]$sigma == 0)
## and its 3-point neighborhood contains a zero intensity, triggering the
## range fallback sigmas[k] <- max(1e-6, 0.5 * diff(range(mz_raw[idx])))
mz_deg_seg <- c(100.000, 100.002, 100.004, 100.006, 100.008)
int_deg_seg <- c(100, 0, 50, 200, 50)
dec_deg <- .deconvolve_subpeaks(
  list(1L, 2:5),
  mz_deg_seg,
  int_deg_seg,
  exponent = 2
)
expect_equal(length(dec_deg), 2L)
expect_true(all(vapply(dec_deg, function(f) is.finite(f$mz), logical(1))))

## 8e. .group_mz_domain edge cases (n == 0L and n == 1L) and tight exponent brackets
expect_equal(.group_mz_domain(numeric(0)), integer(0))
expect_equal(.group_mz_domain(250.0), 1L)

## At m/z = 400, mz_ref = 100 (ratio = 4), tolerance = 0.001, ppm = 0:
## tof (4^0.5 = 2 -> tol = 0.002): 0.0019 merges (1L), 0.0021 splits (2L)
expect_equal(
  length(unique(.group_mz_domain(
    c(400, 400.0019),
    tolerance = 0.001,
    ppm = 0,
    domain_scaling = "tof",
    mz_ref = 100
  ))),
  1L
)
expect_equal(
  length(unique(.group_mz_domain(
    c(400, 400.0021),
    tolerance = 0.001,
    ppm = 0,
    domain_scaling = "tof",
    mz_ref = 100
  ))),
  2L
)
## orbitrap (4^1.5 = 8 -> tol = 0.008): 0.0079 merges (1L), 0.0081 splits (2L)
expect_equal(
  length(unique(.group_mz_domain(
    c(400, 400.0079),
    tolerance = 0.001,
    ppm = 0,
    domain_scaling = "orbitrap",
    mz_ref = 100
  ))),
  1L
)
expect_equal(
  length(unique(.group_mz_domain(
    c(400, 400.0081),
    tolerance = 0.001,
    ppm = 0,
    domain_scaling = "orbitrap",
    mz_ref = 100
  ))),
  2L
)
## fticr (4^2.0 = 16 -> tol = 0.016): 0.0159 merges (1L), 0.0161 splits (2L)
expect_equal(
  length(unique(.group_mz_domain(
    c(400, 400.0159),
    tolerance = 0.001,
    ppm = 0,
    domain_scaling = "fticr",
    mz_ref = 100
  ))),
  1L
)
expect_equal(
  length(unique(.group_mz_domain(
    c(400, 400.0161),
    tolerance = 0.001,
    ppm = 0,
    domain_scaling = "fticr",
    mz_ref = 100
  ))),
  2L
)

## 8f. .peak_group_mzs & .peak_group_intensities all branches
grp_list <- list(1:3, 4:5)
mz_vec <- c(100.000, 100.002, 100.004, 200.000, 200.002)
int_vec <- c(10, 50, 10, 20, 20)

## weighted = TRUE, centroid_method = "weighted", baseline_subtract = TRUE
## Group 1 has baseline 10 -> c(0, 40, 0) -> centroid exactly 100.002
## Group 2 has 2 points; if we also test a 3-point flat group c(20, 20, 20),
## baseline_subtract leaves weights > 0, whereas c(20, 0, 20) gives w == 0
mzs_bs <- .peak_group_mzs(
  list(1:3, 4:6),
  c(100.000, 100.002, 100.004, 200.000, 200.002, 200.004),
  c(10, 50, 10, 0, 0, 0),
  weighted = TRUE,
  intensity_exponent = 2,
  mzFun = base::mean,
  centroid_method = "weighted",
  baseline_subtract = TRUE
)
expect_equal(mzs_bs, c(100.002, 200.002))

## centroid_method = "func" uses mzFun even when weighted = TRUE
mzs_func <- .peak_group_mzs(
  list(1:3),
  c(100.000, 100.002, 100.010),
  c(1, 1000, 1),
  weighted = TRUE,
  intensity_exponent = 3,
  mzFun = base::mean,
  centroid_method = "func"
)
expect_equal(mzs_func, mean(c(100.000, 100.002, 100.010)))

## .peak_group_intensities with trapezoid, gaussian_area, gaussian_apex, and gate <= 0
gate_zero_fun <- function(x) if (length(x) < 3L) 0 else max(x)
ints_trap <- .peak_group_intensities(
  grp_list,
  c(10, 50, 20, 10, 30),
  intensityFun = gate_zero_fun,
  mz_raw = mz_vec,
  intensity_method = "trapezoid"
)
expect_equal(ints_trap[1], expected_area)
expect_equal(ints_trap[2], 0)

ints_garea <- .peak_group_intensities(
  list(seq_along(mz_pts), 1:2),
  c(int_pts, 10, 20),
  intensityFun = gate_zero_fun,
  mz_raw = c(mz_pts, 600.000, 600.002),
  intensity_method = "gaussian_area",
  intensity_exponent = 3
)
expect_equal(ints_garea[1], fit_g$area)
expect_equal(ints_garea[2], 0)

ints_gapex <- .peak_group_intensities(
  list(seq_along(mz_pts)),
  int_pts,
  intensityFun = base::max,
  mz_raw = mz_pts,
  intensity_method = "gaussian_apex",
  intensity_exponent = 3
)
expect_equal(ints_gapex[1], fit_g$apex)

## 8g. .peaks_combine with deconvolve = TRUE across centroid_method and intensity_method
pk_deconv_area <- .peaks_combine(
  cbind(mz = mz_dbl, intensity = int_dbl),
  tolerance = 0.05,
  ppm = 0,
  intensityFun = base::max,
  mzFun = base::mean,
  weighted = FALSE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  timeDomain = FALSE,
  centroid_method = "func",
  intensity_method = "gaussian_area",
  deconvolve = TRUE,
  valley_ratio = 0.50,
  valley_hws = 1L
)
expect_equal(nrow(pk_deconv_area), 2L)
expect_true(all(pk_deconv_area[, "intensity"] > 0))

## Deconvolve with intensity_method = "trapezoid" and a sub-peak gated to 0
## (second peak has < 8 points so min_datapoints = 8 gates it to 0)
pk_deconv_trap_gated <- .peaks_combine(
  cbind(mz = mz_dbl, intensity = int_dbl),
  tolerance = 0.05,
  ppm = 0,
  intensityFun = function(x) if (length(x) < 10L) 0 else max(x),
  mzFun = base::mean,
  weighted = TRUE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  timeDomain = FALSE,
  centroid_method = "gaussian",
  intensity_method = "trapezoid",
  deconvolve = TRUE,
  valley_ratio = 0.50,
  valley_hws = 1L
)
expect_equal(nrow(pk_deconv_trap_gated), 2L)
expect_true(pk_deconv_trap_gated[1, "intensity"] > 0)
expect_equal(pk_deconv_trap_gated[2, "intensity"], 0)

## 8h. Parameter validation in centroid_one_file() for new arguments
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    centroid_method = "bad"
  ),
  pattern = "should be one of"
)
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    intensity_method = "bad"
  ),
  pattern = "should be one of"
)
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    domain_scaling = "bad"
  ),
  pattern = "should be one of"
)
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    deconvolve = NA
  ),
  pattern = "deconvolve"
)
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    baseline_subtract = NA
  ),
  pattern = "baseline_subtract"
)
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    valley_ratio = -0.1
  ),
  pattern = "valley_ratio"
)
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    valley_ratio = 1.1
  ),
  pattern = "valley_ratio"
)
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    valley_hws = 0L
  ),
  pattern = "valley_hws"
)
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    valley_hws = 1.5
  ),
  pattern = "valley_hws"
)
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    mz_ref = 0
  ),
  pattern = "mz_ref"
)
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    mz_ref = -100
  ),
  pattern = "mz_ref"
)

## ---------------------------------------------------------------------------
## 9. Regression tests for F-01, F-02, F-03 (baseline area, weights, deconvolve)
## ---------------------------------------------------------------------------

## F-01: Baseline-subtracted Gaussian area excludes constant baseline offset
fit_nobg_bs <- .fit_gaussian_peak(
  mz_pts,
  int_pts,
  exponent = 3,
  baseline_subtract = TRUE
)
fit_bg500 <- .fit_gaussian_peak(
  mz_pts,
  int_pts + 500,
  exponent = 3,
  baseline_subtract = TRUE
)
expect_equal(fit_bg500$area, fit_nobg_bs$area, tolerance = 1e-10)

mz_wide <- 500 + (-8:8) * 0.0042
int_wide <- 10000 * exp(-0.5 * ((mz_wide - 500) / sigma)^2)
fit_wide_bg <- .fit_gaussian_peak(
  mz_wide,
  int_wide + 500,
  exponent = 3,
  baseline_subtract = TRUE
)
expect_equal(
  fit_wide_bg$area,
  10000 * sigma * sqrt(2 * pi),
  tolerance = 1e-3
)

## F-01: Deconvolving overlapping peaks with baseline_subtract uses net amplitude
segs_dbl <- .split_peak_group(
  seq_along(int_dbl),
  int_dbl,
  valley_ratio = 0.50,
  hws = 1L
)
dec_nobg <- .deconvolve_subpeaks(
  segs_dbl,
  mz_dbl,
  int_dbl,
  exponent = 3,
  baseline_subtract = FALSE
)
dec_withbg <- .deconvolve_subpeaks(
  segs_dbl,
  mz_dbl,
  int_dbl + 500,
  exponent = 3,
  baseline_subtract = TRUE
)
expect_equal(
  vapply(dec_withbg, `[[`, numeric(1), "area"),
  vapply(dec_nobg, `[[`, numeric(1), "area"),
  tolerance = 0.05
)

## F-02: Negative intensity_exponent rejected; negative intensities & 0 exponent safe
expect_error(
  CentroidR::centroid_one_file(
    "x.mzML",
    pattern = "x",
    replacement = "y",
    intensity_exponent = -1
  ),
  pattern = "intensity_exponent"
)
fit_neg <- .fit_gaussian_peak(
  c(100, 100.001, 100.002),
  c(-5, 0, -2),
  exponent = 1.5
)
expect_equal(fit_neg$mz, 100.001)
expect_false(fit_neg$valid)
expect_equal(
  .peak_group_mzs(
    list(1:3),
    c(100, 100.001, 100.002),
    c(-5, 0, -2),
    weighted = TRUE,
    intensity_exponent = 1.5,
    mzFun = base::mean
  ),
  100.001
)
expect_equal(
  .peak_group_mzs(
    list(1:3),
    c(100, 100.001, 100.002),
    c(10, 100, 10),
    weighted = TRUE,
    intensity_exponent = 0,
    mzFun = base::mean
  ),
  100.001
)

## F-03: deconvolve = TRUE must not alter unsplit singlets or ignore methods
mz_mix <- c(mz_dbl, 600.000, 600.001, 600.002)
int_mix <- c(int_dbl, 20, 100, 20)
res_nodec <- .peaks_combine(
  cbind(mz = mz_mix, intensity = int_mix),
  tolerance = 0.005,
  ppm = 0,
  intensityFun = base::sum,
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
res_dec <- .peaks_combine(
  cbind(mz = mz_mix, intensity = int_mix),
  tolerance = 0.005,
  ppm = 0,
  intensityFun = base::sum,
  mzFun = base::mean,
  weighted = TRUE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  timeDomain = FALSE,
  centroid_method = "weighted",
  intensity_method = "func",
  deconvolve = TRUE,
  valley_ratio = 0.50,
  valley_hws = 1L
)
expect_equal(as.numeric(res_dec[3, "intensity"]), 140)
expect_equal(
  as.numeric(res_dec[3, "mz"]),
  as.numeric(res_nodec[3, "mz"]),
  tolerance = 1e-12
)
expect_false(isTRUE(all.equal(
  as.numeric(res_dec[1:2, "mz"]),
  as.numeric(pk_deconv[, "mz"])
)))

res_dec_gapex <- .peaks_combine(
  cbind(mz = mz_mix, intensity = int_mix),
  tolerance = 0.005,
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
expect_equal(nrow(res_dec_gapex), 3L)
expect_true(abs(as.numeric(res_dec_gapex[2, "intensity"]) - a2) / a2 < 0.005)

## Split doublet trapezoid area with deconvolve = TRUE uses tail-corrected intensities
trap_raw <- .peaks_combine(
  cbind(mz = mz_dbl, intensity = int_dbl),
  tolerance = 0.05,
  ppm = 0,
  intensityFun = base::max,
  weighted = TRUE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  intensity_method = "trapezoid",
  deconvolve = FALSE,
  valley_ratio = 0.50,
  valley_hws = 1L
)
trap_dec <- .peaks_combine(
  cbind(mz = mz_dbl, intensity = int_dbl),
  tolerance = 0.05,
  ppm = 0,
  intensityFun = base::max,
  weighted = TRUE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  intensity_method = "trapezoid",
  deconvolve = TRUE,
  valley_ratio = 0.50,
  valley_hws = 1L
)
expect_true(all(trap_dec[, "intensity"] < trap_raw[, "intensity"]))

## Low-intensity scale invariance (no 1e-6 clamp distortion)
pk_scale1 <- .peaks_combine(
  cbind(mz = mz_dbl, intensity = int_dbl),
  tolerance = 0.05,
  ppm = 0,
  intensityFun = base::max,
  weighted = TRUE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  centroid_method = "gaussian",
  intensity_method = "gaussian_area",
  deconvolve = TRUE,
  valley_ratio = 0.50,
  valley_hws = 1L
)
pk_scale9 <- .peaks_combine(
  cbind(mz = mz_dbl, intensity = int_dbl * 1e-9),
  tolerance = 0.05,
  ppm = 0,
  intensityFun = base::max,
  weighted = TRUE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  centroid_method = "gaussian",
  intensity_method = "gaussian_area",
  deconvolve = TRUE,
  valley_ratio = 0.50,
  valley_hws = 1L
)
expect_equal(
  pk_scale9[, "intensity"],
  pk_scale1[, "intensity"] * 1e-9,
  tolerance = 1e-4
)
