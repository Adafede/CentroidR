## Regression tests for weighted m/z calculation, peak alignment, metadata
## preservation, and dense trace splitting across package internals.

library(tinytest)

source(
  if (file.exists("helper-logging.R")) {
    "helper-logging.R"
  } else {
    "inst/tinytest/helper-logging.R"
  }
)

.peak_group_mzs <- getFromNamespace(".peak_group_mzs", "CentroidR")
.peak_group_intensities <- getFromNamespace(
  ".peak_group_intensities",
  "CentroidR"
)
.peak_group_metadata <- getFromNamespace(".peak_group_metadata", "CentroidR")
.peaks_combine <- getFromNamespace(".peaks_combine", "CentroidR")
.split_peak_group <- getFromNamespace(".split_peak_group", "CentroidR")

## Intensity weighting uses int^exponent without a +1 offset
mz_vals <- c(100.001, 100.002, 100.003)
int_vals <- c(10, 500, 200)
expected_wmean <- stats::weighted.mean(mz_vals, int_vals^3)
actual_wmean <- .peak_group_mzs(
  peak_groups = list(seq_along(mz_vals)),
  mz_raw = mz_vals,
  int_raw = int_vals,
  weighted = TRUE,
  intensity_exponent = 3,
  mzFun = base::mean
)
expect_equal(as.numeric(actual_wmean), expected_wmean, tolerance = 1e-12)

## Misalignment guard across two well-separated groups via .peaks_combine()
two_peak_mat <- cbind(
  mz = c(
    100.000,
    100.001,
    100.002,
    100.003,
    100.004,
    105.000,
    105.001,
    105.002,
    105.003,
    105.004
  ),
  intensity = c(100, 500, 1000, 500, 100, 200, 800, 1200, 800, 200)
)
res_two <- .peaks_combine(
  two_peak_mat,
  ppm = 50,
  tolerance = 0,
  intensityFun = base::max,
  weighted = TRUE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  intensity_exponent = 3
)
expect_equal(nrow(res_two), 2L)
expect_equal(
  as.numeric(res_two[, "mz"]),
  c(
    stats::weighted.mean(
      two_peak_mat[1:5, "mz"],
      two_peak_mat[1:5, "intensity"]^3
    ),
    stats::weighted.mean(
      two_peak_mat[6:10, "mz"],
      two_peak_mat[6:10, "intensity"]^3
    )
  ),
  tolerance = 1e-12
)
expect_equal(as.numeric(res_two[, "intensity"]), c(1000, 1200))

## .peak_group_intensities() and .peak_group_metadata() align to peak groups
meta_df <- data.frame(charge = c(1L, 1L, 1L, 2L, 2L, 2L))
grps_two <- list(1:3, 4:6)
ints_raw <- c(100, 500, 200, 300, 800, 400)
ints_out <- .peak_group_intensities(
  peak_groups = grps_two,
  int_raw = ints_raw,
  intensityFun = base::max
)
meta_out <- do.call(rbind, .peak_group_metadata(grps_two, meta_df))
expect_equal(as.numeric(ints_out), c(500, 800))
expect_equal(meta_out$charge, c(1L, 2L))

## Dense profile trace splits into two peaks rather than merging across a valley
mz_trace <- c(
  164.0338839993696,
  164.0357619330662,
  164.03763986676282,
  164.0395178327084,
  164.041395798654,
  164.04327377534926,
  164.04515176279418,
  164.0470297609887,
  164.04890776993292,
  164.05078578962676,
  164.05266382007028,
  164.05454186126343,
  164.05641991320627,
  164.0582979758987,
  164.06017604934087,
  164.0620541335326,
  164.06393222847407,
  164.06581033416512,
  164.06768845060589,
  164.06956657779625,
  164.0714447157363,
  164.073322864426,
  164.07520102386536,
  164.07707919405433,
  164.07895737499302,
  164.08083556668132,
  164.0827137691193,
  164.08459198230688,
  164.08647020624417,
  164.08834844093107,
  164.09022668636769,
  164.09210494255387,
  164.09398320948978,
  164.09586148717528,
  164.09773977561048,
  164.09961807479533,
  164.1014963847298,
  164.10337470541398,
  164.10525303684776,
  164.10713137903124,
  164.10900973196433,
  164.1108880956471,
  164.1127664700795,
  164.1146448552616,
  164.11652325119329,
  164.1184016578747,
  164.1202800753057,
  164.1221584927367,
  164.12403694241672,
  164.12591539209674,
  164.1277938525264,
  164.129672312956,
  164.1315508056346,
  164.1334292983132,
  164.13530779099182,
  164.1371863159194,
  164.13906484084697,
  164.1484576159799,
  164.15033621615504,
  164.15221481633017
)
int_trace <- c(
  49,
  31,
  0,
  27,
  0,
  22,
  73,
  169,
  536,
  1090,
  1724,
  1502,
  759,
  384,
  176,
  95,
  28,
  84,
  100,
  237,
  158,
  74,
  66,
  104,
  46,
  87,
  42,
  78,
  73,
  43,
  112,
  177,
  174,
  53,
  104,
  263,
  470,
  1311,
  2749,
  3613,
  2389,
  1404,
  816,
  371,
  208,
  154,
  90,
  0,
  57,
  71,
  0,
  0,
  27,
  0,
  0,
  27,
  0,
  0,
  27,
  0
)

segments <- .split_peak_group(seq_along(int_trace), int_trace)
expect_equal(length(segments), 9L)

trace_combined <- .peaks_combine(
  cbind(mz = mz_trace, intensity = int_trace),
  ppm = 100,
  tolerance = 0.05,
  intensityFun = base::max,
  weighted = TRUE,
  spectrumMsLevel = 1L,
  msLevel = 1L,
  intensity_exponent = 3
)
expect_equal(nrow(trace_combined), 9L)
idx_p1 <- which(abs(trace_combined[, "mz"] - 164.0526638) < 0.001)
idx_p2 <- which(abs(trace_combined[, "mz"] - 164.1071314) < 0.001)
expect_equal(length(idx_p1), 1L)
expect_equal(length(idx_p2), 1L)
expect_equal(as.numeric(trace_combined[idx_p1, "intensity"]), 1724)
expect_equal(as.numeric(trace_combined[idx_p2, "intensity"]), 3613)
