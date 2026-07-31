library(tinytest)

.split_peak_group <- getFromNamespace(".split_peak_group", "CentroidR")
.peak_group_indices <- getFromNamespace(".peak_group_indices", "CentroidR")
.peak_group_names <- getFromNamespace(".peak_group_names", "CentroidR")
.peak_group_mzs <- getFromNamespace(".peak_group_mzs", "CentroidR")
.peak_group_intensities <- getFromNamespace(
  ".peak_group_intensities",
  "CentroidR"
)
.peak_group_metadata <- getFromNamespace(".peak_group_metadata", "CentroidR")
.onLoad <- getFromNamespace(".onLoad", "CentroidR")
.onAttach <- getFromNamespace(".onAttach", "CentroidR")

expect_equal(.split_peak_group(1:2, c(10, 20)), list(1:2))
expect_equal(.split_peak_group(1:5, c(1, 4, 3, 4, 1)), list(1:5))
expect_equal(
  .split_peak_group(1:9, c(1, 3, 1, 3, 1, 0, 1, 3, 1)),
  list(1:6, 7:9)
)

grps <- c(1, 1, 2, 2, 2)
expect_equal(
  .peak_group_indices(grps),
  structure(list(1:2, 3:5), names = c("1", "2"))
)
expect_equal(.peak_group_names(.peak_group_indices(grps)), c("1", "2"))

peak_groups <- list(1:2, 3:4)
mz_raw <- c(100, 110, 200, 220)
int_raw <- c(1, 3, 5, 7)
expect_equal(
  .peak_group_mzs(
    peak_groups = peak_groups,
    mz_raw = mz_raw,
    int_raw = int_raw,
    weighted = TRUE,
    intensity_exponent = 2,
    mzFun = base::mean
  ),
  c(
    stats::weighted.mean(mz_raw[1:2], int_raw[1:2]^2),
    stats::weighted.mean(mz_raw[3:4], int_raw[3:4]^2)
  )
)
expect_equal(
  .peak_group_mzs(
    peak_groups = peak_groups,
    mz_raw = mz_raw,
    int_raw = int_raw,
    weighted = FALSE,
    intensity_exponent = 2,
    mzFun = base::mean
  ),
  c(mean(mz_raw[1:2]), mean(mz_raw[3:4]))
)

expect_equal(
  .peak_group_intensities(peak_groups, int_raw, base::max),
  c(3, 7)
)

meta <- data.frame(
  charge = c(1, 1, 2, 2),
  adduct = c("H+", "Na+", "K+", "K+"),
  stringsAsFactors = FALSE
)
meta_out <- .peak_group_metadata(list(1:2, 3:4), meta)
expect_equal(meta_out[[1]][, "charge"], 1)
expect_true(is.na(meta_out[[1]][, "adduct"]))
expect_equal(meta_out[[2]][, "charge"], 2)
expect_equal(meta_out[[2]][, "adduct"], "K+")

suppressWarnings(.onLoad(tempdir(), "CentroidR"))
suppressMessages(.onAttach(tempdir(), "CentroidR"))

tmp_in <- tempfile(pattern = "profile_", fileext = ".mzML")
writeLines("dummy", tmp_in)
tmp_out <- sub("profile_", "out_", tmp_in, fixed = TRUE)
writeLines("dummy", tmp_out)
expect_equal(
  CentroidR::centroid_one_file(
    file = tmp_in,
    pattern = "profile_",
    replacement = "out_"
  ),
  TRUE
)
expect_equal(
  CentroidR::centroid_one_file(
    file = tempfile(fileext = ".mzML"),
    pattern = "profile_",
    replacement = "out_"
  ),
  FALSE
)
expect_error(
  CentroidR::centroid_one_file(
    file = 1,
    pattern = "profile_",
    replacement = "out_"
  ),
  info = "Invalid argument types should fail validation"
)
