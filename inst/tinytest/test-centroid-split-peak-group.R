## Tests for .split_peak_group: splitting a dense profile peak group into
## individual peaks by finding the deepest qualifying valley.
##
## The function walks consecutive pairs of local maxima and picks the valley with
## the smallest ratio of valley intensity to the weaker of the two flanking
## maxima, but only among valleys whose ratio is at or below `valley_ratio`.
## These tests pin down that selection rule.

library(tinytest)

## Repairs the global logger appender that setup_logger() installs and never
## removes; see helper-logging.R.
source("helper-logging.R")

.split_peak_group <- getFromNamespace(".split_peak_group", "CentroidR")

peak_centroid <- function(int_raw, idx, exponent = 3) {
  stats::weighted.mean(idx, int_raw[idx]^exponent)
}

## ---------------------------------------------------------------------------
## Guards on degenerate input
## ---------------------------------------------------------------------------

## Fewer than three points cannot contain two maxima, so nothing is split.
expect_equal(.split_peak_group(1:2, c(10, 20)), list(1:2))
expect_equal(.split_peak_group(1L, 5), list(1L))
expect_equal(.split_peak_group(integer(0), numeric(0)), list(integer(0)))

## A single-peaked group has only one maximum and stays whole.
expect_equal(
  .split_peak_group(1:9, c(1, 3, 10, 30, 100, 30, 10, 3, 1)),
  list(1:9)
)

## A flat trace has no local maxima at all.
expect_equal(.split_peak_group(1:5, rep(7, 5)), list(1:5))

## A strictly decreasing trace has a single local maximum at the first point,
## so the "fewer than two maxima" path is taken and the group is returned whole.
expect_equal(
  MsCoreUtils::localMaxima(c(5, 4, 3, 2, 1), hws = 2L),
  c(TRUE, FALSE, FALSE, FALSE, FALSE)
)
expect_equal(.split_peak_group(1:5, c(5, 4, 3, 2, 1)), list(1:5))

## The same holds when the intensities run through zero and back up, so the
## "no local maxima" path is reached even though intensities change sign.
through_zero <- c(0, -20, -10, -10, -1)
expect_equal(MsCoreUtils::localMaxima(through_zero, hws = 2L), rep(FALSE, 5))
expect_equal(
  .split_peak_group(seq_along(through_zero), through_zero),
  list(1:5)
)

## ---------------------------------------------------------------------------
## A clear valley splits a group in two
## ---------------------------------------------------------------------------

two_peaks <- c(1, 5, 20, 60, 100, 60, 20, 5, 1, 5, 20, 60, 100, 60, 20, 5, 1)
segs <- .split_peak_group(seq_along(two_peaks), two_peaks)
expect_equal(length(segs), 2L)
expect_equal(segs[[1]], 1:9)
expect_equal(segs[[2]], 10:17)

## Each half peaks at the intended apex: the centroid of segment 1 sits near
## m/z index 5 and segment 2 near index 13, both within 2 points of the apex.
expect_true(abs(peak_centroid(two_peaks, segs[[1]]) - 5) < 2)
expect_true(abs(peak_centroid(two_peaks, segs[[2]]) - 13) < 2)

## Two symmetric peaks separated by a valley at half the apex height.
## Apexes are at indices 3 and 7 with intensity 100, so the valley ratio is 50/100.
shallow <- c(0, 50, 100, 50, 50, 50, 100, 50, 0)
expect_equal(
  MsCoreUtils::localMaxima(shallow, hws = 2L),
  c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
)
expect_equal(.split_peak_group(seq_along(shallow), shallow), list(1:9))
expect_equal(
  .split_peak_group(seq_along(shallow), shallow, valley_ratio = 0.19),
  list(1:9)
)

## Raising valley_ratio to exactly the valley ratio of 0.5 makes it split.
expect_equal(
  length(.split_peak_group(seq_along(shallow), shallow, valley_ratio = 0.5)),
  2L
)

## ---------------------------------------------------------------------------
## Valley selection: only qualifying valleys are considered, and the deepest
## qualifying one wins
## ---------------------------------------------------------------------------

## Local maxima sit at positions 2, 6 and 9.
##   pair 2-6 : valley at 5, floor 2, ratio 0.00 -> qualifies (<= 0.2)
##   pair 6-9 : valley at 8, floor 2, ratio 0.50 -> does NOT qualify
## The second gap is well above the ratio but the split must happen at the
## first (and only qualifying) valley, leaving exactly two segments.
mixed <- c(2, 10, 1, 1, 0, 2, 2, 1, 50, 1)
mixed_segs <- .split_peak_group(seq_along(mixed), mixed)
expect_equal(length(mixed_segs), 2L)
expect_equal(mixed_segs[[1]], 1:5)
expect_equal(mixed_segs[[2]], 6:10)

## At valley_ratio 0.5 the second, shallower valley also qualifies. The function
## picks the single deepest qualifying valley (index 5) and recurses, so the
## first split is still at the deepest valley; the recursive call on the
## remaining segment then finds the second valley and splits there too.
mixed_wide <- .split_peak_group(seq_along(mixed), mixed, valley_ratio = 0.5)
expect_equal(length(mixed_wide), 3L)
expect_equal(mixed_wide[[1]], 1:5)
expect_equal(mixed_wide[[2]], 6:8)
expect_equal(mixed_wide[[3]], 9:10)

## Just below the shallow valley's ratio only the deep valley qualifies, so the
## group stays in two pieces and the extra valley is left untouched.
mixed_tight <- .split_peak_group(seq_along(mixed), mixed, valley_ratio = 0.2)
expect_equal(length(mixed_tight), 2L)
expect_equal(mixed_tight[[1]], 1:5)
expect_equal(mixed_tight[[2]], 6:10)

## ---------------------------------------------------------------------------
## valley_ratio boundary
## ---------------------------------------------------------------------------

## Two symmetric peaks of 100 separated by a valley of exactly 20, i.e. a valley
## ratio of exactly 0.2 which equals the default valley_ratio.
boundary <- c(0, 50, 100, 50, 20, 50, 100, 50, 0)
expect_equal(
  MsCoreUtils::localMaxima(boundary, hws = 2L),
  c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
)

## The comparison is inclusive, so a ratio exactly at valley_ratio splits.
boundary_segs <- .split_peak_group(
  seq_along(boundary),
  boundary,
  valley_ratio = 0.2
)
expect_equal(length(boundary_segs), 2L)
expect_equal(boundary_segs[[1]], 1:5)
expect_equal(boundary_segs[[2]], 6:9)

## One hundredth below the ratio does not split.
expect_equal(
  .split_peak_group(seq_along(boundary), boundary, valley_ratio = 0.19),
  list(1:9)
)

## A valley well below the ratio splits under every ratio at or above it.
deep <- c(0, 50, 100, 50, 5, 50, 100, 50, 0)
expect_equal(
  length(.split_peak_group(seq_along(deep), deep, valley_ratio = 0.05)),
  2L
)
expect_equal(
  length(.split_peak_group(seq_along(deep), deep, valley_ratio = 0.2)),
  2L
)
expect_equal(
  length(.split_peak_group(seq_along(deep), deep, valley_ratio = 0.9)),
  2L
)

## The two halves peak at the intended apexes (indices 3 and 7).
expect_true(abs(peak_centroid(deep, 1:5) - 3) < 0.5)
expect_equal(peak_centroid(deep, 6:9), 7)

## ---------------------------------------------------------------------------
## hws controls how wide a local maximum must be
## ---------------------------------------------------------------------------

expect_equal(
  .split_peak_group(seq_along(two_peaks), two_peaks, hws = 1L),
  .split_peak_group(seq_along(two_peaks), two_peaks, hws = 2L)
)
expect_equal(
  length(.split_peak_group(seq_along(two_peaks), two_peaks, hws = 1L)),
  2L
)

## ---------------------------------------------------------------------------
## A valley floor of zero must not be divided by
## ---------------------------------------------------------------------------

## Local maxima at indices 4 and 7 (value 1 and 0) with a valley of -20 between
## them, so the weaker of the two flanking maxima is exactly zero. The peak
## floor is used as a divisor, so this pair is skipped rather than producing a
## non-finite valley ratio.
zero_floor <- c(-10, -20, -1, 1, -20, -10, 0, -10)
expect_equal(
  MsCoreUtils::localMaxima(zero_floor, hws = 2L),
  c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
)
zero_segs <- .split_peak_group(seq_along(zero_floor), zero_floor)
expect_equal(length(zero_segs), 1L)
expect_equal(zero_segs[[1]], 1:8)

## ---------------------------------------------------------------------------
## The returned segments always partition the input indices exactly once
## ---------------------------------------------------------------------------

all_segs <- .split_peak_group(seq_along(two_peaks), two_peaks)
expect_equal(sort(unlist(all_segs)), seq_along(two_peaks))
expect_equal(length(unlist(all_segs)), length(unique(unlist(all_segs))))

deep_segs <- .split_peak_group(seq_along(deep), deep, valley_ratio = 0.9)
expect_equal(sort(unlist(deep_segs)), seq_along(deep))
expect_equal(length(unlist(deep_segs)), length(unique(unlist(deep_segs))))
