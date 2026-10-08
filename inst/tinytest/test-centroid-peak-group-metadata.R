## Tests for .peak_group_metadata: aggregating the extra per-peak columns that
## travel alongside m/z and intensity when profile peaks are combined.
##
## A column that is constant within a peak group keeps its value; a column that
## varies within the group cannot be attributed to the centroided peak and is
## therefore recorded as a missing value.

library(tinytest)

.peak_group_metadata <- getFromNamespace(".peak_group_metadata", "CentroidR")

## ---------------------------------------------------------------------------
## Constant columns keep their values
## ---------------------------------------------------------------------------

meta <- data.frame(
  charge = c(1L, 1L, 2L, 2L),
  adduct = c("H+", "H+", "K+", "K+"),
  stringsAsFactors = FALSE
)
out <- .peak_group_metadata(list(1:2, 3:4), meta)

expect_equal(length(out), 2L)
expect_true(all(vapply(out, is.data.frame, logical(1))))
expect_equal(colnames(out[[1]]), c("charge", "adduct"))
expect_equal(out[[1]][, "adduct"], "H+")
expect_equal(out[[2]][, "adduct"], "K+")

## ---------------------------------------------------------------------------
## Varying columns become missing, and the missing value is logical NA
## ---------------------------------------------------------------------------
## The placeholder must be a plain logical NA so that combining it with the
## typed columns of other groups yields a logical column rather than coercing
## the whole metadata block to integer, double or character.

varying <- data.frame(
  charge = c(1L, 2L, 2L, 3L),
  adduct = c("H+", "Na+", "K+", "K+"),
  stringsAsFactors = FALSE
)
vary_out <- .peak_group_metadata(list(1:2, 3:4), varying)

expect_equal(length(vary_out), 2L)
expect_true(is.na(vary_out[[1]][, "adduct"]))
expect_true(is.na(vary_out[[1]][, "charge"]))
expect_true(is.na(vary_out[[2]][, "charge"]))

## Group 3:4 happens to share an adduct ("K+"), so that column survives there,
## while the disagreeing charge column is replaced by a logical NA.
expect_equal(vary_out[[2]][, "adduct"], "K+")
expect_true(is.logical(vary_out[[1]][, "adduct"]))
expect_true(is.logical(vary_out[[1]][, "charge"]))
expect_true(is.logical(vary_out[[2]][, "charge"]))

## Every placeholder is logical NA, never a typed NA.
expect_equal(typeof(vary_out[[1]][, "adduct"]), "logical")
expect_equal(typeof(vary_out[[2]][, "charge"]), "logical")

## ---------------------------------------------------------------------------
## The aggregation operates per group, so mixed groups keep their own columns
## ---------------------------------------------------------------------------

mixed <- data.frame(
  charge = c(1L, 1L, 2L, 3L),
  adduct = c("H+", "H+", "K+", "K+"),
  stringsAsFactors = FALSE
)
mixed_out <- .peak_group_metadata(list(1:2, 3:4), mixed)

## Group 1:2 has constant charge and adduct, so it survives intact.
expect_equal(as.numeric(mixed_out[[1]][, "charge"]), 1)
expect_equal(mixed_out[[1]][, "adduct"], "H+")

## Group 3:4 has charges 2 and 3, which disagree, but a constant adduct.
expect_true(is.na(mixed_out[[2]][, "charge"]))
expect_equal(mixed_out[[2]][, "adduct"], "K+")
expect_true(is.logical(mixed_out[[2]][, "charge"]))

## ---------------------------------------------------------------------------
## Other column types
## ---------------------------------------------------------------------------

## Numeric columns that are constant are kept.
num <- data.frame(rt = c(1.5, 1.5, 2.5, 2.5), score = c(10, 20, 30, 40))
num_out <- .peak_group_metadata(list(1:2, 3:4), num)
expect_equal(as.numeric(num_out[[1]][, "rt"]), 1.5)
expect_equal(as.numeric(num_out[[2]][, "rt"]), 2.5)
expect_true(is.na(num_out[[1]][, "score"]))
expect_true(is.na(num_out[[2]][, "score"]))
expect_true(is.logical(num_out[[1]][, "score"]))

## Logical columns that are constant are kept as they are.
lgl <- data.frame(isolated = c(TRUE, TRUE, FALSE, FALSE))
lgl_out <- .peak_group_metadata(list(1:2, 3:4), lgl)
expect_true(isTRUE(lgl_out[[1]][, "isolated"]))
expect_true(isFALSE(lgl_out[[2]][, "isolated"]))

## A logical column that varies within a group becomes logical NA.
lgl_var <- data.frame(isolated = c(TRUE, FALSE, TRUE, FALSE))
lgl_var_out <- .peak_group_metadata(list(1:2, 3:4), lgl_var)
expect_true(is.na(lgl_var_out[[1]][, "isolated"]))
expect_true(is.na(lgl_var_out[[2]][, "isolated"]))
expect_true(is.logical(lgl_var_out[[1]][, "isolated"]))

## ---------------------------------------------------------------------------
## Group count and ordering
## ---------------------------------------------------------------------------

## One entry is returned per peak group, in order.
many <- .peak_group_metadata(list(1:2, 3:4, 3:4), meta)
expect_equal(length(many), 3L)
expect_equal(as.numeric(many[[1]][, "charge"]), 1)
expect_equal(as.numeric(many[[2]][, "charge"]), 2)
expect_equal(as.numeric(many[[3]][, "charge"]), 2)
expect_equal(many[[1]][, "adduct"], "H+")
expect_equal(many[[2]][, "adduct"], "K+")
expect_equal(many[[3]][, "adduct"], "K+")

## A single group still produces a one row data frame.
single <- .peak_group_metadata(list(1:2), meta)
expect_equal(length(single), 1L)
expect_equal(nrow(single[[1]]), 1L)
expect_equal(colnames(single[[1]]), c("charge", "adduct"))
