## Tests for .peaks_combine: the m/z grouping, tolerance and metadata logic that
## turns raw profile peak lists into centroided peaks.
##
## These assert on concrete expected peak counts, m/z values and intensities so
## that a change in the tolerance arithmetic (Da vs ppm, time-domain conversion)
## or in the metadata passthrough is detected.

library(tinytest)

## Repairs the global logger appender that setup_logger() installs and never
## removes; see helper-logging.R.
source("helper-logging.R")

.peaks_combine <- getFromNamespace(".peaks_combine", "CentroidR")

pk <- function(mz, intensity) cbind(mz = mz, intensity = intensity)

combine <- function(x, ...) {
  args <- list(
    x,
    intensityFun = base::max,
    mzFun = base::mean,
    weighted = FALSE,
    spectrumMsLevel = 1L,
    msLevel = 1L,
    timeDomain = FALSE
  )
  do.call(.peaks_combine, utils::modifyList(args, list(...)))
}

## ---------------------------------------------------------------------------
## Da tolerance: points closer together than `tolerance` merge into one peak
## ---------------------------------------------------------------------------

## 0.0005 Da apart, tolerance 0.0025 -> single peak
one_peak <- combine(
  pk(c(100, 100.0005, 100.001), c(1, 10, 1)),
  tolerance = 0.0025,
  ppm = 0
)
expect_equal(nrow(one_peak), 1L)
expect_equal(as.numeric(one_peak[1, "mz"]), 100.0005)
expect_equal(one_peak[1, "intensity"], 10)

## 0.01 Da apart, same tolerance -> stays two peaks
two_peaks <- combine(pk(c(100, 100.01), c(1, 10)), tolerance = 0.0025, ppm = 0)
expect_equal(nrow(two_peaks), 2L)
expect_equal(as.numeric(two_peaks[, "mz"]), c(100, 100.01))
expect_equal(as.numeric(two_peaks[, "intensity"]), c(1, 10))

## Tolerance exactly on the edge: 0.0025 Da span merges (inclusive upper bound)
edge <- combine(pk(c(100, 100.0025), c(1, 10)), tolerance = 0.0025, ppm = 0)
expect_equal(nrow(edge), 1L)
expect_equal(as.numeric(edge[1, "mz"]), 100.00125)
expect_equal(edge[1, "intensity"], 10)

## Just beyond the edge: 0.0026 Da span must NOT merge
just_beyond <- combine(
  pk(c(100, 100.0026), c(1, 10)),
  tolerance = 0.0025,
  ppm = 0
)
expect_equal(nrow(just_beyond), 2L)

## ---------------------------------------------------------------------------
## ppm tolerance is independent of the Da tolerance
## ---------------------------------------------------------------------------

## 0.0005 Da = 5 ppm at m/z 100 -> merges under 20 ppm
ppm_merge <- combine(
  pk(c(100, 100.0005, 100.001), c(1, 10, 1)),
  tolerance = 0,
  ppm = 20
)
expect_equal(nrow(ppm_merge), 1L)
expect_equal(as.numeric(ppm_merge[1, "mz"]), 100.0005)
expect_equal(ppm_merge[1, "intensity"], 10)

## 0.01 Da = 100 ppm at m/z 100 -> stays split under 20 ppm
ppm_split <- combine(pk(c(100, 100.01), c(1, 10)), tolerance = 0, ppm = 20)
expect_equal(nrow(ppm_split), 2L)

## The same absolute 0.01 Da gap is 100 ppm at m/z 100 but only 50 ppm at m/z 200,
## so a ppm-only tolerance separates the low-mass pair but merges the high one.
ppm_low <- combine(pk(c(100, 100.01), c(1, 10)), tolerance = 0, ppm = 60)
expect_equal(nrow(ppm_low), 2L)
ppm_high <- combine(pk(c(200, 200.01), c(1, 10)), tolerance = 0, ppm = 60)
expect_equal(nrow(ppm_high), 1L)
expect_equal(ppm_high[1, "mz"], 200.005)
expect_equal(ppm_high[1, "intensity"], 10)

## ---------------------------------------------------------------------------
## Time domain: Da tolerance is converted through sqrt(m/z) space
## ---------------------------------------------------------------------------
## With timeDomain = TRUE the grouping happens on sqrt(mz) and the Da
## tolerance is divided by the smallest sqrt(mz) in the spectrum. At m/z 100
## that base is exactly 10, so tolerance 0.005 Da becomes 5e-04 in sqrt space,
## i.e. a very tight window that does NOT merge points 0.02 Da apart.
sq <- pk(c(100, 100.02, 100.04, 100.06), c(1, 10, 1, 1))

td_tight <- combine(sq, tolerance = 0.005, ppm = 0, timeDomain = TRUE)
expect_equal(nrow(td_tight), 4L)
expect_equal(as.numeric(td_tight[, "mz"]), c(100, 100.02, 100.04, 100.06))
expect_equal(as.numeric(td_tight[, "intensity"]), c(1, 10, 1, 1))

## The merge boundary confirms the conversion. Points are 0.02 Da apart and
## mz_base is sqrt(100) == 10, so the effective window is tolerance / 10:
## it needs tolerance >= 0.02 / 10 * 10 == 0.01 Da to take effect.
td_tightest <- combine(sq, tolerance = 0.005, ppm = 0, timeDomain = TRUE)
expect_equal(nrow(td_tightest), 4L)

td_wide <- combine(sq, tolerance = 0.01, ppm = 0, timeDomain = TRUE)
expect_equal(nrow(td_wide), 1L)
expect_equal(as.numeric(td_wide[1, "mz"]), mean(c(100, 100.02, 100.04, 100.06)))
expect_equal(td_wide[1, "intensity"], 10)

## In the linear domain the same data needs the full 0.02 Da, so the time-domain
## window is genuinely narrower by the factor mz_base == 10.
td_vs_linear <- combine(sq, tolerance = 0.01, ppm = 0, timeDomain = FALSE)
expect_equal(nrow(td_vs_linear), 4L)
expect_equal(nrow(td_vs_linear), nrow(td_tightest))

## A tolerance that is enough in the linear domain merges there but not in the
## time domain, isolating the / mz_base conversion itself.
lin_only <- combine(sq, tolerance = 0.02, ppm = 0, timeDomain = FALSE)
expect_equal(nrow(lin_only), 1L)
expect_equal(nrow(lin_only), nrow(td_wide))
expect_true(nrow(td_tightest) > nrow(lin_only))

## ---------------------------------------------------------------------------
## Spectra of a non-requested MS level are passed through untouched
## ---------------------------------------------------------------------------

## msLevel = 1 was requested but this spectrum is MS2: no grouping at all.
ms_skipped <- combine(
  pk(c(100, 100.001, 200, 200.001), c(1, 10, 5, 20)),
  tolerance = 0.05,
  ppm = 0,
  spectrumMsLevel = 2L,
  msLevel = 1L
)
expect_equal(nrow(ms_skipped), 4L)
expect_equal(as.numeric(ms_skipped[, "mz"]), c(100, 100.001, 200, 200.001))
expect_equal(as.numeric(ms_skipped[, "intensity"]), c(1, 10, 5, 20))

## Same data with a matching MS level does group.
ms_processed <- combine(
  pk(c(100, 100.001, 200, 200.001), c(1, 10, 5, 20)),
  tolerance = 0.05,
  ppm = 0,
  spectrumMsLevel = 1L,
  msLevel = 1L
)
expect_equal(nrow(ms_processed), 2L)
expect_equal(as.numeric(ms_processed[, "mz"]), c(100.0005, 200.0005))
expect_equal(as.numeric(ms_processed[, "intensity"]), c(10, 20))

## An empty peak list is returned as-is.
empty_out <- combine(pk(numeric(0), numeric(0)), tolerance = 0.0025, ppm = 0)
expect_equal(nrow(empty_out), 0L)
expect_equal(colnames(empty_out), c("mz", "intensity"))

## ---------------------------------------------------------------------------
## Peak lists whose m/z values are all distinct are returned unchanged
## ---------------------------------------------------------------------------

no_dups <- combine(
  pk(c(100, 200, 300), c(1, 10, 100)),
  tolerance = 0.0025,
  ppm = 0
)
expect_equal(nrow(no_dups), 3L)
expect_equal(as.numeric(no_dups[, "mz"]), c(100, 200, 300))
expect_equal(as.numeric(no_dups[, "intensity"]), c(1, 10, 100))
expect_equal(colnames(no_dups), c("mz", "intensity"))

## The no-duplicate case short-circuits before the grouping machinery runs, so
## the input object is returned verbatim. Its row names are therefore whatever
## the caller's peak matrix carried (NULL for a bare cbind), not the "1","2",...
## labels that the grouping path would attach.
expect_null(rownames(no_dups))
expect_equal(
  rownames(combine(
    pk(c(100, 200, 300), c(1, 10, 100)),
    tolerance = 0.0025,
    ppm = 0
  )),
  NULL
)

## Once any m/z values do collide, the grouping path runs and the result does
## carry positional row names.
with_dups <- combine(
  pk(c(100, 100.0005, 200), c(1, 10, 100)),
  tolerance = 0.0025,
  ppm = 0
)
expect_equal(nrow(with_dups), 2L)
expect_equal(rownames(with_dups), c("1", "2"))

## ---------------------------------------------------------------------------
## Additional peak columns are carried through and aggregated
## ---------------------------------------------------------------------------

x_meta <- data.frame(
  mz = c(100, 100.001, 200, 200.001),
  intensity = c(1, 10, 5, 20),
  charge = c(1L, 1L, 2L, 2L)
)
meta_out <- combine(x_meta, tolerance = 0.05, ppm = 0)
expect_equal(nrow(meta_out), 2L)
expect_equal(colnames(meta_out), c("mz", "intensity", "charge"))
expect_equal(as.numeric(meta_out[, "mz"]), c(100.0005, 200.0005))
expect_equal(as.numeric(meta_out[, "intensity"]), c(10, 20))
## charge is constant within each group, so it survives unchanged.
expect_equal(meta_out[, "charge"], c(1L, 2L))

## A column that is NOT constant within a group becomes NA for that group.
x_meta2 <- data.frame(
  mz = c(100, 100.001, 200, 200.001),
  intensity = c(1, 10, 5, 20),
  charge = c(1L, 2L, 2L, 3L)
)
meta_out2 <- combine(x_meta2, tolerance = 0.05, ppm = 0)
expect_equal(nrow(meta_out2), 2L)
expect_equal(as.numeric(meta_out2[, "mz"]), c(100.0005, 200.0005))
expect_true(all(is.na(meta_out2[, "charge"])))

## Several metadata columns are handled independently.
x_meta3 <- data.frame(
  mz = c(100, 100.001, 200, 200.001),
  intensity = c(1, 10, 5, 20),
  charge = c(1L, 1L, 2L, 2L),
  adduct = c("H+", "H+", "K+", "K+")
)
meta_out3 <- combine(x_meta3, tolerance = 0.05, ppm = 0)
expect_equal(colnames(meta_out3), c("mz", "intensity", "charge", "adduct"))
expect_equal(meta_out3[, "charge"], c(1L, 2L))
expect_equal(meta_out3[, "adduct"], c("H+", "K+"))

## With metadata present, m/z and intensity keep their canonical names and
## order, i.e. the metadata block is appended rather than merged into them.
expect_equal(colnames(meta_out3)[1:2], c("mz", "intensity"))

## ---------------------------------------------------------------------------
## Intensity aggregation and m/z aggregation functions
## ---------------------------------------------------------------------------

## Default intensityFun = max keeps the apex intensity.
int_max <- combine(pk(c(100, 100.001), c(3, 10)), tolerance = 0.05, ppm = 0)
expect_equal(int_max[1, "intensity"], 10)

## intensityFun = mean averages the group intensities instead.
int_mean <- combine(
  pk(c(100, 100.001), c(3, 10)),
  tolerance = 0.05,
  ppm = 0,
  intensityFun = base::mean
)
expect_equal(int_mean[1, "intensity"], 6.5)

## weighted = FALSE with mzFun = median differs from the default mean.
mz_median <- combine(
  pk(c(100, 100.001, 100.002), c(1, 10, 20)),
  tolerance = 0.05,
  ppm = 0,
  mzFun = stats::median
)
expect_equal(nrow(mz_median), 1L)
expect_equal(mz_median[1, "mz"], 100.001)

## ---------------------------------------------------------------------------
## Intensity weighting shifts the centroid m/z toward the dominant point
## ---------------------------------------------------------------------------

w1 <- combine(
  pk(c(100, 110), c(10, 1)),
  tolerance = 15,
  ppm = 0,
  weighted = TRUE,
  intensity_exponent = 1
)
w7 <- combine(
  pk(c(100, 110), c(10, 1)),
  tolerance = 15,
  ppm = 0,
  weighted = TRUE,
  intensity_exponent = 7
)
expect_equal(nrow(w1), 1L)
expect_equal(nrow(w7), 1L)
expect_equal(w1[1, "intensity"], 10)
expect_equal(w7[1, "intensity"], 10)

## exponent 1 weights by intensity: weighted.mean(c(100,110), c(10,1))
expect_equal(w1[1, "mz"], stats::weighted.mean(c(100, 110), c(10, 1)))
## exponent 7 weights by intensity^7, pulling the centroid much closer to 100.
expect_equal(w7[1, "mz"], stats::weighted.mean(c(100, 110), c(10, 1)^7))
expect_true(w7[1, "mz"] < w1[1, "mz"])
expect_true(w1[1, "mz"] > 100)
expect_true(w7[1, "mz"] < 100.001)
