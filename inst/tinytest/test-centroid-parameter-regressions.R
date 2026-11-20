library(tinytest)

## Repairs the global logger appender that setup_logger() installs and never
## removes; see helper-logging.R.
source(
  if (file.exists("helper-logging.R")) {
    "helper-logging.R"
  } else {
    "inst/tinytest/helper-logging.R"
  }
)

centroid_profile <- function(
  mz,
  intensity,
  mz_tol_da_ms1 = 2.5,
  mz_tol_da_ms2 = mz_tol_da_ms1,
  mz_tol_ppm_ms1 = 0,
  mz_tol_ppm_ms2 = mz_tol_ppm_ms1,
  ...
) {
  run <- centroid_temp_spectra(
    make_test_spectra(1L, list(mz), list(intensity)),
    min_datapoints_ms1 = 1L,
    min_datapoints_ms2 = 1L,
    mz_tol_da_ms1 = mz_tol_da_ms1,
    mz_tol_da_ms2 = mz_tol_da_ms2,
    mz_tol_ppm_ms1 = mz_tol_ppm_ms1,
    mz_tol_ppm_ms2 = mz_tol_ppm_ms2,
    ...
  )
  on.exit(unlink(run$dir, recursive = TRUE), add = TRUE)
  expect_true(run$ok)
  Spectra::peaksData(Spectra::Spectra(
    run$outfile,
    backend = Spectra::MsBackendMzR()
  ))[[1]]
}

peaks_exp1 <- centroid_profile(
  mz = c(100, 110),
  intensity = c(10, 1),
  intensity_exponent = 1,
  time_domain = FALSE,
  mz_tol_da_ms1 = 15,
  mz_tol_da_ms2 = 15
)

peaks_exp7 <- centroid_profile(
  mz = c(100, 110),
  intensity = c(10, 1),
  intensity_exponent = 7,
  time_domain = FALSE,
  mz_tol_da_ms1 = 15,
  mz_tol_da_ms2 = 15
)

expect_equal(nrow(peaks_exp1), 1L)
expect_equal(nrow(peaks_exp7), 1L)
expect_true(
  abs(peaks_exp1[1, "mz"] - peaks_exp7[1, "mz"]) > 0.01,
  info = "intensity_exponent should change the centroid m/z"
)
expect_true(
  peaks_exp7[1, "mz"] < peaks_exp1[1, "mz"],
  info = "Higher intensity_exponent should pull the centroid toward the dominant point"
)

peaks_time_false <- centroid_profile(
  mz = c(100, 104),
  intensity = c(100, 1),
  time_domain = FALSE
)

peaks_time_true <- centroid_profile(
  mz = c(100, 104),
  intensity = c(100, 1),
  time_domain = TRUE
)

expect_equal(nrow(peaks_time_false), 2L)
expect_equal(nrow(peaks_time_true), 1L)
