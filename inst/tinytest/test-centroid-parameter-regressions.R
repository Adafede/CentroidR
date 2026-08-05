library(tinytest)

make_profile_mzml <- function(mz, intensity) {
  spd <- data.frame(
    msLevel = 1L,
    polarity = 0L,
    rtime = 1,
    mz = I(list(mz)),
    intensity = I(list(intensity))
  )
  infile <- tempfile(pattern = "profile_", fileext = ".mzML")
  Spectra::export(
    Spectra::Spectra(spd),
    file = infile,
    backend = Spectra::MsBackendMzR()
  )
  infile
}

read_centroid_peaks <- function(file) {
  Spectra::peaksData(Spectra::Spectra(
    file,
    backend = Spectra::MsBackendMzR()
  ))[[1]]
}

centroid_profile <- function(
  mz,
  intensity,
  mz_tol_da_ms1 = 2.5,
  mz_tol_da_ms2 = mz_tol_da_ms1,
  mz_tol_ppm_ms1 = 0,
  mz_tol_ppm_ms2 = mz_tol_ppm_ms1,
  ...
) {
  infile <- make_profile_mzml(mz, intensity)
  outfile <- sub("profile_", "centroided_", infile, fixed = TRUE)
  on.exit(
    {
      unlink(infile)
      unlink(outfile)
    },
    add = TRUE
  )

  expect_true(
    CentroidR::centroid_one_file(
      file = infile,
      pattern = "profile_",
      replacement = "centroided_",
      min_datapoints_ms1 = 1L,
      min_datapoints_ms2 = 1L,
      mz_tol_da_ms1 = mz_tol_da_ms1,
      mz_tol_da_ms2 = mz_tol_da_ms2,
      mz_tol_ppm_ms1 = mz_tol_ppm_ms1,
      mz_tol_ppm_ms2 = mz_tol_ppm_ms2,
      ...
    )
  )

  read_centroid_peaks(outfile)
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
