## Shared test helper, not a test file: tinytest only sources files matching
## "^test.*\\.[rR]$", so this is not collected as a test.
##
## Tests call .process_spectra_batches() directly, which installs a logger
## appender into the output directory that the test then deletes. centroid_one_file()
## hands the logger back when it returns, but a direct call to the internal
## function has no such restore, so the appender can outlive its directory.
##
## The log appender itself no longer fails the session when that happens: it
## degrades to the console and warns once. This helper resets the logger anyway,
## so a test starts from the console appender at INFO rather than inheriting
## whatever an earlier file left behind, which keeps the tests independent of
## ordering and keeps them quiet.

centroidr_reset_logging <- function() {
  getFromNamespace("restore_logger", "CentroidR")()
}

make_test_spectra <- function(
  ms_level,
  mz_list,
  int_list,
  rtime = as.numeric(seq_along(ms_level)),
  polarity = 0L,
  ...
) {
  n <- length(ms_level)
  Spectra::Spectra(data.frame(
    msLevel = as.integer(ms_level),
    polarity = rep_len(as.integer(polarity), n),
    rtime = rtime,
    ...,
    mz = I(mz_list),
    intensity = I(int_list)
  ))
}

write_test_mzml <- function(sp, path) {
  Spectra::export(sp, file = path, backend = Spectra::MsBackendMzR())
  path
}

run_process_spectra <- function(
  spectra,
  mz_tol_da_ms1 = 0.01,
  mz_tol_da_ms2 = 0.01,
  mz_tol_ppm_ms1 = 5,
  mz_tol_ppm_ms2 = 5,
  custom_int_fun_ms1 = base::max,
  custom_int_fun_ms2 = base::max,
  mz_fun_ms1 = base::mean,
  mz_fun_ms2 = base::mean,
  mz_weighted = TRUE,
  time_domain = TRUE
) {
  getFromNamespace(".process_spectra", "CentroidR")(
    spectra = spectra,
    mz_tol_da_ms1 = mz_tol_da_ms1,
    mz_tol_da_ms2 = mz_tol_da_ms2,
    mz_tol_ppm_ms1 = mz_tol_ppm_ms1,
    mz_tol_ppm_ms2 = mz_tol_ppm_ms2,
    custom_int_fun_ms1 = custom_int_fun_ms1,
    custom_int_fun_ms2 = custom_int_fun_ms2,
    mz_fun_ms1 = mz_fun_ms1,
    mz_fun_ms2 = mz_fun_ms2,
    mz_weighted = mz_weighted,
    time_domain = time_domain
  )
}

run_process_spectra_batches <- function(
  spectra,
  outf,
  outd = dirname(outf),
  batch_size = 2L,
  min_datapoints_ms1 = 1L,
  min_datapoints_ms2 = 1L,
  mz_tol_da_ms1 = 0.01,
  mz_tol_da_ms2 = 0.01,
  mz_tol_ppm_ms1 = 0,
  mz_tol_ppm_ms2 = 0,
  mz_fun_ms1 = base::mean,
  mz_fun_ms2 = base::mean,
  int_fun_ms1 = base::max,
  int_fun_ms2 = base::max,
  mz_weighted = TRUE,
  time_domain = FALSE,
  intensity_exponent = 3
) {
  getFromNamespace(".process_spectra_batches", "CentroidR")(
    spectra = spectra,
    outf = outf,
    outd = outd,
    min_datapoints_ms1 = min_datapoints_ms1,
    min_datapoints_ms2 = min_datapoints_ms2,
    mz_tol_da_ms1 = mz_tol_da_ms1,
    mz_tol_da_ms2 = mz_tol_da_ms2,
    mz_tol_ppm_ms1 = mz_tol_ppm_ms1,
    mz_tol_ppm_ms2 = mz_tol_ppm_ms2,
    mz_fun_ms1 = mz_fun_ms1,
    mz_fun_ms2 = mz_fun_ms2,
    int_fun_ms1 = int_fun_ms1,
    int_fun_ms2 = int_fun_ms2,
    mz_weighted = mz_weighted,
    time_domain = time_domain,
    intensity_exponent = intensity_exponent,
    batch_size = batch_size
  )
}

centroid_temp_spectra <- function(
  sp,
  ...,
  tag = "ct_",
  pattern = "profile_",
  replacement = "centroided_"
) {
  d <- tempfile(tag)
  dir.create(d)
  infile <- write_test_mzml(sp, file.path(d, "profile_in.mzML"))
  outfile <- sub(pattern, replacement, infile, fixed = TRUE)
  ok <- CentroidR::centroid_one_file(
    file = infile,
    pattern = pattern,
    replacement = replacement,
    ...
  )
  centroidr_reset_logging()
  list(dir = d, infile = infile, outfile = outfile, ok = ok)
}

centroidr_reset_logging()
