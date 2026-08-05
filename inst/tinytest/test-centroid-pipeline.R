library(tinytest)

.process_spectra <- getFromNamespace(".process_spectra", "CentroidR")
.fix_xml <- getFromNamespace(".fix_xml", "CentroidR")

make_test_spectra <- function() {
  spd <- data.frame(
    msLevel = c(1L, 2L, 1L),
    polarity = c(0L, 0L, 0L),
    rtime = c(1, 2, 3),
    mz = I(list(
      c(100, 100.0004, 100.0008),
      c(200, 200.0004, 200.0008),
      numeric(0)
    )),
    intensity = I(list(
      c(10, 50, 10),
      c(5, 25, 5),
      numeric(0)
    ))
  )
  Spectra::Spectra(spd)
}

custom_int <- function(intensities) {
  if (length(intensities)) max(intensities) else 0
}

sp <- make_test_spectra()
processed <- .process_spectra(
  spectra = sp,
  mz_tol_da_ms1 = 0.01,
  mz_tol_da_ms2 = 0.01,
  mz_tol_ppm_ms1 = 5,
  mz_tol_ppm_ms2 = 5,
  custom_int_fun_ms1 = custom_int,
  custom_int_fun_ms2 = custom_int,
  mz_fun_ms1 = base::mean,
  mz_fun_ms2 = base::mean,
  mz_weighted = TRUE,
  time_domain = FALSE
)

expect_equal(length(processed), 3L)

outf <- tempfile(pattern = "profile_", fileext = ".mzML")
infile <- tempfile(pattern = "profile_", fileext = ".mzML")
Spectra::export(sp, file = infile, backend = Spectra::MsBackendMzR())

expect_equal(
  CentroidR::centroid_one_file(
    file = infile,
    pattern = "profile_",
    replacement = "centroided_"
  ),
  TRUE
)

outf <- sub("profile_", "centroided_", infile, fixed = TRUE)
expect_true(file.exists(outf))

sp_out <- Spectra::Spectra(outf, backend = Spectra::MsBackendMzR())
expect_equal(length(sp_out), 3L)

xml <- tempfile(fileext = ".mzML")
writeLines(
  c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<mzML>',
    '<run id="Experiment_1"><spectrum value="nan"/></run>',
    '</mzML>'
  ),
  xml
)
.fix_xml(xml)
fixed <- readLines(xml, warn = FALSE)
expect_true(any(grepl('value="NaN"', fixed, fixed = TRUE)))
expect_true(any(grepl(basename(xml), fixed, fixed = TRUE)))
