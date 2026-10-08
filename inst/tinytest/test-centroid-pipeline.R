library(tinytest)

.process_spectra <- getFromNamespace(".process_spectra", "CentroidR")
.fix_xml <- getFromNamespace(".fix_xml", "CentroidR")

source("helper-logging.R")

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
centroidr_reset_logging()

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

## ---------------------------------------------------------------------------
## The exported output really is passed through .fix_xml
## ---------------------------------------------------------------------------
## centroid_one_file() rewrites the exported mzML with .fix_xml(), replacing the
## placeholder run id with the output file name. The existing test only checks
## .fix_xml() in isolation, which cannot see whether the pipeline calls it.

.fix_xml <- getFromNamespace(".fix_xml", "CentroidR")

fix_infile <- tempfile(pattern = "profile_", fileext = ".mzML")
Spectra::export(sp, file = fix_infile, backend = Spectra::MsBackendMzR())
fix_outfile <- sub("profile_", "centroided_", fix_infile, fixed = TRUE)
expect_equal(
  CentroidR::centroid_one_file(
    file = fix_infile,
    pattern = "profile_",
    replacement = "centroided_"
  ),
  TRUE
)
centroidr_reset_logging()
expect_true(file.exists(fix_outfile))
fixed_lines <- readLines(fix_outfile, warn = FALSE)

## The run id now carries the output file name rather than the placeholder.
expect_true(any(grepl(basename(fix_outfile), fixed_lines, fixed = TRUE)))
expect_false(any(grepl("<run id=\"Experiment_1\"", fixed_lines, fixed = TRUE)))

## The rewritten file is still readable by the mzML backend.
expect_equal(length(Spectra::Spectra(fix_outfile, backend = Spectra::MsBackendMzR())), 3L)

unlink(c(fix_infile, fix_outfile, file.path(dirname(fix_infile), "centroiding.log")))

## ---------------------------------------------------------------------------
## .fix_xml leaves no temporary file behind
## ---------------------------------------------------------------------------

tf <- tempfile(fileext = ".mzML")
writeLines(
  c('<?xml version="1.0" encoding="UTF-8"?>', '<mzML>', '<run id="Experiment_1"/>', '</mzML>'),
  tf
)
before <- length(list.files(tempdir(), pattern = "^file"))
.fix_xml(tf)
after <- length(list.files(tempdir(), pattern = "^file"))
expect_equal(after, before, info = ".fix_xml should remove its temporary file")
expect_true(file.exists(tf), info = ".fix_xml should keep the file it rewrote")
unlink(tf)
