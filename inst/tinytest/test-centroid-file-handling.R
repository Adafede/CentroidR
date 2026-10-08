## Tests for the file handling and argument validation of centroid_one_file():
## the paths that decide where output goes, what happens when the input cannot
## be read, and which argument types are rejected.
##
## The two error paths matter scientifically because centroid_one_file() reports
## failure by returning FALSE. A run that silently returned TRUE without writing
## an output file would look like success, so both the return value and the
## absence of output are asserted.

library(tinytest)

source("helper-logging.R")

mk_input <- function(dir, name = "profile_in", n = 1) {
  infile <- file.path(dir, paste0(name, ".mzML"))
  Spectra::export(
    Spectra::Spectra(data.frame(
      msLevel = rep(1L, n), polarity = 0L, rtime = seq_len(n),
      mz = I(lapply(seq_len(n), function(i) c(100 * i, 100 * i + 0.0005))),
      intensity = I(lapply(seq_len(n), function(i) c(10, 50)))
    )),
    file = infile,
    backend = Spectra::MsBackendMzR()
  )
  infile
}

## ---------------------------------------------------------------------------
## The output directory is created when it does not exist
## ---------------------------------------------------------------------------

dir1 <- tempfile("outdir_")
dir.create(dir1)
infile <- mk_input(dir1)
expect_equal(
  CentroidR::centroid_one_file(
    file = infile,
    pattern = "profile_",
    replacement = "nested/deeper/centroided_"
  ),
  TRUE
)
centroidr_reset_logging()
nested <- file.path(dir1, "nested", "deeper")
expect_true(dir.exists(nested))
expect_true(file.exists(file.path(nested, "centroided_in.mzML")))
unlink(dir1, recursive = TRUE)

## An existing output directory is reused rather than failing.
dir2 <- tempfile("outdir2_")
dir.create(dir2)
infile2 <- mk_input(dir2)
expect_equal(
  CentroidR::centroid_one_file(
    file = infile2,
    pattern = "profile_",
    replacement = "centroided_"
  ),
  TRUE
)
centroidr_reset_logging()
expect_true(file.exists(file.path(dir2, "centroided_in.mzML")))
unlink(dir2, recursive = TRUE)

## ---------------------------------------------------------------------------
## An unreadable input is reported as failure and writes no output
## ---------------------------------------------------------------------------

dir3 <- tempfile("baddir_")
dir.create(dir3)

## A file that exists but is not mzML.
not_mzml <- file.path(dir3, "profile_notmzml.mzML")
writeLines(c("this is not mzML", "<nope/>"), not_mzml)
expect_equal(
  CentroidR::centroid_one_file(
    file = not_mzml,
    pattern = "profile_",
    replacement = "centroided_"
  ),
  FALSE
)
centroidr_reset_logging()
expect_false(file.exists(file.path(dir3, "centroided_notmzml.mzML")))

## A zero byte file.
empty_file <- file.path(dir3, "profile_empty.mzML")
file.create(empty_file)
expect_equal(
  CentroidR::centroid_one_file(
    file = empty_file,
    pattern = "profile_",
    replacement = "centroided_"
  ),
  FALSE
)
centroidr_reset_logging()
expect_false(file.exists(file.path(dir3, "centroided_empty.mzML")))

## A path that does not exist at all.
expect_equal(
  CentroidR::centroid_one_file(
    file = file.path(dir3, "profile_absent.mzML"),
    pattern = "profile_",
    replacement = "centroided_"
  ),
  FALSE
)
centroidr_reset_logging()
expect_false(file.exists(file.path(dir3, "centroided_absent.mzML")))

unlink(dir3, recursive = TRUE)

## ---------------------------------------------------------------------------
## Argument validation
## ---------------------------------------------------------------------------
## The scientific parameters are checked up front, so a wrong type is an error
## rather than a run that silently uses a default.

valid <- mk_input(tempdir(), "profile_valid")

expect_error(CentroidR::centroid_one_file(file = 1, pattern = "profile_", replacement = "c"))
expect_error(CentroidR::centroid_one_file(file = valid, pattern = 1, replacement = "c"))
expect_error(CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = 1))
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    mz_tol_da_ms1 = "wide")
)
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    mz_tol_ppm_ms1 = "wide")
)
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    mz_fun_ms1 = "not a function")
)
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    mz_fun_ms2 = "not a function")
)
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    int_fun_ms1 = "not a function")
)
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    int_fun_ms2 = "not a function")
)
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    mz_weighted = "yes")
)
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    time_domain = "yes")
)
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    intensity_exponent = "three")
)

## The logical and numeric flags must be length one.
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    mz_weighted = c(TRUE, FALSE))
)
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    time_domain = c(TRUE, FALSE))
)
expect_error(
  CentroidR::centroid_one_file(file = valid, pattern = "profile_", replacement = "c",
    intensity_exponent = c(1, 2))
)

unlink(c(valid, sub("profile_", "centroided_", valid, fixed = TRUE)))

## ---------------------------------------------------------------------------
## pattern and replacement drive the output path
## ---------------------------------------------------------------------------

dir4 <- tempfile("patdir_")
dir.create(dir4)
## sub() replaces only the matched part, so "profile_raw.mzML" becomes
## "profile_centroided.mzML".
infile4 <- mk_input(dir4, "profile_raw")
renamed <- CentroidR::centroid_one_file(
  file = infile4,
  pattern = "raw",
  replacement = "centroided"
)
centroidr_reset_logging()
expect_equal(renamed, TRUE)
expect_true(file.exists(file.path(dir4, "profile_centroided.mzML")))
expect_true(file.exists(infile4))
expect_equal(basename(infile4), "profile_raw.mzML")
unlink(dir4, recursive = TRUE)