## Tests for setup_logger() and the log output of centroid_one_file().
##
## Centroiding writes a provenance log next to its output file. The log is the
## only record of which tolerances, weighting and aggregation functions were in
## effect for a given output file, so the entries that document the run must
## actually be emitted and must record the real parameter values.
##
## These assertions pin the message text as well as its presence, so that
## rewording or dropping an entry is detected.

library(tinytest)

source("helper-logging.R")

setup_logger <- getFromNamespace("setup_logger", "CentroidR")

## ---------------------------------------------------------------------------
## setup_logger writes to the requested directory and file name
## ---------------------------------------------------------------------------

log_dir <- tempfile("logdir_")
dir.create(log_dir)

read_log <- function(dir, filename = "centroiding.log") {
  ## Reset the appender before anything can log into a directory we may delete.
  centroidr_reset_logging()
  f <- file.path(dir, filename)
  if (!file.exists(f)) {
    return(character())
  }
  readLines(f, warn = FALSE)
}

log_dir <- tempfile("logdir_")
dir.create(log_dir)
setup_logger(dir = log_dir)
logger::log_info("marker after setup_logger")
setup_log <- read_log(log_dir)
unlink(log_dir, recursive = TRUE)

expect_true(
  file.exists(file.path(log_dir, "centroiding.log")) || length(setup_log) > 0
)
expect_true(any(grepl("marker after setup_logger", setup_log, fixed = TRUE)))

## setup_logger raises the threshold to TRACE, so TRACE level entries reach the
## log. Without that, the per-batch and per-step entries would be dropped.
trace_dir <- tempfile("logdir_trace_")
dir.create(trace_dir)
setup_logger(dir = trace_dir)
logger::log_trace("marker at trace level")
trace_log <- read_log(trace_dir)
unlink(trace_dir, recursive = TRUE)
expect_true(any(grepl("marker at trace level", trace_log, fixed = TRUE)))
expect_true(any(grepl("TRACE", trace_log)))

## A custom file name is honoured.
alt_dir <- tempfile("logdir2_")
dir.create(alt_dir)
setup_logger(dir = alt_dir, filename = "custom.log")
logger::log_info("marker in custom log")
alt_log <- read_log(alt_dir, "custom.log")
centroidr_reset_logging()
expect_true(file.exists(file.path(alt_dir, "custom.log")))
expect_false(file.exists(file.path(alt_dir, "centroiding.log")))
unlink(alt_dir, recursive = TRUE)
expect_true(any(grepl("marker in custom log", alt_log, fixed = TRUE)))

## ---------------------------------------------------------------------------
## A full run records the parameters it actually used
## ---------------------------------------------------------------------------

write_profile <- function(tag) {
  infile <- tempfile(pattern = paste0("profile_", tag, "_"), fileext = ".mzML")
  Spectra::export(
    Spectra::Spectra(data.frame(
      msLevel = 1L,
      polarity = 0L,
      rtime = 1,
      mz = I(list(c(100, 100.0005))),
      intensity = I(list(c(10, 50)))
    )),
    file = infile,
    backend = Spectra::MsBackendMzR()
  )
  infile
}

run_and_read_log <- function(tag, ...) {
  infile <- write_profile(tag)
  outfile <- sub("profile_", "centroided_", infile, fixed = TRUE)
  logfile <- file.path(dirname(infile), "centroiding.log")
  on.exit(unlink(c(infile, outfile, logfile)), add = TRUE)
  ret <- CentroidR::centroid_one_file(
    file = infile,
    pattern = "profile_",
    replacement = "centroided_",
    ...
  )
  lines <- if (file.exists(logfile)) {
    readLines(logfile, warn = FALSE)
  } else {
    character()
  }
  centroidr_reset_logging()
  list(ret = ret, log = lines, outfile = outfile, infile = infile)
}

run <- run_and_read_log("logdefaults")
expect_equal(run$ret, TRUE)
expect_true(length(run$log) > 0L)

## The input file being processed is named.
expect_true(any(grepl("Processing mzML file", run$log, fixed = TRUE)))
expect_true(any(grepl(basename(run$infile), run$log, fixed = TRUE)))

## Every documented parameter is logged with its value. These are the values
## that make the log a usable provenance record, so a silent omission matters.
expect_true(any(grepl("min datapoints MS1 : 5", run$log, fixed = TRUE)))
expect_true(any(grepl("min datapoints MS2 : 1", run$log, fixed = TRUE)))
expect_true(any(grepl(
  "m/z tolerance (Da, MS1) : 0.0025",
  run$log,
  fixed = TRUE
)))
expect_true(any(grepl(
  "m/z tolerance (Da, MS2) : 0.0025",
  run$log,
  fixed = TRUE
)))
expect_true(any(grepl("m/z tolerance (ppm, MS1) : 5", run$log, fixed = TRUE)))
expect_true(any(grepl("m/z tolerance (ppm, MS2) : 5", run$log, fixed = TRUE)))
expect_true(any(grepl("m/z weighted : TRUE", run$log, fixed = TRUE)))
expect_true(any(grepl("Time domain : TRUE", run$log, fixed = TRUE)))
expect_true(any(grepl("Intensity exponent : 3", run$log, fixed = TRUE)))

## The aggregation functions are recorded too.
expect_true(any(grepl("m/z function (MS1)", run$log, fixed = TRUE)))
expect_true(any(grepl("m/z function (MS2)", run$log, fixed = TRUE)))
expect_true(any(grepl("Intensity function (MS1)", run$log, fixed = TRUE)))
expect_true(any(grepl("Intensity function (MS2)", run$log, fixed = TRUE)))

## ---------------------------------------------------------------------------
## The log reflects non-default parameters
## ---------------------------------------------------------------------------

custom <- run_and_read_log(
  "logcustom",
  min_datapoints_ms1 = 7L,
  mz_tol_da_ms1 = 0.125,
  mz_tol_ppm_ms1 = 42,
  mz_weighted = FALSE,
  time_domain = FALSE,
  intensity_exponent = 5
)
expect_equal(custom$ret, TRUE)
expect_true(any(grepl("min datapoints MS1 : 7", custom$log, fixed = TRUE)))
expect_true(any(grepl(
  "m/z tolerance (Da, MS1) : 0.125",
  custom$log,
  fixed = TRUE
)))
expect_true(any(grepl(
  "m/z tolerance (ppm, MS1) : 42",
  custom$log,
  fixed = TRUE
)))
expect_true(any(grepl("m/z weighted : FALSE", custom$log, fixed = TRUE)))
expect_true(any(grepl("Time domain : FALSE", custom$log, fixed = TRUE)))
expect_true(any(grepl("Intensity exponent : 5", custom$log, fixed = TRUE)))

## ---------------------------------------------------------------------------
## The run narrates its stages and its completion
## ---------------------------------------------------------------------------

expect_true(any(grepl("Processing batch 1 / 1", run$log, fixed = TRUE)))
expect_true(any(grepl(
  "Concatenating all processed batches",
  run$log,
  fixed = TRUE
)))
expect_true(any(grepl("Exporting: ", run$log, fixed = TRUE)))
expect_true(any(grepl("Exported: ", run$log, fixed = TRUE)))
expect_true(any(grepl(
  "Making a few fixes inside mzML: ",
  run$log,
  fixed = TRUE
)))
expect_true(any(grepl("Made fixes inside mzML: ", run$log, fixed = TRUE)))
expect_true(any(grepl("Successfully centroided: ", run$log, fixed = TRUE)))
expect_true(any(grepl("SUCCESS", run$log)))

## ---------------------------------------------------------------------------
## The early-return branches are captured through the logger
## ---------------------------------------------------------------------------
## Both branches below abort before any output directory is created, so there is
## no log file to inspect. Capture the logger output directly instead.

capture_log <- function(expr) {
  ## An environment is used rather than <<- , which would search the parent of
  ## the appender's own frame and so miss the local variable.
  sink <- new.env(parent = emptyenv())
  sink$lines <- character()
  logger::log_appender(function(lines) {
    sink$lines <- c(sink$lines, lines)
  })
  logger::log_threshold(logger::TRACE)
  on.exit(centroidr_reset_logging(), add = TRUE)
  value <- force(expr)
  list(value = value, log = sink$lines)
}

## ---------------------------------------------------------------------------
## An existing output file is reported and skipped, without redoing the work
## ---------------------------------------------------------------------------

skip_infile <- write_profile("logskip")
skip_outfile <- sub("profile_", "centroided_", skip_infile, fixed = TRUE)
skip_log <- file.path(dirname(skip_infile), "centroiding.log")
## No top level on.exit() here: in tinytest's evaluation context it fires
## immediately and would delete the file before it is used.
file.create(skip_outfile)

skip_run <- capture_log(
  CentroidR::centroid_one_file(
    file = skip_infile,
    pattern = "profile_",
    replacement = "centroided_"
  )
)
skip_ret <- skip_run$value
skip_lines <- skip_run$log

expect_equal(skip_ret, TRUE)
expect_true(any(grepl(
  "Skipping. Output file already exists",
  skip_lines,
  fixed = TRUE
)))
expect_false(any(grepl("Successfully centroided", skip_lines, fixed = TRUE)))
unlink(c(skip_infile, skip_outfile, skip_log))

## ---------------------------------------------------------------------------
## A missing input file is reported and returns FALSE
## ---------------------------------------------------------------------------

missing_log <- file.path(dirname(skip_infile), "centroiding.log")
missing_run <- capture_log(
  CentroidR::centroid_one_file(
    file = file.path(dirname(skip_infile), "does_not_exist.mzML"),
    pattern = "profile_",
    replacement = "centroided_"
  )
)
missing_ret <- missing_run$value
missing_lines <- missing_run$log

expect_equal(missing_ret, FALSE)
expect_true(any(grepl(
  "Input file does not exist",
  missing_lines,
  fixed = TRUE
)))
expect_true(any(grepl("does_not_exist.mzML", missing_lines, fixed = TRUE)))
expect_true(any(grepl("ERROR", missing_lines)))
