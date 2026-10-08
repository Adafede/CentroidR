## Tests for setup_logger() and the log output of centroid_one_file().
##
## Centroiding writes a provenance log next to its output file. The log is the
## only record of which tolerances, weighting and aggregation functions were in
## effect for a given output file, so the entries that document the run must
## actually be emitted and must record the real parameter values.
##
## Two design notes:
##
## - Messages are asserted by substring, on the log file, which is what
##   centroid_one_file() actually produces. Whether logger renders a level and
##   timestamp prefix on each line is logger's own formatting contract, differs
##   between logger versions and configurations, and is not something
##   CentroidR controls, so nothing here depends on it.
## - setup_logger() installs a logger appender for the output directory and
##   nothing ever removes it, so the logger is reset before each run. Otherwise a
##   stale appender pointing at a directory a previous test deleted would break
##   every later log call in the session.

library(tinytest)

source("helper-logging.R")

## TRUE if any captured line contains `needle`.
logged <- function(log, needle) {
  any(grepl(needle, log, fixed = TRUE))
}

## Read a log file, tolerating that it does not exist.
read_log <- function(dir, filename = "centroiding.log") {
  f <- file.path(dir, filename)
  if (!file.exists(f)) {
    return(character())
  }
  readLines(f, warn = FALSE)
}

setup_logger <- getFromNamespace("setup_logger", "CentroidR")

## ---------------------------------------------------------------------------
## setup_logger writes to the requested directory and file name
## ---------------------------------------------------------------------------

log_dir <- tempfile("logdir_")
dir.create(log_dir)
setup_logger(dir = log_dir)
logger::log_info("marker after setup_logger")
setup_log <- read_log(log_dir)
centroidr_reset_logging()

## The default log file name is centroiding.log and it receives the message.
expect_true(file.exists(file.path(log_dir, "centroiding.log")))
expect_true(logged(setup_log, "marker after setup_logger"))

## setup_logger raises the threshold to TRACE, so TRACE level entries reach the
## log. Without that, the per-batch and per-step entries would be dropped.
trace_dir <- tempfile("logdir_trace_")
dir.create(trace_dir)
setup_logger(dir = trace_dir)
logger::log_info("marker at info level")
logger::log_trace("marker at trace level")
trace_log <- read_log(trace_dir)
centroidr_reset_logging()

expect_true(logged(trace_log, "marker at info level"))
expect_true(
  logged(trace_log, "marker at trace level"),
  info = "setup_logger should raise the threshold to TRACE"
)

## A custom file name is honoured and the default name is not used.
alt_dir <- tempfile("logdir2_")
dir.create(alt_dir)
setup_logger(dir = alt_dir, filename = "custom.log")
logger::log_info("marker in custom log")
alt_log <- read_log(alt_dir, "custom.log")
centroidr_reset_logging()
expect_true(file.exists(file.path(alt_dir, "custom.log")))
expect_false(file.exists(file.path(alt_dir, "centroiding.log")))
expect_true(logged(alt_log, "marker in custom log"))

unlink(c(log_dir, trace_dir, alt_dir), recursive = TRUE)

## ---------------------------------------------------------------------------
## A full run records the parameters it actually used
## ---------------------------------------------------------------------------

## Each run gets its own directory. setup_logger() installs an appending file
## appender, so runs sharing one directory would append to the same
## centroiding.log and each run would see the previous run's messages.
write_profile <- function(tag, dir = tempfile(paste0("logrun_", tag, "_"))) {
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  infile <- file.path(dir, paste0("profile_", tag, ".mzML"))
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
  ## Clear any appender left pointing at a directory a previous test removed.
  centroidr_reset_logging()
  infile <- write_profile(tag)
  outfile <- sub("profile_", "centroided_", infile, fixed = TRUE)
  value <- CentroidR::centroid_one_file(
    file = infile,
    pattern = "profile_",
    replacement = "centroided_",
    ...
  )
  log <- read_log(dirname(infile))
  centroidr_reset_logging()
  list(
    value = value,
    log = log,
    outfile = outfile,
    infile = infile,
    dir = dirname(infile)
  )
}

cleanup <- function(run) {
  unlink(run$dir, recursive = TRUE)
}

run <- run_and_read_log("logdefaults")

## The run succeeded and wrote a log file next to its output.
expect_equal(run$value, TRUE)
expect_true(
  file.exists(file.path(run$dir, "centroiding.log")),
  info = "a log file should be written"
)
expect_true(logged(run$log, "Processing mzML file"))

## The input file being processed is named.
expect_true(logged(run$log, basename(run$infile)))

## Every documented parameter is logged with its value. These are the values
## that make the log a usable provenance record, so a silent omission matters.
expect_true(logged(run$log, "min datapoints MS1 : 5"))
expect_true(logged(run$log, "min datapoints MS2 : 1"))
expect_true(logged(run$log, "m/z tolerance (Da, MS1) : 0.0025"))
expect_true(logged(run$log, "m/z tolerance (Da, MS2) : 0.0025"))
expect_true(logged(run$log, "m/z tolerance (ppm, MS1) : 5"))
expect_true(logged(run$log, "m/z tolerance (ppm, MS2) : 5"))
expect_true(logged(run$log, "m/z weighted : TRUE"))
expect_true(logged(run$log, "Time domain : TRUE"))
expect_true(logged(run$log, "Intensity exponent : 3"))

## The aggregation functions are recorded too.
expect_true(logged(run$log, "m/z function (MS1)"))
expect_true(logged(run$log, "m/z function (MS2)"))
expect_true(logged(run$log, "Intensity function (MS1)"))
expect_true(logged(run$log, "Intensity function (MS2)"))

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
expect_equal(custom$value, TRUE)
expect_true(logged(custom$log, "min datapoints MS1 : 7"))
expect_true(logged(custom$log, "m/z tolerance (Da, MS1) : 0.125"))
expect_true(logged(custom$log, "m/z tolerance (ppm, MS1) : 42"))
expect_true(logged(custom$log, "m/z weighted : FALSE"))
expect_true(logged(custom$log, "Time domain : FALSE"))
expect_true(logged(custom$log, "Intensity exponent : 5"))

## The parameter block reflects this run rather than the defaults.
expect_false(logged(custom$log, "min datapoints MS1 : 5"))

## ---------------------------------------------------------------------------
## The run narrates its stages and its completion
## ---------------------------------------------------------------------------
## These are logged at TRACE, so their presence also shows the threshold is high
## enough for the whole run to be recorded.

expect_true(logged(run$log, "Processing batch 1 / 1"))
expect_true(logged(run$log, "Concatenating all processed batches"))
expect_true(logged(run$log, "Exporting: "))
expect_true(logged(run$log, "Exported: "))
expect_true(logged(run$log, "Making a few fixes inside mzML: "))
expect_true(logged(run$log, "Made fixes inside mzML: "))
expect_true(logged(run$log, "Successfully centroided: "))

cleanup(run)
cleanup(custom)

## ---------------------------------------------------------------------------
## The two branches that return before an output directory exists
## ---------------------------------------------------------------------------
## These abort before setup_logger() can create a log file, so the messages are
## checked through a capturing appender instead.

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

centroidr_reset_logging()
skip_dir <- tempfile("logskip_")
dir.create(skip_dir)
skip_infile <- write_profile("logskip", dir = skip_dir)
skip_outfile <- sub("profile_", "centroided_", skip_infile, fixed = TRUE)
## No top level on.exit() here: in tinytest's evaluation context it can fire
## before the assertions below run.
file.create(skip_outfile)

skip <- capture_log(CentroidR::centroid_one_file(
  file = skip_infile,
  pattern = "profile_",
  replacement = "centroided_"
))
centroidr_reset_logging()

expect_equal(skip$value, TRUE)
expect_true(logged(skip$log, "Skipping. Output file already exists"))
expect_false(logged(skip$log, "Successfully centroided"))

unlink(skip_dir, recursive = TRUE)
skip_dir2 <- tempfile("logmissing_")
dir.create(skip_dir2)

## ---------------------------------------------------------------------------
## A missing input file is reported and returns FALSE
## ---------------------------------------------------------------------------

centroidr_reset_logging()
missing <- capture_log(CentroidR::centroid_one_file(
  file = file.path(skip_dir2, "does_not_exist.mzML"),
  pattern = "profile_",
  replacement = "centroided_"
))
centroidr_reset_logging()

expect_equal(missing$value, FALSE)
expect_true(logged(missing$log, "Input file does not exist"))
expect_true(logged(missing$log, "does_not_exist.mzML"))
unlink(skip_dir2, recursive = TRUE)
