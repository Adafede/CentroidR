## Shared test helper, not a test file: tinytest only sources files matching
## "^test.*\\.[rR]$", so this is not collected as a test.
##
## setup_logger() (called by .process_spectra_batches) installs a logger
## appender that writes into the output directory, and it is never removed. Once
## a test deletes that directory, every later logger call in the session fails
## with "cannot open the connection". Each test that triggers centroiding must
## therefore reset the appender while its output directory still exists.

centroidr_reset_logging <- function() {
  ## The appender is passed as a function value, not called: log_appender() takes
  ## the appender itself. Calling log_appender() with no argument only prints the
  ## current appender and leaves a broken one in place.
  logger::log_appender(logger::appender_stdout)
  logger::log_threshold(logger::INFO)
  invisible(NULL)
}