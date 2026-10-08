## Shared test helper, not a test file: tinytest only sources files matching
## "^test.*\\.[rR]$", so this is not collected as a test.
##
## setup_logger() (called by .process_spectra_batches) installs a logger
## appender that writes into the output directory, and it is never removed.
## Once a test deletes that directory, any later logger call in the session dies
## with "cannot open the connection", which aborts the whole run rather than
## failing one expectation.
##
## Sourcing this file therefore repairs the logger immediately, so any test file
## that includes it is protected even if an earlier one left a stale appender
## behind. centring the reset on load rather than on individual calls means the
## protection cannot be forgotten.

centroidr_reset_logging <- function() {
  ## The appender is passed as a function value, not called: log_appender() takes
  ## the appender itself. Calling log_appender() with no argument only prints the
  ## current appender and leaves a broken one in place, and calling
  ## appender_stdout() directly fails because the appenders take a `lines`
  ## argument.
  logger::log_appender(logger::appender_stdout)
  logger::log_threshold(logger::INFO)
  invisible(NULL)
}

centroidr_reset_logging()
