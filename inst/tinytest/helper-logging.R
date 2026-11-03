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

centroidr_reset_logging()
