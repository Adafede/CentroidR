#' CentroidR Package
#'
#' Internal package-level definitions and startup hooks for CentroidR.
#' @docType package
#' @keywords internal
"_PACKAGE"

# usethis namespace: start
# usethis namespace: end
NULL

# Set data.table awareness for compatibility
.datatable.aware <- TRUE

#' .onLoad: Package load hook
#'
#' Ensures required S4 classes and external dependencies are registered on load.
#' @noRd
.onLoad <- function(libname, pkgname) {
  # To avoid CRAN warnings for unregistered S4 classes
  optparse::.__C__OptionParser
  mzR::pwiz.version()
  invisible()
}

#' .onAttach: Package attach hook
#'
#' Displays a startup message and citation info when the package is attached.
#' @noRd
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to ", pkgname)
  packageStartupMessage(format(utils::citation(pkgname)))
}
