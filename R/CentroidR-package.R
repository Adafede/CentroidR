#' CentroidR: Centroid Profile Mass Spectrometry Data
#'
#' Infrastructure to centroid profile-mode mass spectrometry (`mzML`) files
#' into peak-picked centroid spectra with configurable MS1 and MS2 tolerances,
#' local-valley splitting of closely spaced peaks, and intensity-weighted m/z
#' aggregation.
#'
#' @section EDAM Ontology Annotations:
#' - **Topics**: `EDAM:topic_3172` (Metabolomics), `EDAM:topic_3370`
#'   (Analytical chemistry), `EDAM:topic_0154` (Small molecules)
#' - **Operations**: `EDAM:operation_3215` (Peak picking / Centroiding),
#'   `EDAM:operation_3695` (Filtering)
#' - **Data**: `EDAM:data_0943` (Mass spectrometry spectra),
#'   `EDAM:data_2536` (Mass spectrometry data)
#' - **Formats**: `EDAM:format_3244` (mzML)
#'
#' @concept EDAM:topic_3172
#' @concept EDAM:topic_3370
#' @concept EDAM:topic_0154
#' @concept EDAM:operation_3215
#' @concept EDAM:data_0943
#' @concept EDAM:format_3244
#' @docType package
#' @keywords internal
"_PACKAGE"

# usethis namespace: start
# usethis namespace: end
NULL

#' .onLoad: Package load hook
#'
#' Ensures required external dependencies are registered on load.
#' @noRd
.onLoad <- function(libname, pkgname) {
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
