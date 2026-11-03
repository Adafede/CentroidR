#' CentroidR: Centroid Profile Mass Spectrometry Data
#'
#' Infrastructure to centroid profile-mode mass spectrometry (`mzML`) files
#' into peak-picked centroid spectra with configurable MS1 and MS2 tolerances,
#' local-valley splitting of closely spaced peaks, and intensity-weighted m/z
#' aggregation.
#'
#' @section EDAM Ontology Annotations:
#' - **Topics**:
#'   - `EDAM:topic_3172` — Metabolomics (`http://edamontology.org/topic_3172`)
#'   - `EDAM:topic_0153` — Lipids / Lipidomics (`http://edamontology.org/topic_0153`)
#'   - `EDAM:topic_0154` — Small molecules (`http://edamontology.org/topic_0154`)
#'   - `EDAM:topic_3370` — Analytical chemistry (`http://edamontology.org/topic_3370`)
#' - **Operations**:
#'   - `EDAM:operation_3215` — Peak detection / Peak picking / Centroiding (`http://edamontology.org/operation_3215`)
#'   - `EDAM:operation_3214` — Spectral analysis (`http://edamontology.org/operation_3214`)
#'   - `EDAM:operation_3695` — Data filtering (`http://edamontology.org/operation_3695`)
#' - **Data**:
#'   - `EDAM:data_0943` — Mass spectrum (`http://edamontology.org/data_0943`)
#'   - `EDAM:data_2536` — Mass spectrometry data (`http://edamontology.org/data_2536`)
#' - **Formats**:
#'   - `EDAM:format_3244` — mzML (`http://edamontology.org/format_3244`)
#'
#' @concept EDAM:topic_3172
#' @concept EDAM:topic_0153
#' @concept EDAM:topic_0154
#' @concept EDAM:topic_3370
#' @concept EDAM:operation_3215
#' @concept EDAM:operation_3214
#' @concept EDAM:operation_3695
#' @concept EDAM:data_0943
#' @concept EDAM:data_2536
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
