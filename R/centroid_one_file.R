#' Centroid an mzML file with configurable parameters
#'
#' This function processes an mzML file to apply centroiding with detailed controls
#' for peak picking, smoothing, and noise estimation. It allows fine-tuning of MS1 and MS2
#' peak detection, optimizing spectral data analysis for various experimental needs.
#'
#' @param file `character(1)`
#'   Path to the input mzML file. Must be a valid, accessible mzML format file.
#' @param pattern `character(1)`
#'   Regular expression pattern to match in the input file path, used for modifying the output file path.
#' @param replacement `character(1)`
#'   Replacement string for altering the output file path based on the `pattern`.
#' @param min_datapoints_ms1 `integer(1)` (default: `2L`)
#'   Minimum datapoints to be considered for MS1 data.
#' @param min_datapoints_ms2 `integer(1)` (default: `1L`)
#'   Minimum datapoints to be considered for MS2 data.
#' @param mz_tol_da_ms1 `numeric(1)` (default: `0.002`)
#'   m/z tolerance in Daltons for MS1 data.
#' @param mz_tol_da_ms2 `numeric(1)` (default: `0.005`)
#'   m/z tolerance in Daltons for MS2 data.
#' @param mz_tol_ppm_ms1 `numeric(1)` (default: `5`)
#'   m/z tolerance in parts per million (ppm) for MS1.
#' @param mz_tol_ppm_ms2 `numeric(1)` (default: `10`)
#'   m/z tolerance in parts per million (ppm) for MS2.
#' @param mz_fun `function` (default: `base::mean`)
#'   Function to aggregate m/z values within each peak group.
#'   Ignored if `mz_weighted = TRUE`.
#' @param int_fun `function` (default: `base::sum`)
#'   Function to aggregate peak intensities within each peak group.
#' @param mz_weighted `logical(1)` (default: `TRUE`)
#'   If `TRUE`, uses intensity-weighted mean for m/z value aggregation.
#'
#' @return `logical(1)`
#'   Returns `TRUE` if centroiding was successful, otherwise `FALSE`.
#'
#' @details
#' The function processes both MS1 and MS2 data with user-defined smoothing, peak-picking, and noise estimation.
#' File path modifications are supported via `pattern` and `replacement`.
#'
#' @export
#' @author
#'   Johannes Rainer, Adriano Rutz
centroid_one_file <- function(file,
                              pattern,
                              replacement,
                              min_datapoints_ms1 = 2L,
                              min_datapoints_ms2 = 1L,
                              mz_tol_da_ms1 = 0.002,
                              mz_tol_da_ms2 = 0.005,
                              mz_tol_ppm_ms1 = 5,
                              mz_tol_ppm_ms2 = 10,
                              mz_fun = base::mean,
                              int_fun = base::sum,
                              mz_weighted = TRUE) {
  # Construct output file path
  outf <- sub(
    pattern = pattern,
    replacement = replacement,
    x = file,
    fixed = TRUE
  )
  outd <- dirname(outf)

  # Initialize logger
  setup_logger(dir = outd)

  # Input validation
  if (!file.exists(file)) {
    logger::log_error("Input file does not exist: {file}")
    message("Error: File does not exist - ", file)
    return(FALSE)
  }

  if (file.exists(outf)) {
    message("Skipping: Output file already exists - ", outf)
    return(TRUE)
  }

  # Ensure output directory exists
  if (!dir.exists(outd)) {
    dir.create(path = outd, recursive = TRUE)
  }

  message("Processing mzML file: ", file)
  message("Output file: ", outf)

  # Logging parameter settings
  params <- list(
    "min datapoints MS1" = min_datapoints_ms1,
    "min datapoints MS2" = min_datapoints_ms2,
    "m/z tolerance (Da, MS1)" = mz_tol_da_ms1,
    "m/z tolerance (Da, MS2)" = mz_tol_da_ms2,
    "m/z tolerance (ppm, MS1)" = mz_tol_ppm_ms1,
    "m/z tolerance (ppm, MS2)" = mz_tol_ppm_ms2,
    "m/z function" = deparse(mz_fun),
    "Intensity function" = deparse(int_fun),
    "m/z weighted" = mz_weighted
  )
  lapply(
    X = names(params),
    FUN = function(param) {
      message(param, ": ", params[[param]])
    }
  )

  # Custom intensity aggregation functions
  custom_int_fun <- function(intensities, min_datapoints) {
    if (length(intensities[intensities > 0]) >= min_datapoints) {
      int_fun(intensities)
    } else {
      0
    }
  }
  custom_int_fun_ms1 <- function(intensities) {
    custom_int_fun(intensities, min_datapoints_ms1)
  }
  custom_int_fun_ms2 <- function(intensities) {
    custom_int_fun(intensities, min_datapoints_ms2)
  }

  # Processing with error handling
  tryCatch(
    {
      sp <- Spectra::Spectra(file, backend = Spectra::MsBackendMzR()) |>
        Spectra::setBackend(Spectra::MsBackendMemory())

      # Perform centroiding for MS1 and MS2 levels
      sp_cen <- sp |>
        Spectra::combinePeaks(
          tolerance = mz_tol_da_ms1,
          ppm = mz_tol_ppm_ms1,
          intensityFun = custom_int_fun_ms1,
          mzFun = mz_fun,
          weighted = mz_weighted,
          msLevel. = 1L
        ) |>
        Spectra::combinePeaks(
          tolerance = mz_tol_da_ms2,
          ppm = mz_tol_ppm_ms2,
          intensityFun = custom_int_fun_ms2,
          mzFun = mz_fun,
          weighted = mz_weighted,
          msLevel. = 2L
        ) |>
        Spectra::filterIntensity(intensity = c(.Machine$double.eps, Inf)) |>
        Spectra::applyProcessing()

      # COMMENT: Feels dirty but works
      # Mark spectra as centroided
      sp_cen@backend@spectraData$centroided <- TRUE

      # Export processed spectra
      sp_cen |>
        Spectra::export(file = outf, backend = Spectra::MsBackendMzR())

      # Update mzML metadata
      filename <- basename(outf)
      readLines(outf) |>
        gsub(
          pattern = "<run id=\"Experiment_1\"",
          replacement = paste0("<run id=\"", filename, "\"")
        ) |>
        writeLines(outf)

      logger::log_info("Successfully centroided: {basename(file)}")
      message("Centroiding completed for: ", file)
      return(TRUE)
    },
    error = function(e) {
      logger::log_error("Error processing {basename(file)}: {e$message}")
      message("Error processing file: ", file, " - ", e$message)
      return(FALSE)
    }
  )
}

#' Wrapper for centroiding with error handling
#'
#' @param file Path to the input mzML file.
#' @param pattern Pattern for modifying the file path.
#' @param replacement Replacement string for output path.
#' @param ... Additional parameters to pass to `centroid_one_file`.
#' @return Logical indicating success (`TRUE`) or failure (`FALSE`).
#' @keywords internal
try_centroid_one_file <- function(file, pattern, replacement, ...) {
  result <- centroid_one_file(file = file, pattern = pattern, replacement = replacement, ...)
  if (!result) {
    logger::log_error("Failed to process file: {basename(file)}")
  }
  return(result)
}

#' Setup Logging for Centroiding Process
#'
#' @param dir Directory for saving the log file. Defaults to the output directory.
#' @param filename Log file name. Default: `"centroiding.log"`.
#' @return NULL
#' @keywords internal
setup_logger <- function(dir = Sys.getenv("HOME"),
                         filename = "centroiding.log") {
  logger::log_threshold(logger::WARN)
  logger::log_appender(appender = logger::appender_file(file = file.path(dir, filename)))
}
