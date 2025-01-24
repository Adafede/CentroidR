#' Centroid an mzML file with Configurable Peak Picking Parameters
#'
#' @param file Path to the input mzML file
#' @param pattern Pattern to replace in file path
#' @param replacement Replacement string for output path
#' @param fixed Logical, whether pattern matching should be fixed
#' @param smooth_method Smoothing method for spectra
#' @param smooth_window Half window size for smoothing
#' @param refine_mz Refining method
#' @param ms1_peak_snr Signal-to-noise ratio for MS1 peak picking
#' @param ms1_signal_percentage Signal percentage for MS1 peak refinement
#' @param ms2_peak_snr Signal-to-noise ratio for MS2 peak picking
#' @param ms2_signal_percentage Signal percentage for MS2 peak refinement
#' @param min_peaks Minimum number of peaks to retain a spectrum
#' @return Logical indicating success of centroiding
#' @export
#' @author Johannes Rainer
centroid_one_file <- function(file,
                              pattern,
                              replacement,
                              fixed = TRUE,
                              smooth_method = "SavitzkyGolay",
                              smooth_window = 6L,
                              refine_mz = "descendPeak",
                              ms1_peak_snr = 1L,
                              ms1_signal_percentage = 33,
                              ms2_peak_snr = 1L,
                              ms2_signal_percentage = 50,
                              min_peaks = 1000) {
  # Setup logging
  setup_logger()

  # Input validation
  if (!file.exists(file)) {
    log_error("File does not exist: {file}")
    return(FALSE)
  }

  # Create output directory
  outf <- sub(
    pattern = pattern,
    replacement = replacement,
    x = file,
    fixed = fixed
  )
  if (file.exists(outf)) {
    message("Output file already exists, skipping: ", outf)
    return(TRUE)
  } else {
    outd <- dirname(outf)
    if (!dir.exists(outd)) {
      dir.create(path = outd, recursive = TRUE) |>
        try(silent = TRUE)
    }

    # tryCatch for comprehensive error handling
    tryCatch(
      {
        # Read MS data
        tmp <- MSnbase::readMSData(files = file, mode = "onDisk")

        # Check for MS levels
        ms_levels <- MSnbase::msLevel(object = tmp) |>
          unique()
        logger::log_info("Processing file with MS levels: {paste(ms_levels, collapse=', ')}")

        if (1L %in% ms_levels) {
          # Sophisticated peak picking strategy
          if (length(ms_levels) > 1) {
            # Multi-level processing
            tmp <- tmp |>
              MSnbase::smooth(
                method = smooth_method,
                halfWindowSize = smooth_window,
                msLevel. = 1L
              ) |>
              MSnbase::pickPeaks(
                refineMz = refine_mz,
                signalPercentage = ms1_signal_percentage,
                SNR = ms1_peak_snr,
                msLevel. = 1L
              ) |>
              MSnbase::pickPeaks(
                halfWindowSize = smooth_window,
                SNR = ms2_peak_snr,
                refineMz = refine_mz,
                signalPercentage = ms2_signal_percentage,
                msLevel. = 2L
              )
          } else {
            # Single level processing with additional checks
            nspec <- tmp |>
              length()
            tmp <- tmp[peaksCount(tmp) > min_peaks]

            if (length(tmp) < nspec) {
              log_warn("Removed {nspec - length(tmp)} spectra with insufficient peaks")
            }

            tmp <- tmp |>
              MSnbase::combineSpectraMovingWindow(timeDomain = TRUE) |>
              MSnbase::smooth(method = smooth_method, halfWindowSize = smooth_window) |>
              MSnbase::pickPeaks(refineMz = refine_mz, signalPercentage = ms1_signal_percentage)
          }

          # Write processed data
          tmp |>
            MSnbase::writeMSData(file = outf, copy = TRUE)

          log_info("Successfully centroided: {basename(file)}")
          return(TRUE)
        } else {
          log_warn("No MS1 spectra found in {basename(file)}")
          return(FALSE)
        }
      },
      error = function(e) {
        logger::log_error("Error processing {basename(file)}: {e$message}")
        return(FALSE)
      }
    )
  }
}

#' Wrapper for centroiding with additional error handling and flexible parameters
#'
#' @param file Path to the input mzML file
#' @param pattern Pattern to replace in file path
#' @param replacement Replacement string for output path
#' @param fixed Logical, whether pattern matching should be fixed
#' @param ... Additional parameters to pass to centroid_one_file
#' @keywords internal
#' @return Logical indicating success of processing
try_centroid_one_file <- function(file, pattern, replacement, fixed = TRUE, ...) {
  result <- centroid_one_file(
    file = file,
    pattern = pattern,
    replacement = replacement,
    fixed = fixed,
    ...
  )

  if (!result) {
    logger::log_error("Failed to process file: {basename(file)}")
  }

  return(result)
}

#' Setup Logging for Centroiding Process
#'
#' @param dir Directory. Defaults to HOME
#' @param filename Filename. Defaults to centroiding.log
#' @return NULL
#' @keywords internal
setup_logger <- function(dir = Sys.getenv("HOME"),
                         filename = "centroiding.log") {
  logger::log_threshold(logger::WARN)
  logger::log_appender(appender = logger::appender_file(file = file.path(dir, filename)))
}
