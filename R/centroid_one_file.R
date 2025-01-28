#' Centroid an mzML file with configurable parameters
#'
#' This function processes an mzML file for centroiding, providing detailed controls for
#' peak picking, smoothing, and noise estimation. It allows fine-tuning of MS1 and MS2
#' peak detection, optimizing spectral data analysis for different experimental needs.
#'
#' @param file `character(1)`
#'   Path to the input mzML file.
#'   The file must be accessible and in valid mzML format.
#' @param pattern `character(1)`
#'   Regular expression pattern to match in the input file path, useful for modifying the output file path.
#' @param replacement `character(1)`
#'   Replacement string for altering the output file path based on the `pattern`.
#' @param ms1_half_window_size `integer(1)`
#'   Half window size for smoothing MS1 spectra.
#'   Larger values apply stronger smoothing.
#'   IGNORED FOR NOW.
#'
#'   Default: `4L`.
#' @param ms1_min_peaks `integer(1)`
#'   Minimum number of peaks required to retain an MS1 spectrum.
#'   Spectra with fewer peaks than this threshold are discarded.
#'
#'   Default: `1000`.
#' @param ms1_noise_estimator `character(1)`
#'   Noise estimation method for MS1 spectra. Available options:
#'   - `"SuperSmoother"`: Smoothing method from the `supsmu` function.
#'   - `"MAD"`: Median Absolute Deviation
#'
#'   Default: `"SuperSmoother"`.
#' @param ms1_peak_snr `numeric(1)`
#'   Signal-to-noise ratio threshold for MS1 peak picking.
#'   Peaks below this threshold will be ignored.
#'   Default: `0`.
#' @param ms1_refine_mz `character(1)`
#'   Method for refining m/z values after MS1 peak picking.
#'   Available options:
#'   - `"kNeighbors"`: Refines using nearest neighbors.
#'   - `"descendPeak"`: Refines peaks by descending intensity.
#'   - `"none"`: No refinement applied.
#'
#'   Default: `"kNeighbors"`.
#' @param ms1_signal_percentage `numeric(1)`
#'   Minimum signal percentage (relative to the maximum signal) for retaining MS1 peaks in centroid calculation.
#'   Default: `33`.
#' @param ms2_noise_estimator `character(1)`
#'   Noise estimation method for MS2 spectra.
#'   Options:
#'   - `"SuperSmoother"`: Smoothing-based estimator.
#'   - `"MAD"`: Median Absolute Deviation
#'
#'   Default: `"SuperSmoother"`.
#' @param ms2_peak_snr `numeric(1)`
#'   Signal-to-noise ratio threshold for MS2 peak picking.
#'   Default: `0`.
#' @param ms2_refine_mz `character(1)`
#'   Method for refining m/z values after MS2 peak picking.
#'   Available options:
#'   - `"kNeighbors"`: Refines using nearest neighbors.
#'   - `"descendPeak"`: Refines peaks by descending intensity.
#'   - `"none"`: No refinement applied.
#'
#'   Default: `"kNeighbors"`.
#' @param ms2_signal_percentage `numeric(1)`
#'   Minimum signal percentage (relative to the maximum signal) for retaining MS2 peaks in centroid calculation.
#'   Default: `50`.
#'
#' @return `logical(1)`
#'   Returns `TRUE` if centroiding was successful; otherwise, returns `FALSE`.
#'
#' @details
#' This function processes both MS1 and MS2 data, applying user-defined smoothing, peak-picking, and noise estimation methods.
#' The centroiding is tailored according to specific experimental requirements, with file path customization available via `pattern` and `replacement`.
#'
#' @export
#' @author
#'   Johannes Rainer, Adriano Rutz
centroid_one_file <- function(file,
                              pattern,
                              replacement,
                              ms1_half_window_size = 4L,
                              ms1_min_peaks = 1000,
                              ms1_noise_estimator = "SuperSmoother",
                              ms1_peak_snr = 0,
                              ms1_refine_mz = "kNeighbors",
                              ms1_signal_percentage = 33,
                              ms2_noise_estimator = "SuperSmoother",
                              ms2_peak_snr = 0,
                              ms2_refine_mz = "kNeighbors",
                              ms2_signal_percentage = 50) {
  # Setup logger
  outf <- sub(
    pattern = pattern,
    replacement = replacement,
    x = file,
    fixed = TRUE
  )
  outd <- dirname(outf)
  setup_logger(dir = outd)

  # Input validation
  if (!file.exists(file)) {
    logger::log_error("Input file does not exist: {file}")
    message("File does not exist: ", file)
    return(FALSE)
  }

  if (file.exists(outf)) {
    message("Output file already exists, skipping: ", outf)
    return(TRUE)
  } else {
    if (!dir.exists(outd)) {
      dir.create(path = outd, recursive = TRUE)
    }

    ## TODO remove this ugly hack to make it work
    library(MSnbase, quietly = TRUE)

    message("Processing mzML file: ", file)
    message("Replacing pattern: ", pattern, " with ", replacement)
    message("MS1 minimum peaks: ", ms1_min_peaks)
    # message("MS1 half window size: ", ms1_half_window_size)
    message("MS1 noise estimator: ", ms1_noise_estimator)
    message("MS1 peak SNR: ", ms1_peak_snr)
    message("MS1 Refine m/z: ", ms1_refine_mz)
    message("MS1 signal percentage: ", ms1_signal_percentage)
    message("MS2 noise estimator: ", ms2_noise_estimator)
    message("MS2 peak SNR: ", ms2_peak_snr)
    message("MS2 Refine m/z: ", ms2_refine_mz)
    message("MS2 signal percentage: ", ms2_signal_percentage)

    tryCatch(
      {
        # Read MS data from file
        ms_data <- MSnbase::readMSData(files = file, mode = "onDisk")

        # Check for MS levels
        ms_levels <- ms_data |>
          MSnbase::msLevel() |>
          unique()
        logger::log_info("Processing file with MS levels: {paste(ms_levels, collapse=', ')}")

        if (1L %in% ms_levels) {
          # Process MS1 and MS2 data separately
          if (length(ms_levels) > 1) {
            ms_data <- ms_data |>
              ## COMMENT: Removed for now
              # MSnbase::smooth(halfWindowSize = ms1_half_window_size, msLevel. = 1L) |>
              MSnbase::pickPeaks(
                method = ms1_noise_estimator,
                refineMz = ms1_refine_mz,
                signalPercentage = ms1_signal_percentage,
                SNR = ms1_peak_snr,
                msLevel. = 1L
              ) |>
              MSnbase::pickPeaks(
                method = ms2_noise_estimator,
                refineMz = ms2_refine_mz,
                signalPercentage = ms2_signal_percentage,
                SNR = ms2_peak_snr,
                msLevel. = 2L
              )
          } else {
            # Single level processing with additional checks
            nspec <- ms_data |>
              length()
            ms_data <- ms_data[MSnbase::peaksCount(ms_data) > min_peaks]

            if (length(ms_data) < nspec) {
              logger::log_warn("Removed {nspec - length(tmp)} spectra with insufficient peaks")
            }

            ms_data <- ms_data |>
              MSnbase::combineSpectraMovingWindow(timeDomain = TRUE) |>
              ## COMMENT: Removed for now
              # MSnbase::smooth(halfWindowSize = ms1_half_window_size) |>
              MSnbase::pickPeaks(refineMz = refine_mz, signalPercentage = ms1_signal_percentage)
          }
          # Write centroided data to output file
          ms_data |>
            MSnbase::writeMSData(file = outf, copy = TRUE)

          logger::log_info("Successfully centroided: {basename(file)}")
          message("Centroiding completed for: ", file)
          return(TRUE)
        } else {
          logger::log_warn("No MS1 spectra found in: {basename(file)}")
          message("No MS1 spectra found in: ", file)
          return(FALSE)
        }
      },
      error = function(e) {
        logger::log_error("Error processing {basename(file)}: {e$message}")
        message("Error processing: ", file, " - ", e$message)
        return(FALSE)
      }
    )
  }
}

#' Wrapper for centroiding with additional error handling and flexible parameters
#'
#' @param file Path to the input mzML file
#' @param pattern Pattern to replace in the file path
#' @param replacement Replacement string for output path
#' @param ... Additional parameters to pass to centroid_one_file
#' @return Logical indicating success of processing
#' @keywords internal
try_centroid_one_file <- function(file, pattern, replacement, ...) {
  result <- centroid_one_file(
    file = file,
    pattern = pattern,
    replacement = replacement,
    ...
  )

  if (!result) {
    logger::log_error("Failed to process file: {basename(file)}")
  }

  return(result)
}

#' Setup Logging for Centroiding Process
#'
#' @param dir Directory for saving the log file. Defaults to the user's output directory.
#' @param filename Filename for the log file. Default: `"centroiding.log"`.
#' @return NULL
#' @keywords internal
setup_logger <- function(dir = Sys.getenv("HOME"),
                         filename = "centroiding.log") {
  logger::log_threshold(logger::WARN)
  logger::log_appender(appender = logger::appender_file(file = file.path(dir, filename)))
}
