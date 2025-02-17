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
#' @param mz_tol_da_ms1 `numeric(1)`
#'   m/z tolerance in Dalton (ms1).
#'
#'   Default: `0.002`.
#' @param mz_tol_da_ms2 `numeric(1)`
#'   m/z tolerance in Dalton (ms2).
#'
#'   Default: `0.005`.
#' @param mz_tol_ppm_ms1 `numeric(1)`
#'   m/z tolerance in ppm (ms1).
#'
#'   Default: `5`.
#' @param mz_tol_ppm_ms2 `numeric(1)`
#'   m/z tolerance in ppm (ms2).
#'
#'   Default: `10`.
#' @param mz_fun
#'   Function to aggregate m/z values for all mass peaks within each peak group into a single m/z value.
#'   This parameter is ignored if weighted = TRUE (the default).
#'
#'   Default: `base::mean`.
#' @param int_fun
#'   Function to aggregate intensities for all peaks in each peak group into a single intensity value.
#'
#'   Default: `base::sum`.
#' @param mz_weighted `logical(1)`
#'   Boolean whether m/z values of peaks within each peak group should be aggregated into a single m/z value using an intensity-weighted mean.
#'
#'   Default: `TRUE`.
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
                              mz_tol_da_ms1 = 0.002,
                              mz_tol_da_ms2 = 0.005,
                              mz_tol_ppm_ms1 = 5,
                              mz_tol_ppm_ms2 = 10,
                              mz_fun = base::mean,
                              int_fun = base::sum,
                              mz_weighted = TRUE) {
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

    message("Processing mzML file: ", file)
    message("Replacing pattern: ", pattern, " with ", replacement)
    message("m/z tolerance in Dalton (MS1): ", mz_tol_da_ms1)
    message("m/z tolerance in Dalton (MS2): ", mz_tol_da_ms2)
    message("m/z tolerance in ppm (MS1): ", mz_tol_ppm_ms1)
    message("m/z tolerance in ppm (MS2): ", mz_tol_ppm_ms2)
    message("Intensity function: ", int_fun |> deparse())
    message("m/z function: ", mz_fun |> deparse())
    message("m/z weighted: ", mz_weighted)

    tryCatch(
      expr = {
        ## Import
        sp <- Spectra::Spectra(file, backend = Spectra::MsBackendMzR())

        ## "Centroiding"
        sp_cen <- sp |>
          Spectra::combinePeaks(
            tolerance = mz_tol_da_ms1,
            ppm = mz_tol_ppm_ms1,
            intensityFun = int_fun,
            mzFun = mz_fun,
            weighted = mz_weighted,
            msLevel. = 1L
          ) |>
          Spectra::combinePeaks(
            tolerance = mz_tol_da_ms2,
            ppm = mz_tol_ppm_ms2,
            intensityFun = int_fun,
            mzFun = mz_fun,
            weighted = mz_weighted,
            msLevel. = 2L
          )

        ## COMMENT: Feels dirty but works
        sp_cen@backend@spectraData$centroided <- TRUE

        ## Export
        sp_cen |>
          Spectra::export(file = outf, backend = Spectra::MsBackendMzR())

        ## Restore filename in mzML
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
