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
#' @param min_datapoints_ms1 `integer(1)` (default: `5L`)
#'   Minimum datapoints to be considered for MS1 data.
#' @param min_datapoints_ms2 `integer(1)` (default: `2L`)
#'   Minimum datapoints to be considered for MS2 data.
#' @param mz_tol_da_ms1 `numeric(1)` (default: `0.0025`)
#'   m/z tolerance in Daltons for MS1 data.
#' @param mz_tol_da_ms2 `numeric(1)` (default: `0.005`)
#'   m/z tolerance in Daltons for MS2 data.
#' @param mz_tol_ppm_ms1 `numeric(1)` (default: `5`)
#'   m/z tolerance in parts per million (ppm) for MS1.
#' @param mz_tol_ppm_ms2 `numeric(1)` (default: `10`)
#'   m/z tolerance in parts per million (ppm) for MS2.
#' @param mz_fun_ms1 `function` (default: `base::mean`)
#'   Function to aggregate m/z values within each peak group (MS1).
#'   Ignored if `mz_weighted = TRUE`.
#' @param mz_fun_ms2 `function` (default: `base::mean`)
#'   Function to aggregate m/z values within each peak group (MS2).
#'   Ignored if `mz_weighted = TRUE`.
#' @param int_fun_ms1 `function` (default: `base::max`)
#'   Function to aggregate peak intensities within each peak group (MS1).
#' @param int_fun_ms2 `function` (default: `base::max`)
#'   Function to aggregate peak intensities within each peak group (MS2).
#' @param mz_weighted `logical(1)` (default: `TRUE`)
#'   If `TRUE`, uses intensity-weighted mean for m/z value aggregation.
#' @param time_domain `logical(1)` (default: `TRUE`)
#'   If `TRUE`, uses square root for m/z weighting (TRUE for TOF).
#' @param intensity_exponent `numeric(1)` (default: `3`)
#'   If `mz_weighted=TRUE`, the exponent of the intensity for m/z weighting.
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
centroid_one_file <- function(
  file,
  pattern,
  replacement,
  min_datapoints_ms1 = 5L,
  min_datapoints_ms2 = 2L,
  mz_tol_da_ms1 = 0.0025,
  mz_tol_da_ms2 = 0.005,
  mz_tol_ppm_ms1 = 5.0,
  mz_tol_ppm_ms2 = 10.0,
  mz_fun_ms1 = base::mean,
  mz_fun_ms2 = base::mean,
  int_fun_ms1 = base::max,
  int_fun_ms2 = base::max,
  mz_weighted = TRUE,
  time_domain = TRUE,
  intensity_exponent = 3
) {
  ## This was copied from the Spectra package to be able to access `timeDomain`
  .peaks_combine <- function(
    x,
    ppm = 20,
    tolerance = 0,
    intensityFun = base::mean,
    mzFun = base::mean,
    weighted = TRUE,
    spectrumMsLevel,
    msLevel = spectrumMsLevel,
    timeDomain = FALSE,
    ...
  ) {
    if (!spectrumMsLevel %in% msLevel || !length(x)) {
      return(x)
    }

    # Apply the sqrt transformation if timeDomain is TRUE
    if (timeDomain) {
      # Adjust the absolute tolerance for sqrt(mz)
      mz_sqrt <- x[, "mz"] |>
        sqrt()
      grps <- MsCoreUtils::group(
        mz_sqrt,
        tolerance = tolerance / min(mz_sqrt),
        ppm = ppm
      )
    } else {
      grps <- MsCoreUtils::group(x[, "mz"], tolerance = tolerance, ppm = ppm)
    }

    lg <- length(grps)
    if (grps[lg] == lg) {
      return(x)
    }

    mzs <- split(x[, "mz"], grps)
    ints <- split(x[, "intensity"], grps)

    if (weighted) {
      mzs <- unlist(
        mapply(
          mzs,
          ints,
          FUN = function(m, i)
            stats::weighted.mean(m, i^intensity_exponent + 1),
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        ),
        use.names = FALSE
      )
    } else {
      mzs <- MsCoreUtils::vapply1d(mzs, mzFun)
    }

    ints <- MsCoreUtils::vapply1d(ints, intensityFun)

    if (ncol(x) > 2) {
      lst <- lapply(x[!colnames(x) %in% c("mz", "intensity")], function(z) {
        z <- lapply(split(z, grps), unique)
        z[lengths(z) > 1] <- NA
        unlist(z, use.names = FALSE, recursive = FALSE)
      })
      do.call(
        cbind.data.frame,
        c(list(mz = mzs, intensity = ints), lst)
      )
    } else {
      cbind(mz = mzs, intensity = ints)
    }
  }

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
    return(FALSE)
  }

  if (file.exists(outf)) {
    logger::log_info("Skipping. Output file already exists: {outf}")
    return(TRUE)
  }

  # Ensure output directory exists
  if (!dir.exists(outd)) {
    dir.create(path = outd, recursive = TRUE)
  }

  logger::log_info("Processing mzML file: {file}")

  # Logging parameter settings
  params <- list(
    "min datapoints MS1" = min_datapoints_ms1,
    "min datapoints MS2" = min_datapoints_ms2,
    "m/z tolerance (Da, MS1)" = mz_tol_da_ms1,
    "m/z tolerance (Da, MS2)" = mz_tol_da_ms2,
    "m/z tolerance (ppm, MS1)" = mz_tol_ppm_ms1,
    "m/z tolerance (ppm, MS2)" = mz_tol_ppm_ms2,
    "m/z function (MS1)" = deparse(mz_fun_ms1),
    "m/z function (MS2)" = deparse(mz_fun_ms2),
    "Intensity function (MS1)" = deparse(int_fun_ms1),
    "Intensity function (MS2)" = deparse(int_fun_ms2),
    "m/z weighted" = mz_weighted,
    "Time domain" = time_domain
  )
  lapply(
    X = names(params),
    FUN = function(param) {
      logger::log_info("{param} : {params[[param]]}")
    }
  )

  # Custom intensity aggregation functions
  custom_int_fun <- function(int_fun, intensities, min_datapoints) {
    if (length(intensities[intensities > 0]) >= min_datapoints) {
      int_fun(intensities)
    } else {
      0
    }
  }
  custom_int_fun_ms1 <- function(intensities) {
    custom_int_fun(int_fun_ms1, intensities, min_datapoints_ms1)
  }
  custom_int_fun_ms2 <- function(intensities) {
    custom_int_fun(int_fun_ms2, intensities, min_datapoints_ms2)
  }
  process_spectra <- function(
    spectra,
    mz_tol_da_ms1,
    mz_tol_da_ms2,
    mz_tol_ppm_ms1,
    mz_tol_ppm_ms2,
    custom_int_fun_ms1,
    custom_int_fun_ms2,
    mz_fun_ms1,
    mz_fun_ms2,
    mz_weighted,
    time_domain
  ) {
    sp_mem <- spectra |>
      Spectra::setBackend(Spectra::MsBackendMemory())
    centroided_2 <- sp_mem |>
      Spectra::filterMsLevel(2L) |>
      Spectra::addProcessing(
        .peaks_combine,
        tolerance = mz_tol_da_ms2,
        ppm = mz_tol_ppm_ms2,
        intensityFun = custom_int_fun_ms2,
        mzFun = mz_fun_ms2,
        weighted = mz_weighted,
        timeDomain = time_domain,
        msLevel. = 2L
      ) |>
      Spectra::filterIntensity(intensity = c(.Machine$double.eps, Inf)) |>
      Spectra::applyProcessing()
    centroided_1 <- sp_mem |>
      Spectra::filterMsLevel(1L) |>
      Spectra::filterIntensity(intensity = c(.Machine$double.eps, Inf)) |>
      Spectra::addProcessing(
        .peaks_combine,
        tolerance = mz_tol_da_ms1,
        ppm = mz_tol_ppm_ms1,
        intensityFun = custom_int_fun_ms1,
        mzFun = mz_fun_ms1,
        weighted = mz_weighted,
        timeDomain = time_domain,
        msLevel. = 1L
      ) |>
      Spectra::filterIntensity(intensity = c(.Machine$double.eps, Inf)) |>
      Spectra::applyProcessing()
    rm(sp_mem)
    return(
      centroided_1 |>
        Spectra::concatenateSpectra(centroided_2)
    )
  }

  tryCatch(
    {
      logger::log_trace("Starting centroiding process for: {basename(file)}")

      sp <- Spectra::Spectra(file, backend = Spectra::MsBackendMzR())

      # TODO see if expose of not
      # Batch processing
      batch_size <- 1000L
      n <- length(sp)
      batch_starts <- seq(1L, n, by = batch_size)

      temp_files <- purrr::imap_chr(batch_starts, function(start_idx, i) {
        idx <- start_idx:min(start_idx + batch_size - 1L, n)
        sp_batch <- sp[idx]

        logger::log_trace("Processing batch {i} / {length(batch_starts)}")

        result <- process_spectra(
          spectra = sp_batch,
          mz_tol_da_ms1 = mz_tol_da_ms1,
          mz_tol_da_ms2 = mz_tol_da_ms2,
          mz_tol_ppm_ms1 = mz_tol_ppm_ms1,
          mz_tol_ppm_ms2 = mz_tol_ppm_ms2,
          custom_int_fun_ms1 = custom_int_fun_ms1,
          custom_int_fun_ms2 = custom_int_fun_ms2,
          mz_fun_ms1 = mz_fun_ms1,
          mz_fun_ms2 = mz_fun_ms2,
          mz_weighted = mz_weighted,
          time_domain = time_domain
        )
        # COMMENT: Feels dirty but works
        result@backend@spectraData$centroided <- TRUE
        temp_file <- tempfile(fileext = ".mzML")
        Spectra::export(
          result,
          file = temp_file,
          backend = Spectra::MsBackendMzR()
        )

        rm(idx, sp_batch, result)
        invisible(gc())

        return(temp_file)
      })
      logger::log_trace("Concatenating all processed batches")
      sp_cen <- Spectra::Spectra(temp_files, backend = Spectra::MsBackendMzR())
      unlink(temp_files)
      invisible(gc(verbose = FALSE))

      logger::log_trace("Exporting: {basename(file)}")
      sp_cen |>
        Spectra::export(file = outf, backend = Spectra::MsBackendMzR())
      rm(sp_cen)
      logger::log_trace("Exported: {basename(file)}")

      logger::log_trace("Renaming inside mzML: {basename(file)}")
      filename <- basename(outf)
      temp_file <- tempfile()
      in_con <- file(outf, "r")
      out_con <- file(temp_file, "w")
      while (length(line <- readLines(in_con, n = 1, warn = FALSE)) > 0) {
        line <- gsub(
          pattern = "<run id=\"Experiment_1\"",
          replacement = paste0("<run id=\"", filename, "\""),
          x = line
        )
        writeLines(line, out_con)
      }
      close(in_con)
      close(out_con)
      file.rename(temp_file, outf)
      logger::log_trace("Renamed inside mzML: {basename(file)}")

      logger::log_success("Successfully centroided: {basename(file)}")
      return(TRUE)
    },
    error = function(e) {
      logger::log_error("Error processing {basename(file)}: {e$message}")
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
#' @param dir Directory for saving the log file. Defaults to the output directory.
#' @param filename Log file name. Default: `"centroiding.log"`.
#' @return NULL
#' @keywords internal
setup_logger <- function(
  dir = Sys.getenv("HOME"),
  filename = "centroiding.log"
) {
  logger::log_threshold(logger::TRACE)
  logger::log_appender(
    appender = logger::appender_tee(
      file = file.path(dir, filename)
    )
  )
}
