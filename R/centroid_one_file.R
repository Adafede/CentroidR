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
#' @param min_datapoints_ms2 `integer(1)` (default: `1L`)
#'   Minimum datapoints to be considered for MS2 data.
#' @param mz_tol_da_ms1 `numeric(1)` (default: `0.0025`)
#'   m/z tolerance in Daltons for MS1 data.
#' @param mz_tol_da_ms2 `numeric(1)` (default: `0.0025`)
#'   m/z tolerance in Daltons for MS2 data.
#' @param mz_tol_ppm_ms1 `numeric(1)` (default: `5`)
#'   m/z tolerance in parts per million (ppm) for MS1.
#' @param mz_tol_ppm_ms2 `numeric(1)` (default: `5`)
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
  min_datapoints_ms2 = 1L,
  mz_tol_da_ms1 = 0.0025,
  mz_tol_da_ms2 = 0.0025,
  mz_tol_ppm_ms1 = 5.0,
  mz_tol_ppm_ms2 = 5.0,
  mz_fun_ms1 = base::mean,
  mz_fun_ms2 = base::mean,
  int_fun_ms1 = base::max,
  int_fun_ms2 = base::max,
  mz_weighted = TRUE,
  time_domain = TRUE,
  intensity_exponent = 3
) {
  ## This was copied from the Spectra package to be able to access `timeDomain`
  ## And slightly adapted
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

    mz_vals <- if (timeDomain) {
      x[, "mz"] |> sqrt()
    } else {
      x[, "mz"]
    }
    mz_base <- if (timeDomain) {
      # Adjust the absolute tolerance for sqrt(mz)
      min(mz_vals)
    } else {
      1
    }
    grps <- MsCoreUtils::group(
      x = mz_vals,
      tolerance = tolerance / mz_base,
      ppm = ppm
    )

    if (length(unique(grps)) == length(grps)) {
      return(x)
    }

    mzs_split <- split(x[, "mz"], grps)
    ints_split <- split(x[, "intensity"], grps)

    # Compute m/z
    if (weighted) {
      mzs <- vapply(
        seq_along(mzs_split),
        function(i) {
          mz <- mzs_split[[i]]
          int <- ints_split[[i]]
          stats::weighted.mean(mz, int^intensity_exponent + 1)
        },
        numeric(1)
      )
    } else {
      mzs <- MsCoreUtils::vapply1d(mzs_split, mzFun)
    }

    # Compute intensities
    ints <- MsCoreUtils::vapply1d(ints_split, intensityFun)

    # Handle metadata columns, if present
    if (ncol(x) > 2) {
      meta <- x[, !colnames(x) %in% c("mz", "intensity"), drop = FALSE]
      meta_split <- split.data.frame(meta, grps)
      meta_combined <- lapply(seq_along(meta_split), function(i) {
        colapply <- lapply(meta_split[[i]], function(col) {
          u <- unique(col)
          if (length(u) == 1) {
            u
          } else {
            NA
          }
        })
        as.data.frame(colapply, stringsAsFactors = FALSE)
      })
      meta_final <- do.call(rbind, meta_combined)
      return(cbind(mz = mzs, intensity = ints, meta_final))
    } else {
      return(cbind(mz = mzs, intensity = ints))
    }
  }
  .fix_xml <- function(file_path) {
    temp_file <- tempfile()
    lines <- readLines(file_path, warn = FALSE)
    run_id <- basename(file_path)

    # Replace nan with NaN, else files are buggy
    lines <- gsub(
      pattern = "value=\"nan\"",
      replacement = "value=\"NaN\"",
      x = lines,
      fixed = TRUE
    )

    # Update run id
    lines <- gsub(
      pattern = "<run id=\"Experiment_1\"",
      replacement = sprintf("<run id=\"%s\"", run_id),
      x = lines,
      fixed = TRUE
    )

    writeLines(lines, temp_file)
    file.copy(temp_file, file_path, overwrite = TRUE)
    unlink(temp_file)
  }

  .keep_empty <- function(original, processed) {
    processed_peaks <- processed |>
      Spectra::peaksData()
    is_empty <- lengths(processed_peaks) == 0

    if (any(is_empty)) {
      logger::log_trace("Restoring {sum(is_empty)} empty spectra to original")
      original_peaks <- original |>
        Spectra::peaksData()
      empty_indices <- which(is_empty)
      restored_peaks <- purrr::map(empty_indices, function(i) {
        pk <- original_peaks[[i]]
        if (nrow(pk) == 0) {
          return(pk)
        }
        pk[pk[, "intensity"] > 0, , drop = FALSE]
      })
      processed@backend@peaksData[empty_indices] <- restored_peaks
    }

    return(processed)
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
  purrr::walk(
    .x = names(params),
    .f = function(param) {
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
    centroided_2 <- spectra |>
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

    centroided_1 <- spectra |>
      Spectra::filterMsLevel(1L) |>
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

    # Combine processed MS1 and MS2
    centroided <- Spectra::concatenateSpectra(centroided_1, centroided_2)

    # Replace empty processed spectra with original input
    centroided <- .keep_empty(spectra, centroided)
    return(centroided)
  }

  tryCatch(
    {
      logger::log_trace("Starting centroiding process for: {basename(file)}")

      sp <- file |>
        Spectra::Spectra(
          backend = Spectra::MsBackendMzR(),
          BPPARAM = BiocParallel::SerialParam()
        ) |>
        Spectra::dropNaSpectraVariables()
      sd <- sp |>
        Spectra::spectraData()
      sp@backend@spectraData <- sd |>
        purrr::discard(.x = sd, .p = ~ all(is.na(.x) | .x == 0))
      ## We need the polarity = 0L column for negative mode
      if (!"polarity" %in% colnames(sp@backend@spectraData)) {
        sp@backend@spectraData$polarity <- 0L
      }
      ## Fix for missing values
      if (!"basePeakIntensity" %in% colnames(sp@backend@spectraData)) {
        sp@backend@spectraData$basePeakIntensity <- NA_integer_
      }
      if (!"collisionEnergy" %in% colnames(sp@backend@spectraData)) {
        sp@backend@spectraData$collisionEnergy <- NA_integer_
      }
      if (!"electronBeamEnergy" %in% colnames(sp@backend@spectraData)) {
        sp@backend@spectraData$electronBeamEnergy <- NA_integer_
      }
      if (!"isolationWindowLowerOffset" %in% colnames(sp@backend@spectraData)) {
        sp@backend@spectraData$isolationWindowLowerOffset <- NA_integer_
      }
      if (!"isolationWindowUpperOffset" %in% colnames(sp@backend@spectraData)) {
        sp@backend@spectraData$isolationWindowUpperOffset <- NA_integer_
      }
      rm(sd)

      # TODO see if expose of not
      # Batch processing
      batch_size <- 4096L
      n <- length(sp)
      batch_starts <- seq(1L, n, by = batch_size)

      tmp_batch_dir <- file.path(outd, "tmp")
      if (!dir.exists(tmp_batch_dir)) {
        dir.create(tmp_batch_dir, recursive = TRUE)
      }

      temp_files <- purrr::imap_chr(batch_starts, function(start_idx, i) {
        idx <- start_idx:min(start_idx + batch_size - 1L, n)
        sp_batch <- sp[idx] |>
          Spectra::setBackend(Spectra::MsBackendMemory())

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
        temp_file <- tempfile(fileext = ".mzML", tmpdir = tmp_batch_dir)
        Spectra::export(
          result,
          file = temp_file,
          backend = Spectra::MsBackendMzR(),
          BPPARAM = BiocParallel::SerialParam()
        )

        rm(idx, sp_batch, result)

        return(temp_file)
      })
      logger::log_trace("Concatenating all processed batches")
      sp_cen <- Spectra::Spectra(temp_files, backend = Spectra::MsBackendMzR())

      logger::log_trace("Exporting: {basename(file)}")
      sp_cen |>
        Spectra::export(file = outf, backend = Spectra::MsBackendMzR())
      rm(sp_cen)
      logger::log_trace("Exported: {basename(file)}")

      logger::log_trace("Making a few fixes inside mzML: {basename(file)}")
      outf |>
        .fix_xml()
      logger::log_trace("Made fixes inside mzML: {basename(file)}")

      logger::log_success("Successfully centroided: {basename(file)}")
      unlink(tmp_batch_dir, recursive = TRUE)
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
