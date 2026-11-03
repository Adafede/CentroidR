#' Centroid an mzML file with configurable parameters
#'
#' This function processes an mzML file to apply centroiding with detailed
#' controls for peak picking, valley splitting, and intensity filtering. It
#' allows fine-tuning of MS1 and MS2 peak detection, optimizing spectral data
#' analysis for various experimental needs.
#'
#' @param file character(1). Path to the input mzML file (`EDAM:format_3244`).
#'   Must be a valid, accessible mzML format file.
#' @param pattern character(1). Regular expression pattern to match in the
#'   input file path, used for modifying the output file path.
#' @param replacement character(1). Replacement string for altering the output
#'   file path based on the `pattern`.
#' @param min_datapoints_ms1 integer(1). Minimum datapoints to be considered
#'   for MS1 data. Must be at least 1. Default: 5L.
#' @param min_datapoints_ms2 integer(1). Minimum datapoints to be considered
#'   for MS2 data. Must be at least 1. Default: 1L.
#' @param mz_tol_da_ms1 numeric(1). m/z tolerance in Daltons for MS1 data.
#'   Default: 0.0025.
#' @param mz_tol_da_ms2 numeric(1). m/z tolerance in Daltons for MS2 data.
#'   Default: 0.0025.
#' @param mz_tol_ppm_ms1 numeric(1). m/z tolerance in parts per million (ppm)
#'   for MS1. Default: 5.
#' @param mz_tol_ppm_ms2 numeric(1). m/z tolerance in parts per million (ppm)
#'   for MS2. Default: 5.
#' @param mz_fun_ms1 function. Function to aggregate m/z values within each
#'   peak group (MS1). Ignored if `mz_weighted = TRUE`. Default: base::mean.
#' @param mz_fun_ms2 function. Function to aggregate m/z values within each
#'   peak group (MS2). Ignored if `mz_weighted = TRUE`. Default: base::mean.
#' @param int_fun_ms1 function. Function to aggregate peak intensities within
#'   each peak group (MS1). Default: base::max.
#' @param int_fun_ms2 function. Function to aggregate peak intensities within
#'   each peak group (MS2). Default: base::max.
#' @param mz_weighted logical(1). If `TRUE`, uses intensity-weighted mean for
#'   m/z value aggregation. Default: TRUE.
#' @param time_domain logical(1). If `TRUE`, uses square root for m/z weighting
#'   (TRUE for TOF). Default: TRUE.
#' @param intensity_exponent numeric(1). If `mz_weighted=TRUE`, the exponent of
#'   the intensity for m/z weighting. Default: 3.
#'
#' @return logical(1). `TRUE` if the file was centroided, or `TRUE` if it was
#'   skipped because the output file already existed; `FALSE` if the input was
#'   missing or if centroiding failed. Invalid argument types raise an error
#'   instead of returning `FALSE`.
#'
#' @details
#' MS1 and MS2 spectra are processed separately with their own tolerances, so
#' the two levels can be tuned independently.
#'
#' Within a spectrum, profile points are grouped by the m/z tolerances, a group
#' is split back into separate peaks at deep valleys so that closely spaced
#' peaks are not merged, and each group is reduced to a single peak. A group is
#' only reduced when it holds at least `min_datapoints_ms*` points of non-zero
#' intensity; below that it is reported as zero intensity and filtered out.
#' Spectra left without any peak by this step are restored from the input, so
#' no spectrum is lost.
#'
#' File path modifications are supported via `pattern` and `replacement`.
#' `pattern` is a regular expression, as in `sub()`.
#'
#' @section EDAM Ontology:
#' - Operation: `http://edamontology.org/operation_3215` (Peak picking)
#' - Input/Output Data: `http://edamontology.org/data_0943` (Mass spectrometry spectra)
#' - Input/Output Format: `http://edamontology.org/format_3244` (mzML)
#'
#' @concept EDAM:operation_3215
#' @concept EDAM:data_0943
#' @concept EDAM:format_3244
#' @export
#' @author Johannes Rainer, Adriano Rutz
#' @examples
#' \dontrun{
#' # Example usage (requires an mzML file):
#' # centroid_one_file(
#' #   file = "raw_data.mzML",
#' #   pattern = "\\.mzML$",
#' #   replacement = "_centroided.mzML",
#' #   min_datapoints_ms1 = 5L,
#' #   min_datapoints_ms2 = 1L
#' # )
#' }
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
  # Input validation for critical arguments
  stopifnot(
    is.character(file),
    length(file) == 1L,
    is.character(pattern),
    length(pattern) == 1L,
    is.character(replacement),
    length(replacement) == 1L,
    is.numeric(min_datapoints_ms1),
    length(min_datapoints_ms1) == 1L,
    ## A threshold counts data points, so it has to be at least one. Zero and
    ## negative values used to be accepted and silently centroided groups that
    ## held no data at all, which is never what a minimum is meant to express.
    ## An NA fails the comparison, so it is rejected too.
    min_datapoints_ms1 >= 1,
    ## It also has to be a whole number of points: a threshold of 1.5 cannot be
    ## reached, and the argument is documented as integer(1). Inf fails the
    ## modulus as well, since Inf %% 1 is NaN.
    min_datapoints_ms1 %% 1 == 0,
    is.numeric(min_datapoints_ms2),
    length(min_datapoints_ms2) == 1L,
    min_datapoints_ms2 >= 1,
    min_datapoints_ms2 %% 1 == 0,
    is.numeric(mz_tol_da_ms1),
    length(mz_tol_da_ms1) == 1L,
    is.numeric(mz_tol_da_ms2),
    length(mz_tol_da_ms2) == 1L,
    is.numeric(mz_tol_ppm_ms1),
    length(mz_tol_ppm_ms1) == 1L,
    is.numeric(mz_tol_ppm_ms2),
    length(mz_tol_ppm_ms2) == 1L,
    is.function(mz_fun_ms1),
    is.function(mz_fun_ms2),
    is.function(int_fun_ms1),
    is.function(int_fun_ms2),
    is.logical(mz_weighted),
    length(mz_weighted) == 1L,
    is.logical(time_domain),
    length(time_domain) == 1L,
    is.numeric(intensity_exponent),
    length(intensity_exponent) == 1L
  )

  # Construct output file path
  outf <- sub(
    pattern = pattern,
    replacement = replacement,
    x = file
  )
  outd <- dirname(outf)

  # Input validation for file existence
  if (!file.exists(file)) {
    .log_error(paste0("Input file does not exist: ", file))
    return(FALSE)
  }
  if (file.exists(outf)) {
    .log_info(paste0("Skipping. Output file already exists: ", outf))
    return(TRUE)
  }
  # Ensure output directory exists
  if (!dir.exists(outd)) {
    dir.create(path = outd, recursive = TRUE)
  }
  # Start the provenance log before anything is logged, so that the entries
  # below reach the log file and not only the console.
  setup_logger(dir = outd)
  ## Hand the logger back when the run is over. The log file is complete by
  ## then and stays on disk; what must not survive is a process-wide appender
  ## pointing at this run's output directory.
  on.exit(restore_logger(), add = TRUE)
  .log_info(paste0("Processing mzML file: ", file))

  # Logging parameter settings
  # deparse() returns one element per source line, so a function argument can
  # come back as a multi element character vector. The log entry has to be a
  # single line, so collapse it here rather than let the logger deal with a
  # length > 1 value.
  fun_label <- function(f) {
    paste(deparse(f), collapse = " ")
  }
  params <- list(
    "min datapoints MS1" = min_datapoints_ms1,
    "min datapoints MS2" = min_datapoints_ms2,
    "m/z tolerance (Da, MS1)" = mz_tol_da_ms1,
    "m/z tolerance (Da, MS2)" = mz_tol_da_ms2,
    "m/z tolerance (ppm, MS1)" = mz_tol_ppm_ms1,
    "m/z tolerance (ppm, MS2)" = mz_tol_ppm_ms2,
    "m/z function (MS1)" = fun_label(mz_fun_ms1),
    "m/z function (MS2)" = fun_label(mz_fun_ms2),
    "Intensity function (MS1)" = fun_label(int_fun_ms1),
    "Intensity function (MS2)" = fun_label(int_fun_ms2),
    "m/z weighted" = mz_weighted,
    "Time domain" = time_domain,
    "Intensity exponent" = intensity_exponent
  )
  for (param_name in names(params)) {
    .log_info(paste0(param_name, " : ", params[[param_name]]))
  }

  tryCatch(
    {
      .log_trace(paste0(
        "Starting centroiding process for: ",
        basename(file)
      ))
      sp <- file |>
        Spectra::Spectra(
          backend = Spectra::MsBackendMzR(),
          BPPARAM = BiocParallel::SerialParam()
        ) |>
        Spectra::dropNaSpectraVariables()
      sd <- Spectra::spectraData(sp)
      keep <- vapply(
        sd,
        function(col) {
          if (is.numeric(col) || is.logical(col)) {
            !all(is.na(col) | col == 0)
          } else {
            !all(is.na(col))
          }
        },
        logical(1)
      )
      sp@backend@spectraData <- sd[, keep, drop = FALSE]
      # Ensure required columns exist for downstream processing
      req_int_cols <- c(
        "basePeakIntensity",
        "collisionEnergy",
        "electronBeamEnergy",
        "isolationWindowLowerOffset",
        "isolationWindowUpperOffset"
      )
      existing_cols <- colnames(sp@backend@spectraData)
      if (!"polarity" %in% existing_cols) {
        sp@backend@spectraData$polarity <- 0L
      }
      for (col_name in req_int_cols) {
        if (!col_name %in% existing_cols) {
          sp@backend@spectraData[[col_name]] <- NA_integer_
        }
      }
      rm(sd)
      .process_spectra_batches(
        spectra = sp,
        outf = outf,
        outd = outd,
        min_datapoints_ms1 = min_datapoints_ms1,
        min_datapoints_ms2 = min_datapoints_ms2,
        mz_tol_da_ms1 = mz_tol_da_ms1,
        mz_tol_da_ms2 = mz_tol_da_ms2,
        mz_tol_ppm_ms1 = mz_tol_ppm_ms1,
        mz_tol_ppm_ms2 = mz_tol_ppm_ms2,
        mz_fun_ms1 = mz_fun_ms1,
        mz_fun_ms2 = mz_fun_ms2,
        int_fun_ms1 = int_fun_ms1,
        int_fun_ms2 = int_fun_ms2,
        mz_weighted = mz_weighted,
        time_domain = time_domain,
        intensity_exponent = intensity_exponent
      )
    },
    error = function(e) {
      .log_error(
        paste0(
          "Error processing ",
          basename(file),
          ": ",
          conditionMessage(e)
        )
      )
      return(FALSE)
    }
  )
}

#' Default console appender for CentroidR logging
#' @noRd
.appender_stdout <- function(lines) {
  cat(paste0(lines, "\n"), file = stdout(), sep = "")
}

#' State tracking for the internal logger
#' @noRd
.centroidr_log <- local({
  env <- new.env(parent = emptyenv())
  env$threshold <- 400L
  env$appender <- .appender_stdout
  env$installed <- FALSE
  env
})

#' Emit a formatted log line when within the active threshold
#' @noRd
.log_emit <- function(level_name, level_num, msg) {
  if (level_num <= .centroidr_log$threshold) {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    .centroidr_log$appender(sprintf("%s [%s] %s", level_name, ts, msg))
  }
  invisible(NULL)
}

#' @noRd
.log_error <- function(msg) .log_emit("ERROR", 200L, msg)

#' @noRd
.log_success <- function(msg) .log_emit("SUCCESS", 350L, msg)

#' @noRd
.log_info <- function(msg) .log_emit("INFO", 400L, msg)

#' @noRd
.log_trace <- function(msg) .log_emit("TRACE", 600L, msg)

#' Configure the logger for a centroiding run
#'
#' Raises the log threshold to TRACE and installs an appender that writes to
#' `filename` in `dir` while still echoing to the console, so the log records
#' the whole run.
#'
#' The appender is global, so `restore_logger()` puts the console appender back
#' once the run is over and the log file has been fully written.
#'
#' @param dir character(1). Directory for saving the log file. Defaults to the
#'   output directory.
#' @param filename character(1). Log file name. Default: "centroiding.log".
#' @return NULL
#' @noRd
setup_logger <- function(
  dir = Sys.getenv("HOME"),
  filename = "centroiding.log"
) {
  .centroidr_log$threshold <- 600L
  .centroidr_log$appender <- .log_appender(file.path(dir, filename))
  .centroidr_log$installed <- TRUE
  invisible(NULL)
}

#' Return the logger to the console appender
#'
#' `setup_logger()` installs a process-wide appender that outlives the run, so
#' the logger is restored once the run is over. That keeps a finished run from
#' capturing messages that other code logs later on, and keeps the appender
#' from pointing at a directory that may since have been removed.
#'
#' @return NULL, invisibly.
#' @noRd
restore_logger <- function() {
  if (!isTRUE(.centroidr_log$installed)) {
    return(invisible(NULL))
  }
  .centroidr_log$appender <- .appender_stdout
  .centroidr_log$threshold <- 400L
  .centroidr_log$installed <- FALSE
  invisible(NULL)
}

#' Build an appender that mirrors to the console and to a log file
#'
#' Writes to `stdout()` and appends to `file`, except that a log file which has
#' become unwritable is not allowed to break the caller. The failure is
#' reported once via `warning()` so it is not silently hidden.
#'
#' @param file character(1). Path of the log file.
#' @return function(lines). Appender callable with the lines to write.
#' @noRd
.log_appender <- function(file) {
  state <- new.env(parent = emptyenv())
  state$warned <- FALSE
  function(lines) {
    ## Console first, so the message is shown even if the file write fails.
    .appender_stdout(lines)
    tryCatch(
      suppressWarnings(cat(
        paste0(lines, "\n"),
        file = file,
        sep = "",
        append = TRUE
      )),
      error = function(e) {
        if (!state$warned) {
          state$warned <- TRUE
          warning(
            "CentroidR cannot write its log file '",
            file,
            "': ",
            conditionMessage(e),
            call. = FALSE
          )
        }
        NULL
      }
    )
  }
}

#' @noRd
.split_peak_group <- function(idx, int_raw, valley_ratio = 0.2, hws = 2L) {
  n <- length(idx)
  if (n < hws + 2L) {
    return(list(idx))
  }
  ints <- int_raw[idx]
  maxima <- which(MsCoreUtils::localMaxima(ints, hws = hws))
  if (length(maxima) < 2L) {
    return(list(idx))
  }
  split_at <- NA_integer_
  split_score <- Inf
  for (j in seq_len(length(maxima) - 1L)) {
    left <- maxima[[j]]
    right <- maxima[[j + 1L]]
    valley_local <- left + which.min(ints[left:right]) - 1L
    peak_floor <- min(ints[left], ints[right])
    if (peak_floor <= 0) {
      next
    }
    score <- ints[valley_local] / peak_floor
    if (score <= valley_ratio && score < split_score) {
      split_at <- valley_local
      split_score <- score
    }
  }
  if (is.na(split_at)) {
    return(list(idx))
  }
  c(
    .split_peak_group(idx[seq_len(split_at)], int_raw, valley_ratio, hws),
    .split_peak_group(
      idx[(split_at + 1L):n],
      int_raw,
      valley_ratio,
      hws
    )
  )
}

#' Vectorized screening of peak groups before recursive valley splitting
#' @noRd
.split_peak_groups <- function(grp_idx, int_raw, valley_ratio = 0.2, hws = 2L) {
  lens <- lengths(grp_idx, use.names = FALSE)
  cand <- which(lens >= hws + 2L)
  if (!length(cand)) {
    return(unname(grp_idx))
  }
  cand_idx <- grp_idx[cand]
  cand_lens <- lens[cand]
  block_lens <- cand_lens + 2L * hws
  padded <- numeric(sum(block_lens))
  ends <- cumsum(block_lens)
  starts <- ends - block_lens + 1L
  pos <- sequence(cand_lens, from = starts)
  padded[pos] <- int_raw[unlist(cand_idx, use.names = FALSE)]
  lm <- MsCoreUtils::localMaxima(padded, hws = hws)[pos]
  lm_ends <- cumsum(cand_lens)
  lm_counts <- diff(c(0L, cumsum(lm)[lm_ends]))
  to_split <- cand[lm_counts >= 2L]
  if (!length(to_split)) {
    return(unname(grp_idx))
  }
  res <- lapply(grp_idx, list)
  res[to_split] <- lapply(
    grp_idx[to_split],
    .split_peak_group,
    int_raw = int_raw,
    valley_ratio = valley_ratio,
    hws = hws
  )
  unlist(res, recursive = FALSE, use.names = FALSE)
}

#' @noRd
.peak_group_indices <- function(grps) {
  split(seq_along(grps), grps)
}

#' @noRd
.peak_group_names <- function(peak_groups) {
  as.character(seq_along(peak_groups))
}

#' @noRd
.peak_group_mzs <- function(
  peak_groups,
  mz_raw,
  int_raw,
  weighted,
  intensity_exponent,
  mzFun
) {
  if (weighted) {
    w_all <- int_raw^intensity_exponent
    mw_all <- mz_raw * w_all
    vapply(
      peak_groups,
      function(i) {
        w <- w_all[i]
        sw <- sum(w)
        if (sw == 0 && sum(abs(w)) == 0) {
          mzFun(mz_raw[i])
        } else {
          sum(mw_all[i]) / sw
        }
      },
      numeric(1)
    )
  } else if (identical(mzFun, base::mean)) {
    vapply(peak_groups, function(i) sum(mz_raw[i]) / length(i), numeric(1))
  } else {
    vapply(peak_groups, function(i) mzFun(mz_raw[i]), numeric(1))
  }
}

#' @noRd
.peak_group_intensities <- function(peak_groups, int_raw, intensityFun) {
  vapply(peak_groups, function(i) intensityFun(int_raw[i]), numeric(1))
}

#' @noRd
.peak_group_metadata <- function(peak_groups, meta) {
  cols <- as.list(as.data.frame(meta, stringsAsFactors = FALSE))
  lapply(peak_groups, function(i) {
    list2DF(lapply(cols, function(col) {
      u <- unique(col[i])
      if (length(u) == 1L) u else NA
    }))
  })
}

#' @noRd
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
  intensity_exponent = 3,
  ...
) {
  if (!spectrumMsLevel %in% msLevel || !length(x)) {
    return(x)
  }
  mz_raw <- x[, "mz"]
  int_raw <- x[, "intensity"]
  mz_vals <- if (timeDomain) sqrt(mz_raw) else mz_raw
  mz_base <- if (timeDomain) min(mz_vals) else 1
  grps <- MsCoreUtils::group(
    x = mz_vals,
    tolerance = tolerance / mz_base,
    ppm = ppm
  )
  ## Nothing to combine when every m/z value landed in its own group.
  if (anyDuplicated(grps) == 0L) {
    return(x)
  }
  grp_idx <- .peak_group_indices(grps)
  peak_groups <- .split_peak_groups(grp_idx, int_raw = int_raw)
  peak_names <- .peak_group_names(peak_groups)

  mzs <- .peak_group_mzs(
    peak_groups = peak_groups,
    mz_raw = mz_raw,
    int_raw = int_raw,
    weighted = weighted,
    intensity_exponent = intensity_exponent,
    mzFun = mzFun
  )
  names(mzs) <- peak_names

  ints <- .peak_group_intensities(peak_groups, int_raw, intensityFun)
  names(ints) <- peak_names

  if (ncol(x) > 2L) {
    meta <- x[, !colnames(x) %in% c("mz", "intensity"), drop = FALSE]
    meta_combined <- .peak_group_metadata(peak_groups, meta)
    meta_final <- do.call(rbind, meta_combined)
    rownames(meta_final) <- peak_names
    cbind(mz = mzs, intensity = ints, meta_final)
  } else {
    cbind(mz = mzs, intensity = ints)
  }
}

#' @noRd
.fix_xml <- function(file_path) {
  temp_file <- tempfile(tmpdir = dirname(file_path))
  on.exit(unlink(temp_file), add = TRUE)
  lines <- readLines(file_path, warn = FALSE)
  run_id <- basename(file_path)
  lines <- gsub(
    pattern = "value=\"nan\"",
    replacement = "value=\"NaN\"",
    x = lines,
    fixed = TRUE,
    useBytes = TRUE
  )
  lines <- gsub(
    pattern = "<run id=\"Experiment_1\"",
    replacement = sprintf("<run id=\"%s\"", run_id),
    x = lines,
    fixed = TRUE,
    useBytes = TRUE
  )
  writeLines(lines, temp_file, useBytes = TRUE)
  if (!file.rename(temp_file, file_path)) {
    file.copy(temp_file, file_path, overwrite = TRUE)
  }
  invisible(TRUE)
}

#' @noRd
.keep_empty <- function(original, processed, order_map = seq_along(processed)) {
  processed_peaks <- Spectra::peaksData(processed)
  is_empty <- lengths(processed_peaks) == 0L
  if (any(is_empty)) {
    .log_trace(
      paste0("Restoring ", sum(is_empty), " empty spectra to original")
    )
    if (length(order_map) != length(processed_peaks)) {
      stop(
        "order_map must have one entry per processed spectrum, got ",
        length(order_map),
        " and ",
        length(processed_peaks),
        "."
      )
    }
    empty_indices <- which(is_empty)
    orig_subset <- Spectra::peaksData(original[order_map[empty_indices]])
    restored_peaks <- lapply(seq_along(empty_indices), function(k) {
      pk <- orig_subset[[k]]
      if (nrow(pk) == 0L) {
        return(pk)
      }
      pk[pk[, "intensity"] > 0, , drop = FALSE]
    })
    processed@backend@peaksData[empty_indices] <- restored_peaks
  }
  processed
}

#' @noRd
.process_spectra <- function(
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
  time_domain,
  intensity_exponent = 3
) {
  ## concatenateSpectra() below emits the MS1 spectra before the MS2 spectra, so
  ## the processed order only matches the acquisition order when the input is
  ## already grouped by MS level. Record which input spectrum each processed
  ## spectrum came from so .keep_empty() can restore the right one.
  ms_levels <- Spectra::msLevel(spectra)
  idx_1 <- which(ms_levels == 1L)
  idx_2 <- which(ms_levels == 2L)
  order_map <- c(idx_1, idx_2)

  proc_level <- function(sub_sp, tol_da, tol_ppm, int_fun, mz_fun, ms_level) {
    sub_sp |>
      Spectra::addProcessing(
        .peaks_combine,
        tolerance = tol_da,
        ppm = tol_ppm,
        intensityFun = int_fun,
        mzFun = mz_fun,
        weighted = mz_weighted,
        timeDomain = time_domain,
        intensity_exponent = intensity_exponent,
        msLevel. = ms_level
      ) |>
      Spectra::filterIntensity(intensity = c(.Machine$double.eps, Inf)) |>
      Spectra::applyProcessing(BPPARAM = BiocParallel::SerialParam())
  }

  centroided <- if (length(idx_1) && length(idx_2)) {
    Spectra::concatenateSpectra(
      proc_level(
        spectra[idx_1],
        mz_tol_da_ms1,
        mz_tol_ppm_ms1,
        custom_int_fun_ms1,
        mz_fun_ms1,
        1L
      ),
      proc_level(
        spectra[idx_2],
        mz_tol_da_ms2,
        mz_tol_ppm_ms2,
        custom_int_fun_ms2,
        mz_fun_ms2,
        2L
      )
    )
  } else if (length(idx_1)) {
    proc_level(
      spectra[idx_1],
      mz_tol_da_ms1,
      mz_tol_ppm_ms1,
      custom_int_fun_ms1,
      mz_fun_ms1,
      1L
    )
  } else if (length(idx_2)) {
    proc_level(
      spectra[idx_2],
      mz_tol_da_ms2,
      mz_tol_ppm_ms2,
      custom_int_fun_ms2,
      mz_fun_ms2,
      2L
    )
  } else {
    spectra[integer(0)]
  }
  .keep_empty(spectra, centroided, order_map)
}

#' @noRd
.process_spectra_batches <- function(
  spectra,
  outf,
  outd,
  min_datapoints_ms1,
  min_datapoints_ms2,
  mz_tol_da_ms1,
  mz_tol_da_ms2,
  mz_tol_ppm_ms1,
  mz_tol_ppm_ms2,
  mz_fun_ms1,
  mz_fun_ms2,
  int_fun_ms1,
  int_fun_ms2,
  mz_weighted,
  time_domain,
  intensity_exponent,
  batch_size = 4096L
) {
  setup_logger(dir = outd)
  make_int_fun <- function(int_fun, min_datapoints) {
    force(int_fun)
    force(min_datapoints)
    if (min_datapoints <= 1L) {
      function(intensities) {
        if (any(intensities > 0, na.rm = TRUE)) int_fun(intensities) else 0
      }
    } else {
      function(intensities) {
        if (
          length(intensities) >= min_datapoints &&
            sum(intensities > 0, na.rm = TRUE) >= min_datapoints
        ) {
          int_fun(intensities)
        } else {
          0
        }
      }
    }
  }
  custom_int_fun_ms1 <- make_int_fun(int_fun_ms1, min_datapoints_ms1)
  custom_int_fun_ms2 <- make_int_fun(int_fun_ms2, min_datapoints_ms2)
  ## seq() below is only well defined for a non-empty input, and there is no
  ## work to do for an empty file anyway. Fail with a readable reason rather
  ## than letting seq() report an unrelated "wrong sign in 'by' argument".
  if (length(spectra) == 0L) {
    stop("Input contains no spectra to centroid.")
  }
  batch_starts <- seq(1L, length(spectra), by = batch_size)
  tmp_batch_dir <- file.path(outd, "tmp")
  if (!dir.exists(tmp_batch_dir)) {
    dir.create(tmp_batch_dir, recursive = TRUE)
  }
  on.exit(unlink(tmp_batch_dir, recursive = TRUE), add = TRUE)
  serial_param <- BiocParallel::SerialParam()
  temp_files <- vapply(
    seq_along(batch_starts),
    function(i) {
      start_idx <- batch_starts[[i]]
      idx <- start_idx:min(start_idx + batch_size - 1L, length(spectra))
      sp_batch <- spectra[idx] |>
        Spectra::setBackend(Spectra::MsBackendMemory(), BPPARAM = serial_param)
      .log_trace(
        paste0("Processing batch ", i, " / ", length(batch_starts))
      )
      result <- .process_spectra(
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
        time_domain = time_domain,
        intensity_exponent = intensity_exponent
      )
      result@backend@spectraData$centroided <- TRUE
      temp_file <- tempfile(fileext = ".mzML", tmpdir = tmp_batch_dir)
      Spectra::export(
        result,
        file = temp_file,
        backend = Spectra::MsBackendMzR(),
        BPPARAM = serial_param
      )
      temp_file
    },
    character(1)
  )
  .log_trace("Concatenating all processed batches")
  .log_trace(paste0("Exporting: ", basename(outf)))
  ## Export to a temporary file and publish it with a rename, both inside `outd`
  ## so the rename stays on one filesystem. When all spectra fit in a single
  ## batch, the batch output is already a complete mzML file in `tmp_batch_dir`,
  ## so we can publish it directly without re-reading and re-exporting.
  if (length(temp_files) == 1L) {
    tmp_outf <- temp_files[[1L]]
  } else {
    sp_cen <- Spectra::Spectra(
      temp_files,
      backend = Spectra::MsBackendMzR(),
      BPPARAM = serial_param
    )
    tmp_outf <- tempfile(
      pattern = "centroid_",
      tmpdir = tmp_batch_dir,
      fileext = ".mzML"
    )
    Spectra::export(
      sp_cen,
      file = tmp_outf,
      backend = Spectra::MsBackendMzR(),
      BPPARAM = serial_param
    )
    rm(sp_cen)
  }
  if (!file.rename(tmp_outf, outf)) {
    stop("Failed to move exported file from ", tmp_outf, " to ", outf)
  }
  .log_trace(paste0("Exported: ", basename(outf)))
  .log_trace(paste0("Making a few fixes inside mzML: ", basename(outf)))
  .fix_xml(outf)
  .log_trace(paste0("Made fixes inside mzML: ", basename(outf)))
  .log_success(paste0("Successfully centroided: ", basename(outf)))
  TRUE
}
