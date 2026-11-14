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
#' @param centroid_method character(1). Mathematical estimator for peak m/z
#'   when `mz_weighted = TRUE`: `"weighted"` (power-weighted mean, default) or
#'   `"gaussian"` (apex-centered weighted log-quadratic Guo-Caruana Gaussian
#'   fit with automatic fallback to power-weighted mean on < 3 points or
#'   non-concave profiles).
#' @param intensity_method character(1). Mathematical estimator for peak
#'   abundance: `"func"` (applies `int_fun_ms1` / `int_fun_ms2`, default),
#'   `"gaussian_apex"` (interpolated continuous Gaussian apex height to
#'   eliminate picket-fence sampling attenuation), `"gaussian_area"`
#'   (analytical Gaussian integral \eqn{A \sigma \sqrt{2\pi}}), or
#'   `"trapezoid"` (numerical trapezoidal integration over m/z).
#' @param deconvolve logical(1). If `TRUE`, applies 2-pass Gauss-Seidel
#'   Gaussian tail subtraction across split sub-peaks within overlapping
#'   multiplet groups before estimating centroids and intensities. Default:
#'   FALSE.
#' @param valley_ratio numeric(1). Maximum ratio of valley intensity to the
#'   topological basin barrier height (1D Morse-Smale persistence) at which a
#'   group is split into separate peaks. Must be in `[0, 1]`. Default: 0.2.
#' @param valley_hws integer(1). Half-window size for local maxima detection
#'   during valley splitting. Must be at least 1. Default: 2L.
#' @param baseline_subtract logical(1). If `TRUE`, subtracts the local edge
#'   baseline floor of each peak group prior to valley splitting and centroid
#'   estimation. Default: FALSE.
#' @param domain_scaling character(1). Mass analyzer spacing scaling model used
#'   when grouping adjacent profile points: `"legacy"` (honours `time_domain`
#'   via `sqrt(mz)` transformation, default), `"linear"` (constant Da + ppm in
#'   m/z space), `"tof"` (local Jacobian \eqn{\Delta m \propto m^{1/2}}),
#'   `"orbitrap"` (local Jacobian \eqn{\Delta m \propto m^{3/2}}), or
#'   `"fticr"` (local Jacobian \eqn{\Delta m \propto m^2}).
#' @param mz_ref numeric(1). Reference m/z (in Daltons) at which `mz_tol_da_*`
#'   is calibrated when `domain_scaling` is `"tof"`, `"orbitrap"`, or
#'   `"fticr"`. Default: 100.
#'
#' @return logical(1). `TRUE` if the file was centroided, or `TRUE` if it was
#'   skipped because the output file already existed; `FALSE` if the input was
#'   missing or if centroiding failed. Invalid argument types raise an error
#'   instead of returning `FALSE`.
#'
#' @details
#' MS1 and MS2 spectra are processed separately with their own tolerances, so
#' the two levels can be tuned independently while preserving the original
#' chronological scan acquisition order in the exported `mzML`.
#'
#' Within a spectrum, profile points are first grouped by m/z tolerance using
#' either the legacy `sqrt(mz)` transformation (`domain_scaling = "legacy"`) or
#' an exact local Jacobian analyzer spacing law (`domain_scaling` in `"linear"`,
#' `"tof"`, `"orbitrap"`, or `"fticr"`). Each group is then recursively split at
#' deep valleys evaluated via 1D Morse-Smale topological basin prominence
#' (`valley_ratio`, `valley_hws`, and optional `baseline_subtract`) so that
#' closely spaced peaks are not merged even when small noise ripples sit inside
#' a valley.
#'
#' Peak centroids are estimated via power-weighted mean (`centroid_method =
#' "weighted"`), apex-centered weighted log-quadratic Guo-Caruana Gaussian
#' fitting (`centroid_method = "gaussian"`), or custom aggregation (`"func"`),
#' while peak abundances can be reported via `int_fun_ms*` (`intensity_method =
#' "func"`), interpolated Gaussian apex (`"gaussian_apex"`), analytical Gaussian
#' area (`"gaussian_area"`), or numerical trapezoidal integration
#' (`"trapezoid"`). Setting `deconvolve = TRUE` performs 2-pass Gauss-Seidel
#' Gaussian tail subtraction across split sub-peaks prior to centroid and
#' intensity estimation.
#'
#' A group is only reduced when it holds at least `min_datapoints_ms*` points of
#' non-zero intensity; below that it is reported as zero intensity and filtered
#' out. Spectra left without any peak by this step are restored from the input,
#' so no spectrum is lost.
#'
#' File path modifications are supported via `pattern` and `replacement`.
#' `pattern` is a regular expression, as in `sub()`.
#'
#' @section EDAM Ontology:
#' - **Topics**: `http://edamontology.org/topic_3172` (Metabolomics),
#'   `http://edamontology.org/topic_0153` (Lipids),
#'   `http://edamontology.org/topic_0154` (Small molecules),
#'   `http://edamontology.org/topic_3370` (Analytical chemistry)
#' - **Operations**: `http://edamontology.org/operation_3215` (Peak detection /
#'   Peak picking / Centroiding), `http://edamontology.org/operation_3214`
#'   (Spectral analysis), `http://edamontology.org/operation_3695`
#'   (Data filtering)
#' - **Input/Output Data**: `http://edamontology.org/data_0943` (Mass spectrum),
#'   `http://edamontology.org/data_2536` (Mass spectrometry data)
#' - **Input/Output Format**: `http://edamontology.org/format_3244` (mzML)
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
  intensity_exponent = 3,
  centroid_method = c("weighted", "gaussian", "func"),
  intensity_method = c("func", "gaussian_apex", "gaussian_area", "trapezoid"),
  deconvolve = FALSE,
  valley_ratio = 0.2,
  valley_hws = 2L,
  baseline_subtract = FALSE,
  domain_scaling = c("legacy", "linear", "tof", "orbitrap", "fticr"),
  mz_ref = 100
) {
  # Input validation for critical arguments
  stopifnot(
    is.character(file),
    length(file) == 1L,
    !is.na(file),
    nzchar(file),
    is.character(pattern),
    length(pattern) == 1L,
    !is.na(pattern),
    is.character(replacement),
    length(replacement) == 1L,
    !is.na(replacement),
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
    is.finite(mz_tol_da_ms1),
    mz_tol_da_ms1 >= 0,
    is.numeric(mz_tol_da_ms2),
    length(mz_tol_da_ms2) == 1L,
    is.finite(mz_tol_da_ms2),
    mz_tol_da_ms2 >= 0,
    is.numeric(mz_tol_ppm_ms1),
    length(mz_tol_ppm_ms1) == 1L,
    is.finite(mz_tol_ppm_ms1),
    mz_tol_ppm_ms1 >= 0,
    is.numeric(mz_tol_ppm_ms2),
    length(mz_tol_ppm_ms2) == 1L,
    is.finite(mz_tol_ppm_ms2),
    mz_tol_ppm_ms2 >= 0,
    is.function(mz_fun_ms1),
    is.function(mz_fun_ms2),
    is.function(int_fun_ms1),
    is.function(int_fun_ms2),
    is.logical(mz_weighted),
    length(mz_weighted) == 1L,
    !is.na(mz_weighted),
    is.logical(time_domain),
    length(time_domain) == 1L,
    !is.na(time_domain),
    is.numeric(intensity_exponent),
    length(intensity_exponent) == 1L,
    is.finite(intensity_exponent),
    intensity_exponent >= 0,
    is.character(centroid_method),
    !anyNA(centroid_method),
    is.character(intensity_method),
    !anyNA(intensity_method),
    is.logical(deconvolve),
    length(deconvolve) == 1L,
    !is.na(deconvolve),
    is.numeric(valley_ratio),
    length(valley_ratio) == 1L,
    is.finite(valley_ratio),
    valley_ratio >= 0,
    valley_ratio <= 1,
    is.numeric(valley_hws),
    length(valley_hws) == 1L,
    valley_hws >= 1,
    valley_hws %% 1 == 0,
    is.logical(baseline_subtract),
    length(baseline_subtract) == 1L,
    !is.na(baseline_subtract),
    is.character(domain_scaling),
    !anyNA(domain_scaling),
    is.numeric(mz_ref),
    length(mz_ref) == 1L,
    is.finite(mz_ref),
    mz_ref > 0
  )
  centroid_method <- match.arg(centroid_method)
  intensity_method <- match.arg(intensity_method)
  domain_scaling <- match.arg(domain_scaling)
  valley_hws <- as.integer(valley_hws)

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
    "Intensity exponent" = intensity_exponent,
    "Centroid method" = centroid_method,
    "Intensity method" = intensity_method,
    "Deconvolve" = deconvolve,
    "Valley ratio" = valley_ratio,
    "Valley hws" = valley_hws,
    "Baseline subtract" = baseline_subtract,
    "Domain scaling" = domain_scaling,
    "Reference m/z" = mz_ref
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
        intensity_exponent = intensity_exponent,
        centroid_method = centroid_method,
        intensity_method = intensity_method,
        deconvolve = deconvolve,
        valley_ratio = valley_ratio,
        valley_hws = valley_hws,
        baseline_subtract = baseline_subtract,
        domain_scaling = domain_scaling,
        mz_ref = mz_ref
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
  .centroidr_log$appender <- .appender_stdout
  .centroidr_log$threshold <- 400L
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
.split_peak_group <- function(
  idx,
  int_raw,
  valley_ratio = 0.2,
  hws = 2L,
  baseline_subtract = FALSE
) {
  if (length(idx) < 3L) {
    return(list(idx))
  }
  ints <- int_raw[idx]
  if (baseline_subtract) {
    base_floor <- min(ints[1L], ints[length(ints)])
    if (is.finite(base_floor) && base_floor > 0) {
      ints <- pmax(0, ints - base_floor)
    }
  }
  maxima <- which(MsCoreUtils::localMaxima(ints, hws = hws))
  if (length(maxima) < 2L) {
    return(list(idx))
  }
  n_valleys <- length(maxima) - 1L
  v_locs <- integer(n_valleys)
  v_vals <- numeric(n_valleys)
  for (j in seq_len(n_valleys)) {
    left <- maxima[[j]]
    right <- maxima[[j + 1L]]
    v_locs[[j]] <- left + which.min(ints[left:right]) - 1L
    v_vals[[j]] <- ints[v_locs[[j]]]
  }
  split_at <- NA_integer_
  split_score <- Inf
  for (j in seq_len(n_valleys)) {
    v_val <- v_vals[[j]]
    ## 1D Morse-Smale topological basin barrier: pair saddle v_j with the
    ## highest maxima in its left and right basins bounded by any strictly
    ## deeper valley, so small noise ripples inside a valley do not mask two
    ## well-separated major peaks.
    k_left <- j - 1L
    while (k_left >= 1L && v_vals[[k_left]] >= v_val) {
      k_left <- k_left - 1L
    }
    left_max <- max(ints[maxima[(k_left + 1L):j]])

    k_right <- j + 1L
    while (k_right <= n_valleys && v_vals[[k_right]] >= v_val) {
      k_right <- k_right + 1L
    }
    right_max <- max(ints[maxima[(j + 1L):k_right]])

    peak_floor <- min(left_max, right_max)
    if (peak_floor <= 0) {
      next
    }
    score <- v_val / peak_floor
    if (score <= valley_ratio && score < split_score) {
      split_at <- v_locs[[j]]
      split_score <- score
    }
  }
  if (is.na(split_at)) {
    return(list(idx))
  }
  c(
    .split_peak_group(
      idx[seq_len(split_at)],
      int_raw,
      valley_ratio,
      hws,
      baseline_subtract
    ),
    .split_peak_group(
      idx[(split_at + 1L):length(idx)],
      int_raw,
      valley_ratio,
      hws,
      baseline_subtract
    )
  )
}

#' @noRd
.subtract_baseline <- function(ints, baseline_subtract = FALSE) {
  n <- length(ints)
  base_applied <- 0
  if (baseline_subtract && n >= 3L) {
    base_floor <- min(ints[1L], ints[n])
    if (is.finite(base_floor) && base_floor > 0 && base_floor < max(ints)) {
      ints <- pmax(0, ints - base_floor)
      base_applied <- base_floor
    }
  }
  list(ints = ints, base_applied = base_applied)
}

#' @noRd
.intensity_weights <- function(ints, exponent) {
  pmax(0, ints)^exponent
}

#' @noRd
.weighted_centroid <- function(
  mz,
  ints,
  exponent,
  baseline_subtract,
  mzFun
) {
  ints_adj <- .subtract_baseline(ints, baseline_subtract)$ints
  w <- .intensity_weights(ints_adj, exponent)
  w_sum <- sum(w)
  if (w_sum == 0) mzFun(mz) else sum(mz * w) / w_sum
}

#' @noRd
.fit_gaussian_peak <- function(
  mz,
  ints,
  exponent = 3,
  baseline_subtract = FALSE,
  mzFun = base::mean,
  intensityFun = base::max
) {
  n <- length(mz)
  if (n == 0L) {
    return(list(
      mz = NA_real_,
      amp = 0,
      apex = 0,
      sigma = 0,
      area = 0,
      valid = FALSE
    ))
  }
  bs <- .subtract_baseline(ints, baseline_subtract)
  ints_adj <- bs$ints
  base_applied <- bs$base_applied
  w_all <- .intensity_weights(ints_adj, exponent)
  w_sum <- sum(w_all)
  fallback_mz <- if (w_sum == 0) mzFun(mz) else sum(mz * w_all) / w_sum
  fallback_apex <- intensityFun(ints)

  apex_i <- which.max(ints_adj)
  if (n >= 3L && length(apex_i) == 1L && ints_adj[apex_i] > 0) {
    core <- which(ints_adj >= 0.05 * ints_adj[apex_i] & ints_adj > 0)
    if (length(core) < 3L) {
      left_i <- max(1L, apex_i - 1L)
      right_i <- min(n, apex_i + 1L)
      core <- seq.int(left_i, right_i)
      core <- core[ints_adj[core] > 0]
    }
    if (length(core) >= 3L) {
      m_c <- mz[core]
      i_c <- ints_adj[core]
      m0 <- mz[apex_i]
      raw_x <- m_c - m0
      h <- max(abs(raw_x))
      if (is.finite(h) && h > 0) {
        x <- raw_x / h
        y <- log(i_c)
        w <- .intensity_weights(i_c / max(i_c), exponent)
        s0 <- sum(w)
        s1 <- sum(w * x)
        s2 <- sum(w * x^2)
        s3 <- sum(w * x^3)
        s4 <- sum(w * x^4)
        t0 <- sum(w * y)
        t1 <- sum(w * x * y)
        t2 <- sum(w * x^2 * y)

        det_a <- s0 *
          (s2 * s4 - s3^2) -
          s1 * (s1 * s4 - s2 * s3) +
          s2 * (s1 * s3 - s2^2)
        if (is.finite(det_a) && abs(det_a) > 1e-12) {
          c0 <- (t0 *
            (s2 * s4 - s3^2) -
            s1 * (t1 * s4 - t2 * s3) +
            s2 * (t1 * s3 - t2 * s2)) /
            det_a
          c1 <- (s0 *
            (t1 * s4 - t2 * s3) -
            t0 * (s1 * s4 - s2 * s3) +
            s2 * (s1 * t2 - s2 * t1)) /
            det_a
          c2 <- (s0 *
            (s2 * t2 - s3 * t1) -
            s1 * (s1 * t2 - s2 * t1) +
            t0 * (s1 * s3 - s2^2)) /
            det_a
          if (is.finite(c2) && c2 < 0) {
            delta <- -c1 / (2 * c2) * h
            mz_est <- m0 + delta
            if (is.finite(mz_est) && mz_est >= min(m_c) && mz_est <= max(m_c)) {
              amp_est <- exp(c0 - c1^2 / (4 * c2))
              apex_est <- amp_est + base_applied
              sigma_est <- sqrt(-1 / (2 * c2)) * h
              area_est <- amp_est * sigma_est * sqrt(2 * pi)
              return(list(
                mz = mz_est,
                amp = amp_est,
                apex = apex_est,
                sigma = sigma_est,
                area = area_est,
                valid = TRUE
              ))
            }
          }
        }
      }
    }
  }
  list(
    mz = fallback_mz,
    amp = intensityFun(ints_adj),
    apex = fallback_apex,
    sigma = if (n >= 2L) 0.5 * diff(range(mz)) else 0,
    area = .trapezoid_area(mz, ints_adj, baseline_subtract = FALSE),
    valid = FALSE
  )
}

#' @noRd
.trapezoid_area <- function(mz, ints, baseline_subtract = FALSE) {
  n <- length(mz)
  if (n == 0L) {
    return(0)
  }
  if (n == 1L) {
    return(max(0, ints[1L]))
  }
  ints_adj <- .subtract_baseline(ints, baseline_subtract)$ints
  ord <- order(mz)
  m_s <- mz[ord]
  i_s <- pmax(0, ints_adj[ord])
  area <- sum(0.5 * (i_s[-n] + i_s[-1L]) * diff(m_s))
  if (area <= 0 && any(i_s > 0)) max(i_s) else area
}

#' @noRd
.deconvolve_subpeaks <- function(
  segs,
  mz_raw,
  int_raw,
  exponent = 3,
  baseline_subtract = FALSE,
  mzFun = base::mean,
  intensityFun = base::max
) {
  k_n <- length(segs)
  fits <- lapply(segs, function(idx) {
    .fit_gaussian_peak(
      mz_raw[idx],
      int_raw[idx],
      exponent = exponent,
      baseline_subtract = baseline_subtract,
      mzFun = mzFun,
      intensityFun = intensityFun
    )
  })
  if (k_n < 2L) {
    for (k in seq_len(k_n)) {
      fits[[k]]$ints_corr <- int_raw[segs[[k]]]
    }
    return(fits)
  }
  all_grp <- unlist(segs, use.names = FALSE)
  fallback_sigma <- max(1e-6, 0.5 * diff(range(mz_raw[all_grp])) / k_n)
  sigmas <- vapply(
    fits,
    function(f) if (f$sigma > 0) f$sigma else fallback_sigma,
    numeric(1)
  )
  mu0 <- vapply(fits, `[[`, numeric(1), "mz")
  amp0 <- vapply(fits, `[[`, numeric(1), "amp")
  for (iter in 1:2) {
    for (k in seq_len(k_n)) {
      idx <- segs[[k]]
      ext_idx <- sort(unique(c(
        max(min(all_grp), min(idx) - 1L),
        idx,
        min(max(all_grp), max(idx) + 1L)
      )))
      m_sub <- mz_raw[ext_idx]
      i_corr <- int_raw[ext_idx]
      for (j in seq_len(k_n)) {
        if (j != k && is.finite(sigmas[j]) && sigmas[j] > 0) {
          tail_j <- amp0[j] * exp(-0.5 * ((m_sub - mu0[j]) / sigmas[j])^2)
          i_corr <- pmax(0, i_corr - tail_j)
        }
      }
      fit_k <- .fit_gaussian_peak(
        m_sub,
        i_corr,
        exponent = exponent,
        baseline_subtract = baseline_subtract,
        mzFun = mzFun,
        intensityFun = intensityFun
      )
      fit_k$ints_corr <- i_corr[match(idx, ext_idx)]
      mu0[k] <- fit_k$mz
      amp0[k] <- fit_k$amp
      if (fit_k$valid && is.finite(fit_k$sigma) && fit_k$sigma > 0) {
        sigmas[k] <- fit_k$sigma
      }
      fits[[k]] <- fit_k
    }
  }
  fits
}

#' @noRd
.group_mz_domain <- function(
  mz_raw,
  tolerance = 0,
  ppm = 20,
  timeDomain = FALSE,
  domain_scaling = "legacy",
  mz_ref = 100
) {
  n <- length(mz_raw)
  if (n == 0L) {
    return(integer(0))
  }
  if (n == 1L) {
    return(1L)
  }
  domain_scaling <- match.arg(
    domain_scaling,
    c("legacy", "linear", "tof", "orbitrap", "fticr")
  )
  if (domain_scaling == "legacy") {
    mz_vals <- if (timeDomain) sqrt(mz_raw) else mz_raw
    mz_base <- if (timeDomain) min(mz_vals) else 1
    return(MsCoreUtils::group(
      x = mz_vals,
      tolerance = tolerance / mz_base,
      ppm = ppm
    ))
  }
  alpha <- switch(
    domain_scaling,
    linear = 0,
    tof = 0.5,
    orbitrap = 1.5,
    fticr = 2.0
  )
  dm <- diff(mz_raw)
  m_mid <- 0.5 * (mz_raw[-n] + mz_raw[-1L])
  tol_local <- tolerance *
    (pmax(m_mid, 1e-9) / mz_ref)^alpha +
    (ppm * 1e-6) * pmax(m_mid, 0) +
    sqrt(.Machine$double.eps)
  cumsum(c(1L, as.integer(dm > tol_local)))
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
  mzFun,
  centroid_method = "weighted",
  baseline_subtract = FALSE
) {
  if (!weighted || identical(centroid_method, "func")) {
    return(vapply(peak_groups, function(i) mzFun(mz_raw[i]), numeric(1)))
  }
  if (identical(centroid_method, "gaussian")) {
    return(vapply(
      peak_groups,
      function(i) {
        .fit_gaussian_peak(
          mz = mz_raw[i],
          ints = int_raw[i],
          exponent = intensity_exponent,
          baseline_subtract = baseline_subtract,
          mzFun = mzFun
        )$mz
      },
      numeric(1)
    ))
  }
  vapply(
    peak_groups,
    function(i) {
      .weighted_centroid(
        mz = mz_raw[i],
        ints = int_raw[i],
        exponent = intensity_exponent,
        baseline_subtract = baseline_subtract,
        mzFun = mzFun
      )
    },
    numeric(1)
  )
}

#' @noRd
.peak_group_intensities <- function(
  peak_groups,
  int_raw,
  intensityFun,
  mz_raw = NULL,
  intensity_method = "func",
  intensity_exponent = 3,
  baseline_subtract = FALSE
) {
  if (identical(intensity_method, "func") || is.null(mz_raw)) {
    return(vapply(
      peak_groups,
      function(i) intensityFun(int_raw[i]),
      numeric(1)
    ))
  }
  vapply(
    peak_groups,
    function(i) {
      gate <- intensityFun(int_raw[i])
      if (gate <= 0) {
        return(0)
      }
      if (identical(intensity_method, "trapezoid")) {
        return(.trapezoid_area(
          mz_raw[i],
          int_raw[i],
          baseline_subtract = baseline_subtract
        ))
      }
      fit <- .fit_gaussian_peak(
        mz = mz_raw[i],
        ints = int_raw[i],
        exponent = intensity_exponent,
        baseline_subtract = baseline_subtract,
        intensityFun = intensityFun
      )
      if (identical(intensity_method, "gaussian_area")) {
        fit$area
      } else {
        fit$apex
      }
    },
    numeric(1)
  )
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
  centroid_method = "weighted",
  intensity_method = "func",
  deconvolve = FALSE,
  valley_ratio = 0.2,
  valley_hws = 2L,
  baseline_subtract = FALSE,
  domain_scaling = "legacy",
  mz_ref = 100,
  ...
) {
  if (!spectrumMsLevel %in% msLevel || !length(x)) {
    return(x)
  }
  mz_raw <- x[, "mz"]
  int_raw <- x[, "intensity"]
  grps <- .group_mz_domain(
    mz_raw = mz_raw,
    tolerance = tolerance,
    ppm = ppm,
    timeDomain = timeDomain,
    domain_scaling = domain_scaling,
    mz_ref = mz_ref
  )
  ## Nothing to combine when every m/z value landed in its own group.
  if (anyDuplicated(grps) == 0L) {
    return(x)
  }
  grp_idx <- .peak_group_indices(grps)
  split_by_grp <- lapply(
    grp_idx,
    .split_peak_group,
    int_raw = int_raw,
    valley_ratio = valley_ratio,
    hws = valley_hws,
    baseline_subtract = baseline_subtract
  )
  peak_groups <- unlist(split_by_grp, recursive = FALSE, use.names = FALSE)
  peak_names <- .peak_group_names(peak_groups)

  is_split <- if (deconvolve) {
    rep(lengths(split_by_grp) > 1L, lengths(split_by_grp))
  } else {
    logical(length(peak_groups))
  }
  unsplit_idx <- !is_split
  mzs <- numeric(length(peak_groups))
  ints <- numeric(length(peak_groups))
  if (any(unsplit_idx)) {
    mzs[unsplit_idx] <- .peak_group_mzs(
      peak_groups = peak_groups[unsplit_idx],
      mz_raw = mz_raw,
      int_raw = int_raw,
      weighted = weighted,
      intensity_exponent = intensity_exponent,
      mzFun = mzFun,
      centroid_method = centroid_method,
      baseline_subtract = baseline_subtract
    )
    ints[unsplit_idx] <- .peak_group_intensities(
      peak_groups = peak_groups[unsplit_idx],
      int_raw = int_raw,
      intensityFun = intensityFun,
      mz_raw = mz_raw,
      intensity_method = intensity_method,
      intensity_exponent = intensity_exponent,
      baseline_subtract = baseline_subtract
    )
  }
  if (any(is_split)) {
    dec_fits <- unlist(
      lapply(
        split_by_grp[lengths(split_by_grp) > 1L],
        .deconvolve_subpeaks,
        mz_raw = mz_raw,
        int_raw = int_raw,
        exponent = intensity_exponent,
        baseline_subtract = baseline_subtract,
        mzFun = mzFun,
        intensityFun = intensityFun
      ),
      recursive = FALSE
    )
    split_pos <- which(is_split)
    mzs[split_pos] <- vapply(
      seq_along(split_pos),
      function(j) {
        idx <- peak_groups[[split_pos[j]]]
        if (!weighted || identical(centroid_method, "func")) {
          mzFun(mz_raw[idx])
        } else if (identical(centroid_method, "gaussian")) {
          dec_fits[[j]]$mz
        } else {
          .weighted_centroid(
            mz = mz_raw[idx],
            ints = dec_fits[[j]]$ints_corr,
            exponent = intensity_exponent,
            baseline_subtract = baseline_subtract,
            mzFun = mzFun
          )
        }
      },
      numeric(1)
    )
    ints[split_pos] <- vapply(
      seq_along(split_pos),
      function(j) {
        idx <- peak_groups[[split_pos[j]]]
        if (intensityFun(int_raw[idx]) <= 0) {
          return(0)
        }
        ints_j <- dec_fits[[j]]$ints_corr
        if (identical(intensity_method, "gaussian_area")) {
          dec_fits[[j]]$area
        } else if (identical(intensity_method, "gaussian_apex")) {
          dec_fits[[j]]$apex
        } else if (identical(intensity_method, "trapezoid")) {
          .trapezoid_area(mz_raw[idx], ints_j, baseline_subtract)
        } else {
          intensityFun(ints_j)
        }
      },
      numeric(1)
    )
  }
  names(mzs) <- peak_names
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
  file.rename(temp_file, file_path)
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
  intensity_exponent = 3,
  centroid_method = "weighted",
  intensity_method = "func",
  deconvolve = FALSE,
  valley_ratio = 0.2,
  valley_hws = 2L,
  baseline_subtract = FALSE,
  domain_scaling = "legacy",
  mz_ref = 100
) {
  ## concatenateSpectra() below emits the MS1 spectra before the MS2 spectra, so
  ## the processed order only matches the acquisition order when the input is
  ## already grouped by MS level. Record which input spectrum each processed
  ## spectrum came from so .keep_empty() can restore the right one and
  ## chronological scan order can be restored afterwards.
  ms_levels <- Spectra::msLevel(spectra)
  order_map <- c(which(ms_levels == 1L), which(ms_levels == 2L))
  serial_param <- BiocParallel::SerialParam()

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
      intensity_exponent = intensity_exponent,
      centroid_method = centroid_method,
      intensity_method = intensity_method,
      deconvolve = deconvolve,
      valley_ratio = valley_ratio,
      valley_hws = valley_hws,
      baseline_subtract = baseline_subtract,
      domain_scaling = domain_scaling,
      mz_ref = mz_ref,
      msLevel. = 2L
    ) |>
    Spectra::filterIntensity(intensity = c(.Machine$double.eps, Inf)) |>
    Spectra::applyProcessing(BPPARAM = serial_param)
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
      intensity_exponent = intensity_exponent,
      centroid_method = centroid_method,
      intensity_method = intensity_method,
      deconvolve = deconvolve,
      valley_ratio = valley_ratio,
      valley_hws = valley_hws,
      baseline_subtract = baseline_subtract,
      domain_scaling = domain_scaling,
      mz_ref = mz_ref,
      msLevel. = 1L
    ) |>
    Spectra::filterIntensity(intensity = c(.Machine$double.eps, Inf)) |>
    Spectra::applyProcessing(BPPARAM = serial_param)
  centroided <- Spectra::concatenateSpectra(centroided_1, centroided_2)
  res <- .keep_empty(spectra, centroided, order_map)
  if (length(order_map) == length(res) && is.unsorted(order_map)) {
    res <- res[order(order_map)]
  }
  res
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
  centroid_method = "weighted",
  intensity_method = "func",
  deconvolve = FALSE,
  valley_ratio = 0.2,
  valley_hws = 2L,
  baseline_subtract = FALSE,
  domain_scaling = "legacy",
  mz_ref = 100,
  batch_size = 4096L
) {
  setup_logger(dir = outd)
  custom_int_fun <- function(int_fun, intensities, min_datapoints) {
    if (sum(intensities > 0, na.rm = TRUE) >= min_datapoints) {
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
        intensity_exponent = intensity_exponent,
        centroid_method = centroid_method,
        intensity_method = intensity_method,
        deconvolve = deconvolve,
        valley_ratio = valley_ratio,
        valley_hws = valley_hws,
        baseline_subtract = baseline_subtract,
        domain_scaling = domain_scaling,
        mz_ref = mz_ref
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
  sp_cen <- Spectra::Spectra(
    temp_files,
    backend = Spectra::MsBackendMzR(),
    BPPARAM = serial_param
  )
  ## Export to a temporary file and publish it with a rename, both inside `outd`
  ## so the rename stays on one filesystem. A torn export therefore never lands
  ## at `outf`: the next run would otherwise see that output already exists and
  ## skip the work, reporting success for a truncated file.
  tmp_outf <- tempfile(
    pattern = "centroid_",
    tmpdir = tmp_batch_dir,
    fileext = ".mzML"
  )
  .log_trace(paste0("Exporting: ", basename(outf)))
  Spectra::export(
    sp_cen,
    file = tmp_outf,
    backend = Spectra::MsBackendMzR(),
    BPPARAM = serial_param
  )
  rm(sp_cen)
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
