# CentroidR CLI: Batch or single-file centroiding for mzML files
#
# Usage:
#   Rscript centroiding.R --file <input.mzML> --pattern <pattern> \
#                         --replacement <replacement>
#   Rscript centroiding.R --directory <dir> --pattern <pattern> \
#                         --replacement <replacement>
#
# Wraps CentroidR::centroid_one_file() without external option-parsing packages.

print_cli_help <- function() {
  cat(
    "Usage: centroiding.R [options]\n\n",
    "Options:\n",
    "  -f CHARACTER, --file=CHARACTER\n",
    "      Path to the input file [exclusive with --directory]\n",
    "  -d CHARACTER, --directory=CHARACTER\n",
    "      Path to a directory of mzML files [exclusive with --file]\n",
    "  -p CHARACTER, --pattern=CHARACTER\n",
    "      Pattern to replace in file path [required]\n",
    "  -r CHARACTER, --replacement=CHARACTER\n",
    "      Replacement string [required]\n",
    "  --min-datapoints-ms1=INTEGER\n",
    "      Minimum datapoints (MS1). (default: 5)\n",
    "  --min-datapoints-ms2=INTEGER\n",
    "      Minimum datapoints (MS2). (default: 1)\n",
    "  --mz-tol-da-ms1=NUMERIC\n",
    "      m/z tolerance in Dalton (MS1). (default: 0.0025)\n",
    "  --mz-tol-da-ms2=NUMERIC\n",
    "      m/z tolerance in Dalton (MS2). (default: 0.0025)\n",
    "  --mz-tol-ppm-ms1=NUMERIC\n",
    "      m/z tolerance in ppm (MS1). (default: 5)\n",
    "  --mz-tol-ppm-ms2=NUMERIC\n",
    "      m/z tolerance in ppm (MS2). (default: 5)\n",
    "  --mz-fun-ms1=CHARACTER\n",
    "      Function to aggregate m/z values (MS1). (default: mean)\n",
    "  --mz-fun-ms2=CHARACTER\n",
    "      Function to aggregate m/z values (MS2). (default: mean)\n",
    "  --int-fun-ms1=CHARACTER\n",
    "      Function to aggregate intensity values (MS1). (default: max)\n",
    "  --int-fun-ms2=CHARACTER\n",
    "      Function to aggregate intensity values (MS2). (default: max)\n",
    "  --mz-weighted=LOGICAL\n",
    "      Weight m/z aggregation by intensity. (default: TRUE)\n",
    "  --time-domain=LOGICAL\n",
    "      Group peaks in sqrt(mz) domain instead of m/z. (default: TRUE)\n",
    "  --intensity-exponent=NUMERIC\n",
    "      Exponent applied to intensities when weighting m/z. (default: 3)\n",
    "  --centroid-method=CHARACTER\n",
    "      Centroid m/z estimator: weighted, gaussian, func.\n",
    "      (default: weighted)\n",
    "  --intensity-method=CHARACTER\n",
    "      Intensity estimator: func, gaussian_apex, gaussian_area,\n",
    "      trapezoid. (default: func)\n",
    "  --deconvolve=LOGICAL\n",
    "      Apply Gaussian tail deconvolution on split peaks.\n",
    "      (default: FALSE)\n",
    "  --valley-ratio=NUMERIC\n",
    "      Max valley-to-peak prominence ratio for splitting. (default: 0.2)\n",
    "  --valley-hws=INTEGER\n",
    "      Half-window size for local maxima detection. (default: 2)\n",
    "  --baseline-subtract=LOGICAL\n",
    "      Subtract group edge baseline before centroiding.\n",
    "      (default: FALSE)\n",
    "  --domain-scaling=CHARACTER\n",
    "      Analyzer scaling: legacy, linear, tof, orbitrap, fticr.\n",
    "      (default: legacy)\n",
    "  --mz-ref=NUMERIC\n",
    "      Reference m/z for analyzer scaling. (default: 100)\n",
    "  -h, --help\n",
    "      Show this help message and exit\n",
    sep = ""
  )
}

.parse_cli_int <- function(key, val) {
  if (!grepl("^[+-]?[0-9]+$", val)) {
    stop("Invalid integer value for --", key, ": ", val, call. = FALSE)
  }
  out <- suppressWarnings(as.integer(val))
  if (is.na(out)) {
    stop("Invalid integer value for --", key, ": ", val, call. = FALSE)
  }
  out
}

.parse_cli_num <- function(key, val) {
  out <- suppressWarnings(as.numeric(val))
  if (!nzchar(trimws(val)) || is.na(out) || !is.finite(out)) {
    stop("Invalid numeric value for --", key, ": ", val, call. = FALSE)
  }
  out
}

.parse_cli_lgl <- function(key, val) {
  norm <- tolower(trimws(val))
  if (norm %in% c("true", "t")) {
    return(TRUE)
  }
  if (norm %in% c("false", "f")) {
    return(FALSE)
  }
  stop("Invalid logical value for --", key, ": ", val, call. = FALSE)
}

parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  short_map <- c(
    "-f" = "file",
    "-d" = "directory",
    "-p" = "pattern",
    "-r" = "replacement"
  )
  int_keys <- c("min-datapoints-ms1", "min-datapoints-ms2", "valley-hws")
  num_keys <- c(
    "mz-tol-da-ms1",
    "mz-tol-da-ms2",
    "mz-tol-ppm-ms1",
    "mz-tol-ppm-ms2",
    "intensity-exponent",
    "valley-ratio",
    "mz-ref"
  )
  lgl_keys <- c(
    "mz-weighted",
    "time-domain",
    "deconvolve",
    "baseline-subtract"
  )
  chr_keys <- c(
    "file",
    "directory",
    "pattern",
    "replacement",
    "mz-fun-ms1",
    "mz-fun-ms2",
    "int-fun-ms1",
    "int-fun-ms2",
    "centroid-method",
    "intensity-method",
    "domain-scaling"
  )
  valid_keys <- c(int_keys, num_keys, lgl_keys, chr_keys)

  opt <- list()
  i <- 1L
  n <- length(args)
  while (i <= n) {
    arg <- args[[i]]
    if (arg %in% c("-h", "--help")) {
      return(list(help = TRUE))
    }
    if (grepl("^--[^=]+=", arg)) {
      key <- sub("^--([^=]+)=.*$", "\\1", arg)
      val <- sub("^--[^=]+=", "", arg)
    } else if (arg %in% names(short_map)) {
      key <- short_map[[arg]]
      i <- i + 1L
      if (i > n) {
        stop("Missing value for ", arg, call. = FALSE)
      }
      val <- args[[i]]
    } else if (startsWith(arg, "--")) {
      key <- substring(arg, 3L)
      i <- i + 1L
      if (i > n) {
        stop("Missing value for ", arg, call. = FALSE)
      }
      val <- args[[i]]
    } else {
      stop("Unrecognized argument: ", arg, call. = FALSE)
    }
    if (!key %in% valid_keys) {
      stop("Unknown option --", key, call. = FALSE)
    }
    opt[[key]] <- if (key %in% int_keys) {
      .parse_cli_int(key, val)
    } else if (key %in% num_keys) {
      .parse_cli_num(key, val)
    } else if (key %in% lgl_keys) {
      .parse_cli_lgl(key, val)
    } else {
      val
    }
    i <- i + 1L
  }
  opt
}

centroid_one_file_cli <- function(file, opt) {
  CentroidR::centroid_one_file(
    file = file,
    pattern = opt$pattern,
    replacement = opt$replacement,
    min_datapoints_ms1 = opt$`min-datapoints-ms1` %||% 5L,
    min_datapoints_ms2 = opt$`min-datapoints-ms2` %||% 1L,
    mz_tol_da_ms1 = opt$`mz-tol-da-ms1` %||% 0.0025,
    mz_tol_da_ms2 = opt$`mz-tol-da-ms2` %||% 0.0025,
    mz_tol_ppm_ms1 = opt$`mz-tol-ppm-ms1` %||% 5.0,
    mz_tol_ppm_ms2 = opt$`mz-tol-ppm-ms2` %||% 5.0,
    mz_fun_ms1 = match.fun(opt$`mz-fun-ms1` %||% "mean"),
    mz_fun_ms2 = match.fun(opt$`mz-fun-ms2` %||% "mean"),
    int_fun_ms1 = match.fun(opt$`int-fun-ms1` %||% "max"),
    int_fun_ms2 = match.fun(opt$`int-fun-ms2` %||% "max"),
    mz_weighted = opt$`mz-weighted` %||% TRUE,
    time_domain = opt$`time-domain` %||% TRUE,
    intensity_exponent = opt$`intensity-exponent` %||% 3,
    centroid_method = opt$`centroid-method` %||% "weighted",
    intensity_method = opt$`intensity-method` %||% "func",
    deconvolve = opt$deconvolve %||% FALSE,
    valley_ratio = opt$`valley-ratio` %||% 0.2,
    valley_hws = opt$`valley-hws` %||% 2L,
    baseline_subtract = opt$`baseline-subtract` %||% FALSE,
    domain_scaling = opt$`domain-scaling` %||% "legacy",
    mz_ref = opt$`mz-ref` %||% 100
  )
}

run_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  opt <- tryCatch(
    parse_cli_args(args),
    error = function(e) {
      message("Error: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(opt)) {
    return(2L)
  }
  if (isTRUE(opt$help)) {
    print_cli_help()
    return(0L)
  }
  if (!xor(is.null(opt$file), is.null(opt$directory))) {
    print_cli_help()
    message("Error: Provide either --file or --directory, but not both.")
    return(2L)
  }
  if (is.null(opt$pattern) || is.null(opt$replacement)) {
    print_cli_help()
    message("Error: Missing required arguments --pattern or --replacement.")
    return(2L)
  }

  ok <- tryCatch(
    {
      if (!is.null(opt$file)) {
        isTRUE(centroid_one_file_cli(opt$file, opt))
      } else if (!dir.exists(opt$directory)) {
        message("Error: Directory does not exist: ", opt$directory)
        FALSE
      } else {
        files <- list.files(
          opt$directory,
          pattern = "\\.mzML$",
          full.names = TRUE
        )
        if (length(files) == 0L) {
          message("Error: No mzML files found in directory: ", opt$directory)
          FALSE
        } else {
          all(vapply(files, centroid_one_file_cli, logical(1), opt = opt))
        }
      }
    },
    error = function(e) {
      message("Error: ", conditionMessage(e))
      FALSE
    }
  )
  if (ok) 0L else 1L
}

if (sys.nframe() == 0L) {
  quit(save = "no", status = run_cli())
}
