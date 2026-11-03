# CentroidR CLI: Batch or single-file centroiding for mzML files
#
# Usage:
#   Rscript centroiding.R --file <input.mzML> --pattern <pattern> \
#                         --replacement <replacement>
#   Rscript centroiding.R --directory <dir> --pattern <pattern> \
#                         --replacement <replacement>
#
# This script wraps CentroidR::centroid_one_file for command-line use
# without requiring external CLI option-parsing packages.

print_cli_help <- function() {
  cat(
    "Usage: centroiding.R [options]\n\n",
    "Options:\n",
    "  -f CHARACTER, --file=CHARACTER\n",
    "      Path to the input file [exclusive with --directory]\n",
    "  -d CHARACTER, --directory=CHARACTER\n",
    "      Path to a directory containing files [exclusive with --file]\n",
    "  -p CHARACTER, --pattern=CHARACTER\n",
    "      Pattern to replace in the file [required]\n",
    "  -r CHARACTER, --replacement=CHARACTER\n",
    "      Replacement pattern [required]\n",
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
    "      Whether m/z values of peaks within each peak group should be\n",
    "      aggregated using an intensity-weighted mean. (default: TRUE)\n",
    "  --time-domain=LOGICAL\n",
    "      Whether grouping of mass peaks is performed on the m/z values\n",
    "      (FALSE) or on sqrt(mz) (TRUE). (default: TRUE)\n",
    "  --intensity-exponent=NUMERIC\n",
    "      Exponent to apply to the intensities when weighting m/z. (default: 3)\n",
    "  -h, --help\n",
    "      Show this help message and exit\n",
    sep = ""
  )
}

parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  short_map <- c(
    "-f" = "file",
    "-d" = "directory",
    "-p" = "pattern",
    "-r" = "replacement"
  )
  int_keys <- c("min-datapoints-ms1", "min-datapoints-ms2")
  num_keys <- c(
    "mz-tol-da-ms1",
    "mz-tol-da-ms2",
    "mz-tol-ppm-ms1",
    "mz-tol-ppm-ms2",
    "intensity-exponent"
  )
  lgl_keys <- c("mz-weighted", "time-domain")
  chr_keys <- c(
    "file",
    "directory",
    "pattern",
    "replacement",
    "mz-fun-ms1",
    "mz-fun-ms2",
    "int-fun-ms1",
    "int-fun-ms2"
  )
  valid_keys <- c(int_keys, num_keys, lgl_keys, chr_keys)

  opt <- list()
  i <- 1L
  n <- length(args)
  while (i <= n) {
    arg <- args[[i]]
    if (arg %in% c("-h", "--help")) {
      print_cli_help()
      quit(save = "no", status = 0L)
    }
    if (grepl("^--[^=]+=", arg)) {
      key <- sub("^--([^=]+)=.*$", "\\1", arg)
      val <- sub("^--[^=]+=", "", arg)
    } else if (arg %in% names(short_map)) {
      key <- short_map[[arg]]
      i <- i + 1L
      if (i > n) {
        stop("Error: Missing value for ", arg, call. = FALSE)
      }
      val <- args[[i]]
    } else if (startsWith(arg, "--")) {
      key <- substring(arg, 3L)
      i <- i + 1L
      if (i > n) {
        stop("Error: Missing value for ", arg, call. = FALSE)
      }
      val <- args[[i]]
    } else {
      stop("Error: Unrecognized argument: ", arg, call. = FALSE)
    }
    if (!key %in% valid_keys) {
      stop("Error: Unknown option --", key, call. = FALSE)
    }
    opt[[key]] <- if (key %in% int_keys) {
      as.integer(val)
    } else if (key %in% num_keys) {
      as.numeric(val)
    } else if (key %in% lgl_keys) {
      as.logical(val)
    } else {
      val
    }
    i <- i + 1L
  }
  opt
}

opt <- parse_cli_args()

# Input validation
if (!xor(is.null(opt$file), is.null(opt$directory))) {
  print_cli_help()
  stop("Error: Provide either --file or --directory, but not both.")
}
if (is.null(opt$pattern) || is.null(opt$replacement)) {
  print_cli_help()
  stop("Error: Missing required arguments --pattern or --replacement.")
}

# Wrapper for calling CentroidR::centroid_one_file with parsed options
centroid_one_file_cli <- function(file) {
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
    intensity_exponent = opt$`intensity-exponent` %||% 3
  )
}

# Main processing logic
if (!is.null(opt$file)) {
  centroid_one_file_cli(opt$file)
} else {
  lapply(
    list.files(opt$directory, pattern = "\\.mzML$", full.names = TRUE),
    centroid_one_file_cli
  )
}
