#!/usr/bin/env Rscript

# CentroidR CLI: Batch or single-file centroiding for mzML files
#
# Usage:
#   Rscript centroiding.R --file <input.mzML> --pattern <pattern> --replacement <replacement>
#   Rscript centroiding.R --directory <dir> --pattern <pattern> --replacement <replacement>
#
# This script wraps CentroidR::centroid_one_file for command-line use.

# Utility: Null coalescing operator (if not already defined)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Define command-line options
parser <- optparse::OptionParser() |>
  optparse::add_option(
    opt_str = c("-f", "--file"),
    type = "character",
    default = NULL,
    help = "Path to the input file [exclusive with --directory]",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = c("-d", "--directory"),
    type = "character",
    default = NULL,
    help = "Path to a directory containing files [exclusive with --file]",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = c("-p", "--pattern"),
    type = "character",
    default = NULL,
    help = "Pattern to replace in the file [required]",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = c("-r", "--replacement"),
    type = "character",
    default = NULL,
    help = "Replacement pattern [required]",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--min-datapoints-ms1",
    type = "integer",
    default = 5L,
    help = "Minimum datapoints (MS1). (default: 5)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = "--min-datapoints-ms2",
    type = "integer",
    default = 1L,
    help = "Minimum datapoints (MS2). (default: 1)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = "--mz-tol-da-ms1",
    type = "numeric",
    default = 0.0025,
    help = "m/z tolerance in Dalton (MS1). (default: 0.0025)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--mz-tol-da-ms2",
    type = "numeric",
    default = 0.0025,
    help = "m/z tolerance in Dalton (MS2). (default: 0.0025)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--mz-tol-ppm-ms1",
    type = "numeric",
    default = 5,
    help = "m/z tolerance in ppm (MS1). (default: 5)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--mz-tol-ppm-ms2",
    type = "numeric",
    default = 5,
    help = "m/z tolerance in ppm (MS2). (default: 5)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--mz-fun-ms1",
    type = "character",
    default = "mean",
    help = "Function to aggregate m/z values (MS1). (default: mean)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--mz-fun-ms2",
    type = "character",
    default = "mean",
    help = "Function to aggregate m/z values (MS2). (default: mean)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--int-fun-ms1",
    type = "character",
    default = "max",
    help = "Function to aggregate intensity values (MS1). (default: max)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--int-fun-ms2",
    type = "character",
    default = "max",
    help = "Function to aggregate intensity values (MS2). (default: max)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--mz-weighted",
    type = "logical",
    default = TRUE,
    help = "Whether m/z values of peaks within each peak group should be aggregated using an intensity-weighted mean. (default: TRUE)",
    metavar = "logical"
  ) |>
  optparse::add_option(
    opt_str = "--time-domain",
    type = "logical",
    default = TRUE,
    help = "Whether grouping of mass peaks is performed on the m/z values (FALSE) or on sqrt(mz) (TRUE). (default: TRUE)",
    metavar = "logical"
  ) |>
  optparse::add_option(
    opt_str = "--intensity-exponent",
    type = "numeric",
    default = 3,
    help = "Exponent to apply to the intensities when weighting m/z. (default: 3)",
    metavar = "numeric"
  )

# Parse the command-line arguments
opt <- parse_args(parser)

# Input validation
if (
  (is.null(opt$file) && is.null(opt$directory)) ||
    (!is.null(opt$file) && !is.null(opt$directory))
) {
  optparse::print_help(parser)
  stop("Error: Provide either --file or --directory, but not both.")
}
if (is.null(opt$pattern) || is.null(opt$replacement)) {
  optparse::print_help(parser)
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
  # Process all .mzML files in the directory
  list.files(opt$directory, pattern = ".mzML$", full.names = TRUE) |>
    purrr::walk(.f = centroid_one_file_cli, .progress = TRUE)
}
