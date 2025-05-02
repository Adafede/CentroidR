#!/usr/bin/env Rscript

# Define command-line options
parser <- optparse::OptionParser() |>
  optparse::add_option(
    opt_str = c("-f", "--file"),
    type = "character",
    default = NULL,
    help = "Path to the input file [exclusive with --dir]",
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
    help = " minimum datapoints (ms1). (default: 5L)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = "--min-datapoints-ms2",
    type = "integer",
    default = 2L,
    help = " minimum datapoints (ms2). (default: 2L)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = "--ms-tol-da-ms1",
    type = "numeric",
    default = 0.0025,
    help = " m/z tolerance in Dalton (ms1). (default: 0.0025)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--ms-tol-da-ms2",
    type = "numeric",
    default = 0.0025,
    help = " m/z tolerance in Dalton (ms2). (default: 0.0025)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--ms-tol-ppm-ms1",
    type = "numeric",
    default = 5,
    help = " m/z tolerance in ppm (ms1). (default: 5)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--ms-tol-ppm-ms2",
    type = "numeric",
    default = 5,
    help = " m/z tolerance in ppm (ms2). (default: 5)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--mz-fun-ms1",
    type = "character",
    default = "mean",
    help = "Function to aggregate m/z values (MS1). (default: `mean`)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--mz-fun-ms2",
    type = "character",
    default = "mean",
    help = "Function to aggregate m/z values (MS2). (default: `mean`)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--int-fun-ms1",
    type = "character",
    default = "max",
    help = " Function to aggregate intensity values (MS1). (default: `max`)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--int-fun-ms2",
    type = "character",
    default = "max",
    help = " Function to aggregate intensity values (MS2). (default: `sum`)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--mz-weighted",
    type = "logical",
    default = TRUE,
    help = " Boolean whether m/z values of peaks within each peak group should be aggregated into a single m/z value using an intensity-weighted mean. (default: TRUE)",
    metavar = "logical"
  ) |>
  optparse::add_option(
    opt_str = "--time-domain",
    type = "logical",
    default = TRUE,
    help = " Boolean logical(1) whether grouping of mass peaks is performed on the m/z values (timeDomain = FALSE) or on sqrt(mz) (timeDomain = TRUE). (default: TRUE)",
    metavar = "logical"
  ) |>
  optparse::add_option(
    opt_str = "--intensity-exponent",
    type = "numeric",
    default = 3,
    help = " Exponent to apply to the intensities when weighting m/z. (default: 3)",
    metavar = "numeric"
  )

# Parse the command-line arguments
opt <- parser |>
  optparse::parse_args()

# Input validation
if (
  (is.null(opt$file) && is.null(opt$dir)) ||
    (!is.null(opt$file) && !is.null(opt$dir))
) {
  optparse::print_help(parser)
  stop("Error: Provide either --file or --dir, but not both.")
}
if (
  is.null(opt$pattern) ||
    is.null(opt$replacement)
) {
  optparse::print_help(parser)
  stop("Error: Missing required arguments --pattern, or --replacement.")
}

centroid_one_file <- function(file) {
  CentroidR::centroid_one_file(
    file = file,
    pattern = opt$pattern,
    replacement = opt$replacement,
    min_datapoints_ms1 = opt$`min-datapoints-ms1` %||% 5L,
    min_datapoints_ms2 = opt$`min-datapoints-ms2` %||% 2L,
    mz_tol_da_ms1 = opt$`mz-tol-da-ms1` %||% 0.0025,
    mz_tol_da_ms2 = opt$`mz-tol-da-ms2` %||% 0.0025,
    mz_tol_ppm_ms1 = opt$`mz-tol-ppm-ms1` %||% 5.0,
    mz_tol_ppm_ms2 = opt$`mz-tol-ppm-ms2` %||% 5.0,
    mz_fun_ms1 = opt$`mz-fun-ms1` |> match.fun() %||% base::mean,
    mz_fun_ms2 = opt$`mz-fun-ms2` |> match.fun() %||% base::mean,
    int_fun_ms1 = opt$`int-fun-ms1` |> match.fun() %||% base::max,
    int_fun_ms2 = opt$`int-fun-ms2` |> match.fun() %||% base::max,
    mz_weighted = opt$`mz-weighted` %||% TRUE,
    time_domain = opt$`time-domain` %||% TRUE,
    intensity_exponent = opt$`intensity-exponent` %||% 3
  )
}

# Process
if (!is.null(opt$file)) {
  centroid_one_file(opt$file)
} else {
  opt$dir |>
    list.files(pattern = ".mzML", full.names = TRUE) |>
    purrr::walk(.f = centroid_one_file, .progress = TRUE)
}
