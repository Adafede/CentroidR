#!/usr/bin/env Rscript

# Define command-line options
parser <- optparse::OptionParser() |>
  optparse::add_option(
    opt_str = c("-f", "--file"),
    type = "character",
    default = NULL,
    help = "Path to the input file [required]",
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
    default = 0.002,
    help = " m/z tolerance in Dalton (ms1). (default: 0.002)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--ms-tol-da-ms2",
    type = "numeric",
    default = 0.005,
    help = " m/z tolerance in Dalton (ms2). (default: 0.005)",
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
    default = 10,
    help = " m/z tolerance in ppm (ms2). (default: 10)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--mz-fun",
    type = "character",
    default = "mean",
    help = "Function to aggregate m/z values. (default: `mean`)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--int-fun",
    type = "character",
    default = "max",
    help = " Function to aggregate intensity values. (default: `max`)",
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

# Ensure required arguments are provided
if (is.null(opt$file) ||
  is.null(opt$pattern) || is.null(opt$replacement)) {
  optparse::print_help(parser)
  stop("Error: Missing required arguments --file, --pattern, or --replacement.")
}

CentroidR::centroid_one_file(
  file = opt$file,
  pattern = opt$pattern,
  replacement = opt$replacement,
  min_datapoints_ms1 = opt$`min-datapoints-ms1` %||% 5L,
  min_datapoints_ms2 = opt$`min-datapoints-ms2` %||% 2L,
  mz_tol_da_ms1 = opt$`mz-tol-da-ms1` %||% 0.002,
  mz_tol_da_ms2 = opt$`mz-tol-da-ms2` %||% 0.005,
  mz_tol_ppm_ms1 = opt$`mz-tol-ppm-ms1` %||% 5,
  mz_tol_ppm_ms2 = opt$`mz-tol-ppm-ms2` %||% 10,
  mz_fun = opt$`mz-fun` |> match.fun() %||% base::mean,
  int_fun = opt$`int-fun` |> match.fun() %||% base::max,
  mz_weighted = opt$`mz-weighted` %||% TRUE,
  time_domain = opt$`time-domain` %||% TRUE,
  intensity_exponent = opt$`intensity-exponent` %||% 3,
)
