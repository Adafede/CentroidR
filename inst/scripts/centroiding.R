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
    opt_str = "--ms-tol-da",
    type = "numeric",
    default = 0,
    help = " m/z tolerance in Dalton. (default: 0)",
    metavar = "numeric"
  ) |>
  optparse::add_option(
    opt_str = "--ms-tol-ppm",
    type = "numeric",
    default = 5,
    help = " m/z tolerance in ppm. (default: 5)",
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
    default = "sum",
    help = "Function to aggregate intensity values. (default: `sum`)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = "--mz-weighted",
    type = "logical",
    default = TRUE,
    help = "Boolean whether m/z values of peaks within each peak group should be aggregated into a single m/z value using an intensity-weighted mean. (default: TRUE)",
    metavar = "logical"
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
  mz_tol_da = opt$`mz-tol-da` %||% 0,
  mz_tol_ppm = opt$`mz-tol-ppm` %||% 5,
  mz_fun = opt$`mz-fun` |> match.fun() %||% base::mean,
  int_fun = opt$`int-fun` |> match.fun() %||% base::sum,
  mz_weighted = opt$`mz-weigthed` %||% TRUE
)
