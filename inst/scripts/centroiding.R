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
    opt_str = c("-s", "--smooth-method"),
    type = "character",
    default = "SavitzkyGolay",
    help = "Smoothing method (default: SavitzkyGolay)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = c("-w", "--smooth-window"),
    type = "integer",
    default = 6L,
    help = "Smoothing window size (default: 6)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("-m", "--refine-mz"),
    type = "character",
    default = "descendPeak",
    help = "Method for refining m/z (default: descendPeak)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = c("--ms1-peak-snr"),
    type = "integer",
    default = 1L,
    help = "MS1 peak signal-to-noise ratio (default: 1)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("--ms1-signal-percentage"),
    type = "integer",
    default = 33,
    help = "MS1 signal percentage (default: 33)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("--ms2-peak-snr"),
    type = "integer",
    default = 1L,
    help = "MS2 peak signal-to-noise ratio (default: 1)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("--ms2-signal-percentage"),
    type = "integer",
    default = 50,
    help = "MS2 signal percentage (default: 50)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("--min-peaks"),
    type = "integer",
    default = 1000,
    help = "Minimum number of peaks (default: 1000)",
    metavar = "integer"
  )

# Parse the command-line arguments
opt <- parser |>
  optparse::parse_args()

# Ensure required arguments are provided
if (is.null(opt$file) ||
    is.null(opt$pattern) || is.null(opt$replacement)) {
  optparse::print_help(opt_parser)
  stop("Error: Missing required arguments --file, --pattern, or --replacement.")
}

CentroidR::centroid_one_file(
  file = opt$file,
  pattern = opt$pattern,
  replacement = opt$replacement,
  smooth_method = opt$smooth_method,
  smooth_window = opt$smooth_window,
  refine_mz = opt$refine_mz,
  ms1_peak_snr = opt$ms1_peak_snr,
  ms1_signal_percentage = opt$ms1_signal_percentage,
  ms2_peak_snr = opt$ms2_peak_snr,
  ms2_signal_percentage = opt$ms2_signal_percentage,
  min_peaks = opt$min_peaks
)
