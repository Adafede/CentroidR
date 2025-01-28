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
    opt_str = c("--ms1-min-peaks"),
    type = "integer",
    default = 1000,
    help = "MS1 Minimum number of peaks (default: 1000)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("--ms1-noise-estimator"),
    type = "character",
    default = "MAD",
    help = "MS1 smoothing window size (default: SuperSmoother)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = c("--ms1-peak-snr"),
    type = "integer",
    default = 0,
    help = "MS1 peak signal-to-noise ratio (default: 0)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("--ms1-refine-mz"),
    type = "character",
    default = "descendPeak",
    help = "MS1 method for refining m/z (default: descendPeak)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = c("--ms1-signal-percentage"),
    type = "integer",
    default = 33,
    help = "MS1 signal percentage (default: 33)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("--ms1-smooth-window"),
    type = "integer",
    default = 6L,
    help = "MS1 smoothing window size (default: 6)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("--ms2-noise-estimator"),
    type = "character",
    default = "MAD",
    help = "MS2 smoothing window size (default: SuperSmoother)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = c("--ms2-peak-snr"),
    type = "integer",
    default = 0,
    help = "MS2 peak signal-to-noise ratio (default: 0)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("--ms2-refine-mz"),
    type = "character",
    default = "descendPeak",
    help = "MS2 method for refining m/z (default: descendPeak)",
    metavar = "character"
  ) |>
  optparse::add_option(
    opt_str = c("--ms2-signal-percentage"),
    type = "integer",
    default = 50,
    help = "MS2 signal percentage (default: 50)",
    metavar = "integer"
  ) |>
  optparse::add_option(
    opt_str = c("--ms2-smooth-window"),
    type = "integer",
    default = 4L,
    help = "MS2 smoothing window size (default: 4)",
    metavar = "integer"
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
  ms1_min_peaks = opt$`ms1-min-peaks` %||% 1000,
  ms1_noise_estimator = opt$`ms1-noise-estimator` %||% "SuperSmoother",
  ms1_peak_snr = opt$`ms1-peak-snr` %||% 0,
  ms1_refine_mz = opt$`ms1-refine-mz` %||% "descendPeak",
  ms1_signal_percentage = opt$`ms1-signal-percentage` %||% 33,
  ms1_smooth_window = opt$`ms1-smooth-window` %||% 6L,
  ms2_noise_estimator = opt$`ms2-noise-estimator` %||% "SuperSmoother",
  ms2_peak_snr = opt$`ms2-peak-snr` %||% 0,
  ms2_refine_mz = opt$`ms2-refine-mz` %||% "descendPeak",
  ms2_signal_percentage = opt$`ms2-signal-percentage` %||% 50,
  ms2_smooth_window = opt$`ms2-smooth-window` %||% 6L
)
