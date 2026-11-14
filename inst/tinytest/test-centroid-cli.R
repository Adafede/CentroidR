## Tests for inst/scripts/centroiding.R CLI parsing, error handling, and exit codes.

library(tinytest)

source(
  if (file.exists("helper-logging.R")) {
    "helper-logging.R"
  } else {
    "inst/tinytest/helper-logging.R"
  }
)

cli_path <- if (file.exists("inst/scripts/centroiding.R")) {
  normalizePath("inst/scripts/centroiding.R")
} else if (file.exists("../scripts/centroiding.R")) {
  normalizePath("../scripts/centroiding.R")
} else {
  system.file("scripts", "centroiding.R", package = "CentroidR")
}

cli_env <- new.env(parent = globalenv())
source(cli_path, local = cli_env)

## Strict option parser rejects fractional integers, bad numbers, bad logicals,
## missing values, and unknown options instead of coercing silently to NA.
expect_error(
  cli_env$parse_cli_args(c("--valley-hws=1.5")),
  pattern = "Invalid integer"
)
expect_error(
  cli_env$parse_cli_args(c("--min-datapoints-ms1=abc")),
  pattern = "Invalid integer"
)
expect_error(
  cli_env$parse_cli_args(c("--mz-tol-ppm-ms1=notnum")),
  pattern = "Invalid numeric"
)
expect_error(
  cli_env$parse_cli_args(c("--deconvolve=maybe")),
  pattern = "Invalid logical"
)
expect_error(
  cli_env$parse_cli_args(c("--unknown-flag=1")),
  pattern = "Unknown option"
)
expect_error(
  cli_env$parse_cli_args(c("-f")),
  pattern = "Missing value"
)

parsed <- cli_env$parse_cli_args(c(
  "-f",
  "in.mzML",
  "-p",
  "in",
  "-r",
  "out",
  "--deconvolve=true",
  "--baseline-subtract",
  "FALSE",
  "--valley-hws=3",
  "--valley-ratio=0.15",
  "--domain-scaling=tof"
))
expect_equal(parsed$file, "in.mzML")
expect_equal(parsed$pattern, "in")
expect_equal(parsed$replacement, "out")
expect_identical(parsed$deconvolve, TRUE)
expect_identical(parsed$`baseline-subtract`, FALSE)
expect_identical(parsed$`valley-hws`, 3L)
expect_equal(parsed$`valley-ratio`, 0.15)
expect_equal(parsed$`domain-scaling`, "tof")

## run_cli() usage / validation exit codes
status_help <- NULL
help_out <- capture.output({
  status_help <- cli_env$run_cli("--help")
})
expect_identical(status_help, 0L)
expect_true(any(grepl("--centroid-method", help_out, fixed = TRUE)))

expect_identical(
  suppressMessages(cli_env$run_cli(c(
    "--file=a.mzML",
    "--directory=d",
    "-p",
    "a",
    "-r",
    "b"
  ))),
  2L
)
expect_identical(
  suppressMessages(cli_env$run_cli(c("--file=a.mzML"))),
  2L
)
expect_identical(
  suppressMessages(cli_env$run_cli(c("--bad-flag=1"))),
  2L
)

## run_cli() returns exit code 1L on missing file, missing directory, or empty dir
cli_dir <- tempfile("cli_test_")
dir.create(cli_dir)
empty_dir <- file.path(cli_dir, "empty")
dir.create(empty_dir)

expect_identical(
  suppressMessages(cli_env$run_cli(c(
    "-f",
    file.path(cli_dir, "missing.mzML"),
    "-p",
    "profile_",
    "-r",
    "centroided_"
  ))),
  1L
)
centroidr_reset_logging()

expect_identical(
  suppressMessages(cli_env$run_cli(c(
    "-d",
    file.path(cli_dir, "missing_dir"),
    "-p",
    "profile_",
    "-r",
    "centroided_"
  ))),
  1L
)
expect_identical(
  suppressMessages(cli_env$run_cli(c(
    "-d",
    empty_dir,
    "-p",
    "profile_",
    "-r",
    "centroided_"
  ))),
  1L
)

## Happy-path single-file and directory runs return 0L and produce outputs
in1 <- file.path(cli_dir, "profile_one.mzML")
Spectra::export(
  Spectra::Spectra(data.frame(
    msLevel = 1L,
    polarity = 0L,
    rtime = 1,
    mz = I(list(c(100, 100.0005, 100.001))),
    intensity = I(list(c(10, 50, 20)))
  )),
  file = in1,
  backend = Spectra::MsBackendMzR()
)
expect_identical(
  cli_env$run_cli(c(
    "-f",
    in1,
    "-p",
    "profile_",
    "-r",
    "centroided_",
    "--min-datapoints-ms1=1"
  )),
  0L
)
centroidr_reset_logging()
expect_true(file.exists(file.path(cli_dir, "centroided_one.mzML")))

batch_dir <- file.path(cli_dir, "batch")
dir.create(batch_dir)
file.copy(in1, file.path(batch_dir, "profile_two.mzML"))
expect_identical(
  cli_env$run_cli(c(
    "-d",
    batch_dir,
    "-p",
    "profile_",
    "-r",
    "centroided_",
    "--min-datapoints-ms1=1"
  )),
  0L
)
centroidr_reset_logging()
expect_true(file.exists(file.path(batch_dir, "centroided_two.mzML")))

## Subprocess exit code verification via Rscript
rscript <- file.path(R.home("bin"), "Rscript")
sub_help <- system2(
  rscript,
  c(cli_path, "--help"),
  stdout = TRUE,
  stderr = TRUE
)
expect_equal(attr(sub_help, "status") %||% 0L, 0L)

sub_missing <- suppressWarnings(system2(
  rscript,
  c(
    cli_path,
    "-f",
    file.path(cli_dir, "nope.mzML"),
    "-p",
    "a",
    "-r",
    "b"
  ),
  stdout = TRUE,
  stderr = TRUE
))
expect_equal(attr(sub_missing, "status"), 1L)

sub_bad_int <- suppressWarnings(system2(
  rscript,
  c(
    cli_path,
    "-f",
    in1,
    "-p",
    "profile_",
    "-r",
    "centroided_",
    "--valley-hws=1.5"
  ),
  stdout = TRUE,
  stderr = TRUE
))
expect_equal(attr(sub_bad_int, "status"), 2L)

unlink(cli_dir, recursive = TRUE)
