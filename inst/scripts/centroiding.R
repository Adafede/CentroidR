#!/usr/bin/env Rscript

# Command line argument parsing
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop(
    "Usage: Rscript centroiding.R <input_file> <pattern_to_replace> <replacement_pattern>"
  )
}

file <- args[1] # First argument is the mzML file path
pattern <- args[2] # Second argument is the pattern to replace
replacement <- args[3] # Third argument is the replacement pattern

CentroidR::centroid_one_file(
  file = file,
  pattern = pattern,
  replacement = replacement
)
