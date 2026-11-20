library(tinytest)

.fix_xml <- getFromNamespace(".fix_xml", "CentroidR")
.xml_escape_attr <- getFromNamespace(".xml_escape_attr", "CentroidR")

source(
  if (file.exists("helper-logging.R")) {
    "helper-logging.R"
  } else {
    "inst/tinytest/helper-logging.R"
  }
)

write_mock_xml <- function(
  path,
  body = '<run id="Experiment_1"><spectrum value="nan"/></run>'
) {
  writeLines(
    c('<?xml version="1.0" encoding="UTF-8"?>', "<mzML>", body, "</mzML>"),
    path
  )
  path
}

custom_int <- function(intensities) {
  if (length(intensities)) max(intensities) else 0
}

sp <- make_test_spectra(
  c(1L, 2L, 1L),
  list(c(100, 100.0004, 100.0008), c(200, 200.0004, 200.0008), numeric(0)),
  list(c(10, 50, 10), c(5, 25, 5), numeric(0))
)
processed <- run_process_spectra(
  spectra = sp,
  custom_int_fun_ms1 = custom_int,
  custom_int_fun_ms2 = custom_int,
  time_domain = FALSE
)

expect_equal(length(processed), 3L)

infile <- write_test_mzml(sp, tempfile(pattern = "profile_", fileext = ".mzML"))

expect_equal(
  CentroidR::centroid_one_file(
    file = infile,
    pattern = "profile_",
    replacement = "centroided_"
  ),
  TRUE
)
centroidr_reset_logging()

outf <- sub("profile_", "centroided_", infile, fixed = TRUE)
expect_true(file.exists(outf))

sp_out <- Spectra::Spectra(outf, backend = Spectra::MsBackendMzR())
expect_equal(length(sp_out), 3L)

xml <- write_mock_xml(tempfile(fileext = ".mzML"))
.fix_xml(xml)
fixed <- readLines(xml, warn = FALSE)
expect_true(any(grepl('value="NaN"', fixed, fixed = TRUE)))
expect_true(any(grepl(basename(xml), fixed, fixed = TRUE)))

## The fixtures above are created directly in tempdir(), so they are removed
## explicitly here rather than by an on.exit(), which tinytest may fire before
## the assertions above have run.
unlink(c(infile, outf, xml, file.path(dirname(outf), "centroiding.log")))

## ---------------------------------------------------------------------------
## The exported output really is passed through .fix_xml
## ---------------------------------------------------------------------------
## centroid_one_file() rewrites the exported mzML with .fix_xml(), replacing the
## placeholder run id with the output file name. The existing test only checks
## .fix_xml() in isolation, which cannot see whether the pipeline calls it.

fix_infile <- write_test_mzml(
  sp,
  tempfile(pattern = "profile_", fileext = ".mzML")
)
fix_outfile <- sub("profile_", "centroided_", fix_infile, fixed = TRUE)
expect_equal(
  CentroidR::centroid_one_file(
    file = fix_infile,
    pattern = "profile_",
    replacement = "centroided_"
  ),
  TRUE
)
centroidr_reset_logging()
expect_true(file.exists(fix_outfile))
fixed_lines <- readLines(fix_outfile, warn = FALSE)

## The run id now carries the output file name rather than the placeholder.
expect_true(any(grepl(basename(fix_outfile), fixed_lines, fixed = TRUE)))
expect_false(any(grepl("<run id=\"Experiment_1\"", fixed_lines, fixed = TRUE)))

## The rewritten file is still readable by the mzML backend.
expect_equal(
  length(Spectra::Spectra(fix_outfile, backend = Spectra::MsBackendMzR())),
  3L
)

unlink(c(
  fix_infile,
  fix_outfile,
  file.path(dirname(fix_infile), "centroiding.log")
))

## ---------------------------------------------------------------------------
## .fix_xml leaves no temporary file behind
## ---------------------------------------------------------------------------

fix_dir <- tempfile("fixxml_")
dir.create(fix_dir)
tf <- write_mock_xml(
  file.path(fix_dir, "sample.mzML"),
  '<run id="Experiment_1"/>'
)
.fix_xml(tf)
expect_equal(
  list.files(fix_dir),
  "sample.mzML",
  info = ".fix_xml should remove its temporary file and preserve only the target file"
)
unlink(fix_dir, recursive = TRUE)

## F-08: .fix_xml XML-escapes special characters in basename(file_path)
expect_equal(
  .xml_escape_attr('a & "b" <c>\'.mzML'),
  "a &amp; &quot;b&quot; &lt;c&gt;&apos;.mzML"
)
esc_dir <- tempfile("fixxml_esc_")
dir.create(esc_dir)
esc_tf <- write_mock_xml(file.path(esc_dir, "a & b's.mzML"))
.fix_xml(esc_tf)
esc_lines <- readLines(esc_tf, warn = FALSE)
expect_true(any(grepl(
  '<run id="a &amp; b&apos;s.mzML"',
  esc_lines,
  fixed = TRUE
)))
expect_true(any(grepl('value="NaN"', esc_lines, fixed = TRUE)))
unlink(esc_dir, recursive = TRUE)
