
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CentroidR

<!-- <img src='https://raw.githubusercontent.com/adafede/CentroidR/main/man/figures/logo.svg' align="right" height="139" /> -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/adafede/CentroidR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adafede/CentroidR/actions/workflows/R-CMD-check.yaml)
[![R-Universe](https://adafede.r-universe.dev/badges/CentroidR)](https://adafede.r-universe.dev/CentroidR)
[![Codecov test
coverage](https://codecov.io/gh/adafede/CentroidR/graph/badge.svg)](https://app.codecov.io/gh/adafede/CentroidR)
<!-- badges: end -->

Repository to centroid profile spectra.

This repository is experimental. Use it at your own risks. Inspired from
the original work at
<https://github.com/EuracBiomedicalResearch/batch_centroid>

## Requirements

Here is what you *minimally* need:

- **An mzML file containing profile spectra**

## Installation

As the package is not (yet) available on CRAN, you will need to install
with:

``` r
install.packages(
  "CentroidR",
  repos = c(
    "https://adafede.r-universe.dev",
    "https://bioc.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

## Use

### R

``` r
CentroidR::centroid_one_file(file = "path_to_your/profile/spectra.mzML",
                             pattern = "/profile/",
                             replacement = "/profile_centroided/")
```

``` bash
Rscript inst/scripts/centroiding.R --file "path_to_your/profile/spectra.mzML" --pattern "/profile/" --replacement "/profile_centroided/"
```

``` bash
Rscript inst/scripts/centroiding.R --help
```

### Docker

``` bash
docker pull adafede/centroidr
# docker build . -t adafede/centroidr
```

``` bash
 docker run --rm \
  -v path_to_your:/home \
  adafede/centroidr \
  Rscript centroiding.R --file "home/profile/spectra.mzML" --pattern "/profile/" --replacement "/profile_centroided/"
```

To see all parameters

``` bash
 docker run --rm \
   -v path_to_your:/home \
  adafede/centroidr \
  Rscript centroiding.R --help
```

## Main Citations

TODO

### Others

- The *RforMassSpectrometry* packages suite:
  <https://doi.org/10.3390/metabo12020173>
