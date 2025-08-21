

<!-- README.md is generated from README.qmd. Please edit that file -->

# CentroidR <img src='https://raw.githubusercontent.com/adafede/CentroidR/main/man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/adafede/CentroidR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adafede/CentroidR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/adafede/CentroidR/graph/badge.svg)](https://app.codecov.io/gh/adafede/CentroidR)
[![r-universe
badge](https://adafede.r-universe.dev/CentroidR/badges/version?&color=blue&style=classic.png)](https://adafede.r-universe.dev/CentroidR)
[![Docker](https://badgen.net/badge/docker/centroidr?icon&label.png)](https://hub.docker.com/r/adafede/centroidr/)
<!-- badges: end -->

Repository to centroid profile spectra.

This repository is experimental. Use it at your own risks. Inspired from
the original work at
<https://github.com/EuracBiomedicalResearch/batch_centroid>

## Requirements

Here is what you *minimally* need:

- **An mzML file containing profile spectra**

Here is a generic command in case:

``` bash
docker run -it --rm -e WINEDEBUG=-all -v .:/data proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses wine msconvert "path_to_your/raw/spectra.wiff" --ignoreUnknownInstrumentError
```

Note: If using Sciex raw format, you can use both the `.wiff` and the
`.wiff2` format for this step.

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

### Single file

``` r
CentroidR::centroid_one_file(file = "path_to_your/profile/spectra.mzML",
                             pattern = "/profile/",
                             replacement = "/profile_centroided/")
```

``` bash
Rscript inst/scripts/centroiding.R --file "path_to_your/profile/spectra.mzML" --pattern "/profile/" --replacement "/profile_centroided/"
```

### Multiple files

``` r
"path_to_your/profiles/" |>
    list.files(pattern = ".mzML", full.names = TRUE) |>
    purrr::walk(
      .f = CentroidR::centroid_one_file,
      pattern = "/profiles/",
      replacement = "/profiles_centroided/",
      .progress = TRUE)
```

``` bash
Rscript inst/scripts/centroiding.R --directory "path_to_your/profiles/" --pattern "/profiles/" --replacement "/profiles_centroided/"
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

``` bash
 docker run --rm \
  -v path_to_your:/home \
  adafede/centroidr \
  Rscript centroiding.R --directory "home/profiles/" --pattern "/profiles/" --replacement "/profiles_centroided/"
```

To see all parameters

``` bash
 docker run --rm \
   -v path_to_your:/home \
  adafede/centroidr \
  Rscript centroiding.R --help
```

## Citation

TODO

## Additional software credits

| Package | Version | Citation |
|:---|:---|:---|
| BiocManager | 1.30.26 | Morgan and Ramos (2025) |
| BiocParallel | 1.43.4 | Wang et al. (2025) |
| BiocVersion | 3.22.0 | Morgan (2025) |
| CentroidR | 0.0.0.9001 | Rutz and Rainer (2025) |
| MsCoreUtils | 1.21.0 | Rainer et al. (2022a) |
| Spectra | 1.19.4 | Rainer et al. (2022b) |
| base | 4.5.1 | R Core Team (2025) |
| knitr | 1.50 | Xie (2014); Xie (2015); Xie (2025) |
| logger | 0.4.0 | Daróczi and Wickham (2024) |
| mzR | 2.43.1 | Pedrioli et al. (2004); Keller et al. (2005); Kessner et al. (2008); Martens et al. (2010); Chambers et al. (2012) |
| optparse | 1.7.5 | Davis (2024) |
| rmarkdown | 2.29 | Xie, Allaire, and Grolemund (2018); Xie, Dervieux, and Riederer (2020); Allaire et al. (2024) |
| testthat | 3.2.3 | Wickham (2011) |
| tidyverse | 2.0.0 | Wickham et al. (2019) |

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-rmarkdown2024" class="csl-entry">

Allaire, JJ, Yihui Xie, Christophe Dervieux, Jonathan McPherson, Javier
Luraschi, Kevin Ushey, Aron Atkins, et al. 2024.
*<span class="nocase">rmarkdown</span>: Dynamic Documents for r*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-mzR2012" class="csl-entry">

Chambers, Matthew C., Maclean, Brendan, Burke, Robert, Amodei, et al.
2012. “<span class="nocase">A cross-platform toolkit for mass
spectrometry and proteomics</span>.” *Nat Biotech* 30 (10): 918–20.
<https://doi.org/10.1038/nbt.2377>.

</div>

<div id="ref-logger" class="csl-entry">

Daróczi, Gergely, and Hadley Wickham. 2024.
*<span class="nocase">logger</span>: A Lightweight, Modern and Flexible
Logging Utility*. <https://doi.org/10.32614/CRAN.package.logger>.

</div>

<div id="ref-optparse" class="csl-entry">

Davis, Trevor L. 2024. *<span class="nocase">optparse</span>: Command
Line Option Parser*. <https://doi.org/10.32614/CRAN.package.optparse>.

</div>

<div id="ref-mzR2005" class="csl-entry">

Keller, Andrew, Jimmy Eng, Ning Zhang, Xiao-jun Li, and Ruedi Aebersold.
2005. “A Uniform Proteomics MS/MS Analysis Platform Utilizing Open XML
File Formats.” *Mol Syst Biol*.

</div>

<div id="ref-mzR2008" class="csl-entry">

Kessner, Darren, Matt Chambers, Robert Burke, David Agus, and Parag
Mallick. 2008. “ProteoWizard: Open Source Software for Rapid Proteomics
Tools Development.” *Bioinformatics* 24 (21): 2534–36.
<https://doi.org/10.1093/bioinformatics/btn323>.

</div>

<div id="ref-mzR2010" class="csl-entry">

Martens, Lennart, Matthew Chambers, Marc Sturm, Darren Kessner, Fredrik
Levander, Jim Shofstahl, Wilfred H Tang, et al. 2010. “MzML - a
Community Standard for Mass Spectrometry Data.” *Mol Cell Proteomics*.
<https://doi.org/10.1074/mcp.R110.000133>.

</div>

<div id="ref-BiocVersion" class="csl-entry">

Morgan, Martin. 2025. *BiocVersion: Set the Appropriate Version of
Bioconductor Packages*. <https://doi.org/10.18129/B9.bioc.BiocVersion>.

</div>

<div id="ref-BiocManager" class="csl-entry">

Morgan, Martin, and Marcel Ramos. 2025. *BiocManager: Access the
Bioconductor Project Package Repository*.
<https://doi.org/10.32614/CRAN.package.BiocManager>.

</div>

<div id="ref-mzR2004" class="csl-entry">

Pedrioli, Patrick G A, Jimmy K Eng, Robert Hubley, Mathijs Vogelzang,
Eric W Deutsch, Brian Raught, Brian Pratt, et al. 2004. “A Common Open
Representation of Mass Spectrometry Data and Its Application to
Proteomics Research.” *Nat Biotechnol* 22 (11): 1459–66.
<https://doi.org/10.1038/nbt1031>.

</div>

<div id="ref-base" class="csl-entry">

R Core Team. 2025. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-MsCoreUtils" class="csl-entry">

Rainer, Johannes, Andrea Vicini, Liesa Salzer, Jan Stanstrup, Josep M.
Badia, Steffen Neumann, Michael A. Stravs, et al. 2022a. “A Modular and
Expandable Ecosystem for Metabolomics Data Annotation in r.”
*Metabolites* 12: 173. <https://doi.org/10.3390/metabo12020173>.

</div>

<div id="ref-Spectra" class="csl-entry">

———, et al. 2022b. “A Modular and Expandable Ecosystem for Metabolomics
Data Annotation in r.” *Metabolites* 12: 173.
<https://doi.org/10.3390/metabo12020173>.

</div>

<div id="ref-CentroidR" class="csl-entry">

Rutz, Adriano, and Johannes Rainer. 2025. *CentroidR: CentroidR Provides
the Infrastructure to Centroid Profile Spectra*.

</div>

<div id="ref-BiocParallel" class="csl-entry">

Wang, Jiefei, Martin Morgan, Valerie Obenchain, Michel Lang, Ryan
Thompson, and Nitesh Turaga. 2025. *BiocParallel: Bioconductor
Facilities for Parallel Evaluation*.
<https://doi.org/10.18129/B9.bioc.BiocParallel>.

</div>

<div id="ref-testthat" class="csl-entry">

Wickham, Hadley. 2011. “<span class="nocase">testthat</span>: Get
Started with Testing.” *The R Journal* 3: 5–10.
<https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-knitr2014" class="csl-entry">

Xie, Yihui. 2014. “<span class="nocase">knitr</span>: A Comprehensive
Tool for Reproducible Research in R.” In *Implementing Reproducible
Computational Research*, edited by Victoria Stodden, Friedrich Leisch,
and Roger D. Peng. Chapman; Hall/CRC.

</div>

<div id="ref-knitr2015" class="csl-entry">

———. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca Raton,
Florida: Chapman; Hall/CRC. <https://yihui.org/knitr/>.

</div>

<div id="ref-knitr2025" class="csl-entry">

———. 2025. *<span class="nocase">knitr</span>: A General-Purpose Package
for Dynamic Report Generation in R*. <https://yihui.org/knitr/>.

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown>.

</div>

<div id="ref-rmarkdown2020" class="csl-entry">

Xie, Yihui, Christophe Dervieux, and Emily Riederer. 2020. *R Markdown
Cookbook*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown-cookbook>.

</div>

</div>
