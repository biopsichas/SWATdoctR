Introduction to SWATdoctR
================

# SWATdoctR

[![](https://img.shields.io/badge/devel%20version-0.1.27-blue.svg)](https://github.com/biopsichas/SWATdoctR)
[![](https://img.shields.io/github/last-commit/biopsichas/SWATdoctR.svg)](https://github.com/biopsichas/SWATdoctR/commits/green)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/github/languages/code-size/biopsichas/SWATdoctR.svg)](https://github.com/biopsichas/SWATdoctR)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![](https://img.shields.io/badge/doi-https://doi.org/10.1016/j.envsoft.2023.105878-yellow.svg)](https://doi.org/https://doi.org/10.1016/j.envsoft.2023.105878)

`SWATdoctR` is a collection of functions and routines for SWAT model
calibration and model diagnostics. The R package includes routines for a
guided model setup verification, as well as functions for the
visualization and diagnosis of simulation outputs. The aim of the
`SWATdoctR` is to identify potential issues in the model setup early in
the calibration process and to support the SWAT modeler to focus on a
plausible process representation in the model calibration process.
Additionally, package is intended to be used as SWAT+ models quality
assurance tool in order to increase confidence level in model results
and applications. A detailed overview is presented in the article by
Plunge, Schürz, et al. (2024).

Most functions were developed for the implementation of modeling tasks
in the [OPTAIN project](https://www.optain.eu/). These tools are
intended to fill the gaps in the SWAT+ workflow alongside the main tools
developed by [Christoph Schuerz](https://www.ufz.de/index.php?en=49467).
Therefore, we highly recommend trying and using these tools:

- [SWATprepR](https://biopsichas.github.io/SWATprepR/) - SWAT+ model
  input data preparation helper. The package is presented in the article
  Plunge, Szabó, et al. (2024);
- [SWATbuildR](https://github.com/chrisschuerz/SWATbuildR) - R tool for
  building SWAT+ setups.
- [SWATfarmR](http://chrisschuerz.github.io/SWATfarmR/) - R tool for
  preparing management schedules for the SWAT model.
- [SWATrunR](https://chrisschuerz.github.io/SWATrunR/) - R tool for
  running SWAT models for different parameters and scenarios. Please
  install branch names *remove_legacy*. It could be done using line like
  this `remotes::install_github("chrisschuerz/SWATrunR@remove_legacy")`
- [SWATtunR](https://biopsichas.github.io/SWATtunR/) - R tool for soft &
  hard calibration, validation of SWAT+ models.
- [SWATmeasR](https://git.ufz.de/schuerz/swatmeasr) - R tool for
  implementing Natural/Small Water Retention Measures (NSWRMs) in the
  SWAT+ models and running scenarios.

Additionally, we recommend checking out the following packages, which
can be used to build scripted workflows for SWAT+:

- [SWATreadR](https://github.com/chrisschuerz/SWATreadR) - Read and
  write SWAT+ input and output files.
- [SWATdata](https://github.com/chrisschuerz/SWATdata) - SWAT project
  datasets for SWATrunR and SWATfarmR.

<img src="man/figures/swativerse_update.png" title="SWAT packages for R" alt="swativerse logo" width="80%" style="display: block; margin: auto;" />

Detailed information about packages, workflow steps, input data, SWAT+
parameters, model calibration, validation, etc., could be found in the
[SWAT+ modeling protocol](https://doi.org/10.5281/zenodo.7463395)
Christoph et al. (2022).

## Installation

You can install the development version of **SWATdoctR** from
[GitHub](https://github.com/biopsichas/SWATdoctR/).

``` r
# If the package 'remotes' is not installed run first:
install.packages("remotes")

# The installation of `SWATprepR`.
remotes::install_github("biopsichas/SWATdoctR")
```

## Getting started

To get started with `SWATdoctR`, please refer to the vignettes available
at:
[https://biopsichas.github.io/SWATdoctR/](https://biopsichas.github.io/SWATdoctR/articles/qa.html).

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-optain2022" class="csl-entry">

Christoph, Schürz, Čerkasova Natalja, Farkas Csilla, Nemes Attila,
Plunge Svajunas, Strauch Michael, Szabó Brigitta, and Piniewski Mikołaj.
2022. “<span class="nocase">SWAT+ modeling protocol for the assessment
of water and nutrient retention measures in small agricultural
catchments</span>.” Zenodo. <https://doi.org/10.5281/zenodo.7463395>.

</div>

<div id="ref-plunge2024a" class="csl-entry">

Plunge, Svajunas, Christoph Schürz, Natalja Čerkasova, Michael Strauch,
and Mikołaj Piniewski. 2024. “<span class="nocase">SWAT+ model setup
verification tool: SWATdoctR</span>.” *Environmental Modelling &
Software* 171: 105878. <https://doi.org/10.1016/j.envsoft.2023.105878>.

</div>

<div id="ref-plunge2024b" class="csl-entry">

Plunge, Svajunas, Brigitta Szabó, Michael Strauch, Natalja Čerkasova,
Christoph Schürz, and Mikołaj Piniewski. 2024.
“<span class="nocase">SWAT + input data preparation in a scripted
workflow: SWATprepR</span>.” *Environmental Sciences Europe* 36 (1): 53.
<https://doi.org/10.1186/s12302-024-00873-1>.

</div>

</div>
