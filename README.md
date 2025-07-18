suwo: access nature media repositories through R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Dependencies](https://tinyverse.netlify.com/badge/suwo)](https://cran.r-project.org/package=suwo)
[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- [![minimal R version](https://img.shields.io/badge/R%3E%3D-Depends:-6666ff.svg)](https://cran.r-project.org/)  -->
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](commits/develop)  -->
![GitHub last commit](https://img.shields.io/github/last-commit/maRce10/suwo)
[![Codecov test
coverage](https://codecov.io/gh/maRce10/suwo/branch/master/graph/badge.svg)](https://app.codecov.io/gh/maRce10/suwo?branch=master)
<!-- [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/suwo)](https://cran.r-project.org/package=suwo) -->
<!-- [![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/suwo)](https://cranlogs.r-pkg.org/badges/grand-total/suwo) -->
<!-- badges: end -->

<!-- <img src="man/figures/suwo_sticker.png" alt="suwo logo" align="right" width = "25%" height="25%"/> -->
<img src="man/figures/logo.png" alt="suwo logo" align="right" width = "25%" height="25%"/>

[suwo](https://github.com/maRce10/suwo) is intended streamline
searching/downloading of nature media files (e.g. audios, photos) from
online repositories.

[suwo](https://github.com/maRce10/suwo) offers functions for critical steps in the acquisition of data necessary to develop methods that require a large amount of data.

The main features of the package are:

- Obtaining media metadata from online repositories
- Downloading associated media files
- Updating data sets with new records

THE PACKAGE IS ON AN EARLY DEVELOPMENTAL STAGE. We are currently
searching for new online repositories to be included in the package.
Please [open a new
issue](https://github.com/maRce10/suwo/issues/new/choose) if you want to
suggest a new repository.

Online repositories currently supported:
- INaturalist
- Wikiaves
- Macaulay Library
- Xenocanto
- Observation.org

## Installing suwo

The package is only available on github at this time.
[github](https://github.com/). You will need the R package
[remotes](https://cran.r-project.org/package=remotes) to install it:

``` r
remotes::install_github("maRce10/suwo")

#load package
library(suwo)
```

Please cite [suwo](https://github.com/maRce10/suwo) as follows:

Araya-Salas, M., & Elizondo-Calvo, J. 2023. suwo: access nature media
repositories through R. R package version 0.1.0.
