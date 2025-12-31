
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lasertrapr

<img src = "inst/app/www/lasertrapr-logo.gif">

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of lasertrapr is to automate the analysis of laser trap data.
The app is still being heavily developed, end-users should proceed with
caution. For more information and the official documentation refer to
the [website](https://myolab.org/software/lasertrapr).

Several peer-reviewed research articles have used either this app for some analyses or prior versions of the codebase:

1) [FRET and optical trapping reveal mechanisms of actin activation of the power stroke and phosphate release in myosin V](https://www.jbc.org/article/S0021-9258(17)50627-4/fulltext)

2) [A mutation in switch I alters the load-dependent kinetics of myosin Va](https://www.nature.com/articles/s41467-023-38535-0)

3) [Myosin's powerstroke occurs prior to the release of phosphate from the active site](https://onlinelibrary.wiley.com/doi/epdf/10.1002/cm.21682)

4) [Positional Isomers of a Non-Nucleoside Substrate Differentially Affect Myosin Function](https://www.sciencedirect.com/science/article/pii/S0006349520305002)

5) [Modeling thick filament activation suggests a molecular basis for force depression](https://www.cell.com/biophysj/pdf/S0006-3495(24)00040-7.pdf)

6)[Characterizing the concentration and load dependence of phosphate binding to rabbit fast skeletal actomyosin](https://www.pnas.org/doi/epdf/10.1073/pnas.2504758122)

7) [Danicamtiv reduces myosin’s working stroke but activates the thin filament by accelerating actomyosin attachment](https://www.pnas.org/doi/abs/10.1073/pnas.2515786122)

## Installation

Current stable version will be hosted on my `drat` repo. Note, this will install a very old version from 2022. 

``` r
# install.packages("drat")
drat::addRepo("brentscott93")
install.packages("lasertrapr")
```

You can install the most recent developmental version of lasertrapr from GitHub with: 
``` r
# install.packages("devtools")
devtools::install_github("brentscott93/lasertrapr")
```
Note: This will install a random snapshot from my last commit. A new more recent "drat" version will be added soon.

We are in the process of submitting for publication. After reviewer recommended changes are made we can make a stable release in the "drat" repository.
