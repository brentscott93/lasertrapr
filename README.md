
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
the [website](https://lasertrapr.app).

Project future: The goal is to have the app published in the Journal of Open Source Software and rOpenSci. Updates and tests need to be added to prepare this for submission. Additionally, updates to support co-variance and two detector systems are being developed as well as an analyzer for isometric force clamping. 

## Installation

Current stable version will be hosted on my `drat` repo. 

``` r
# install.packages("drat")
drat::addRepo("brentscott93")
install.packages("lasertrapr")
```

You can install the development version of lasertrapr from GitHub with. 
``` r
# install.packages("devtools")
devtools::install_github("brentscott93/lasertrapr")
```
Note: This will install a random snapshot from my last commit. 

Disclaimer: This projected was started when I was in grad school at UMass Amherst in the Muscle Biophysics lab. I currently am at WashU in St. Louis in the Greenberg Lab, the developers of the MATLAB based SPASM program for analyzing lasertrap data. Development of lasertrapr is still on-going but at a slower pace than previously. If you want to use lastertrapr and could use a specific update, please open an issue explaining your need. 
