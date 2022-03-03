#!/usr/bin/env Rscript
## Script to correct DSSAT PROFILE for Linux & Install DSSAT for R
devtools::install_github('https://github.com/palderman/DSSAT')
devtools::install_github("palderman/weathRman")
devtools::install_github("femiguez/apsimx")
devtools::install_github("ropensci/nasapower")
pkgs <- c('soilDB',
          'ggplot2',
          'doParallel',
          'foreach',
          'here')
install.packages(pkgs)