pkgs <- c('soilDB',
          'ggplot2',
          'odbc',
          'doParallel',
          'foreach')
install.packages(pkgs)
devtools::install_github("ropensci/nasapower")
devtools::install_github("ropensci/chirps")
devtools::install_github("femiguez/apsimx")
