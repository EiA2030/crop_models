#' Title Execute APSIMX
#'
#' @param coords \strong{data.frame} with XY (Longitude, Latitude) format indicating the coordinate points in WGS84.
#' @param jobs \strong{integer} with the number of parallel processes. Default is 1 (non-parallel)
#' @param ex.name \strong{character string} with the name of the experiment (must be same as of .apsimx).
#' @param path.to.extdata \strong{character string} with the full path to the directory where the outputs of the simulation will be written. Needs to contain an .apsimx file.
#' @return void
#'
#' @examples dssat.expfile(coords = data.frame("LON" = c(7.5, 7.94), "LAT" = c(10.634, 11.12)), jobs = 2, ex.name = "ABCD12345", path.to.extdata = "path/to/")

apsimx.exec <- function(coords,jobs =1,ex.name,path.to.extdata){
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  #Set working directory (where the file is)
  setwd(path.to.extdata)
  # Process Experimental Files
  foreach::foreach(pnt=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "apsimx")) %dopar% {
    # Set the experimental directory
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0")), sep = "/"))
    # Set executable path
    apsimx::apsimx_options(exe.path = "/usr/local/lib/apsim/bin/Release/netcoreapp3.1/Models")
    # Execute APSIMX
    apsimx::apsimx(paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '.apsimx'))
    setwd(path.to.extdata)
    gc()
  }
  rm(list=ls(name = foreach:::.foreachGlobals), pos = foreach:::.foreachGlobals)
}
