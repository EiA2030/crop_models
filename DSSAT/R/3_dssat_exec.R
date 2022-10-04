#' Title Execute DSSAT Simulation
#'
#' @param coords \strong{data.frame} with XY (Longitude, Latitude) format indicating the coordinate points in WGS84.
#' @param jobs \strong{integer} with the number of parallel processes. Default is 1 (non-parallel)
#' @param path.to.extdata \strong{character string} with the full path to the directory where the outputs of the simulation will be written.
#' @param ex.file \strong{character string} with the name of the name of the experimental file. Needs to be locatedinside path.to.extdata. For example: "ABCD12345.MZX"
#' @return void
#'
#' @examples dssat.expfile(coords = data.frame("LON" = c(7.5, 7.94), "LAT" = c(10.634, 11.12)), jobs = 2, ex.name = "ABCD12345.MZX", path.to.extdata = "path/to/")

dssat.exec <- function(coords,jobs = 1,ex.name,path.to.extdata){
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  #Set working directory (where the file is)
  setwd(path.to.extdata)
  # Process Experimental Files
  foreach::foreach(pnt=seq_along(coords[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
    # Set the experimental directory
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0")), sep = "/"))
    # Generate a DSSAT batch file using a tibble
    options(DSSAT.CSM="/DSSAT47/dssat-csm-os/build/bin/dscsm047")
    tibble(FILEX=paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"),'.MZX'), TRTNO=1:9, RP=1, SQ=0, OP=0, CO=0) %>%
      write_dssbatch()
    # Run DSSAT-CSM
    run_dssat(suppress_output = TRUE)
    # Change output file name
    file.rename(list.files(pattern = "Summary.*", full.names = TRUE), paste0(path.to.extdata, '/', ex.name, '/', 'EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '/', 'EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '.OUT'))
    setwd(path.to.extdata)
    gc()
  }
  rm(list=ls(name = foreach:::.foreachGlobals), pos = foreach:::.foreachGlobals)
}
