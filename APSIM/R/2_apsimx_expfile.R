#' Title Prepare APSIMX experimental parameters in .apsimx
#'
#' @param coords \strong{data.frame} with XY (Longitude, Latitude) format indicating the coordinate points in WGS84.
#' @param sdate \strong{character string} with the date for the start of the simulation and YYYY-MM-DD format.
#' @param edate \strong{character string} with the date for the end of the simulation and YYYY-MM-DD format.
#' @param jobs \strong{integer} with the number of parallel processes. Default is 1 (non-parallel)
#' @param ex.name \strong{character string} with the name of the experiment (must be same as of .apsimx).
#' @param path.to.extdata \strong{character string} with the full path to the directory where the outputs of the simulation will be written. Needs to contain an .apsimx file.
#' @return void
#'
#' @examples dssat.expfile(coords = data.frame("LON" = c(7.5, 7.94), "LAT" = c(10.634, 11.12)), sdate = "2021-03-14", edate "2021-10-21", jobs = 2, ex.name = "ABCD12345", path.to.extdata = "path/to/")

apsimx.expfile <- function(coords,sdate=NULL,edate=NULL,jobs =1,ex.name,path.to.extdata){
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  #Set working directory (where the file is)
  setwd(path.to.extdata)
  # Format dates
  sdate <- format(as.Date(sdate, "%Y-%m-%d"), "%Y-%m-%dT%H:%M:%S")
  edate <- format(as.Date(edate, "%Y-%m-%d"), "%Y-%m-%dT%H:%M:%S")
  # Function to add months
  add.months <- function(date,n) seq(as.Date(date), by = paste (n, "months"), length = 2)[2]
  # Process Experimental Files
  foreach::foreach(pnt=seq_along(coords[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "apsimx")) %dopar% {
    # Set the experimental directory
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0")), sep = "/"))
    # Set start and end of simulation
    apsimx::edit_apsimx(paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"),'.apsimx'),
                        node = "Clock",
                        parm = c("Start", "End"),
                        value = c(sdate, edate),
                        overwrite = TRUE)
    # Define planting date
    apsimx::edit_apsimx(paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '.apsimx'), node = "Crop",
                        parm = "StartDate", value = tolower(sub("^0+", "", format(as.Date(sdate, "%Y-%m-%d"), "%d-%b"))))
    # Define end date
    apsimx::edit_apsimx(paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '.apsimx'), node = "Crop",
                        parm = "EndDate", value = tolower(sub("^0+", "", format(as.Date(add.months(sdate,6), "%Y-%m-%d"), "%d-%b"))))
    ####################################################################################################################
    setwd(path.to.extdata)
    gc()
  }
  rm(list=ls(name = foreach:::.foreachGlobals), pos = foreach:::.foreachGlobals)
}
