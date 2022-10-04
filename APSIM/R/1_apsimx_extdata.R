#' Title Download and prepare soil and weather data inputs for APSIMX
#'
#' @param coords \strong{data.frame} with XY (Longitude, Latitude) format indicating the coordinate points in WGS84.
#' @param sdate \strong{character string} with the date for the start of the simulation and YYYY-MM-DD format.
#' @param edate \strong{character string} with the date for the end of the simulation and YYYY-MM-DD format.
#' @param jobs \strong{integer} with the number of parallel processes. Default is 1 (non-parallel)
#' @param ex.name \strong{character string} with the name of the name of the experiment name (same as for .apsimx).
#' @param path.to.extdata \strong{character string} with the full path to the directory where the outputs of the simulation will be written. Needs to contain an .apsimx file.
#' @return void
#'
#' @examples apsimx.extdata(coords = data.frame("LON" = c(7.5, 7.94), "LAT" = c(10.634, 11.12)), sdate = "2021-03-14", edate "2021-10-21", jobs = 2, ex.name = "ABCD12345.apsimx", path.to.extdata = "path/to/")

apsimx.extdata <- function(coords,sdate=NULL,edate=NULL,jobs =1,ex.name,path.to.extdata){
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  #Set working directory (where the file is)
  setwd(path.to.extdata)
  # Create experiment directory
  dir.create(file.path(paste(path.to.extdata, ex.name, sep = "/")))
  # Process soil & weather
  foreach::foreach(pnt=seq_along(coords[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "lubridate", "apsimx")) %dopar% {
    dir.create(file.path(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, (as.integer(pnt)-1), flag = "0")), sep = "/")))
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, (as.integer(pnt)-1), flag = "0")), sep = "/"))
    # read coordinates of the point
    x = coords[pnt,1]
    y = coords[pnt,2]
    ##########################################
    # Get soil ISRIC data
    s <- tryCatch(
      expr = {
        sp <- apsimx::get_isric_soil_profile(lonlat = c(x,y))
      },
      error = function(e){
        sp <- apsimx::get_isric_soil_profile(lonlat = c(x,y))
        sp$soil[,] <- NA
        return(sp)
      })
    edit_apsimx_replace_soil_profile(paste0(ex.name,".apsimx"), soil.profile = s, src.dir = "../../", wrt.dir = ".")
    file.rename(list.files(pattern = "*-edited.apsimx", full.names = TRUE), paste0(path.to.extdata, '/', ex.name, '/', 'EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '/', 'EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '.apsimx'))
    ##########################################
    # Get weather NASA POWER data
    w <- tryCatch(
      expr = {
        wth <- apsimx::get_power_apsim_met(c(x, y), 
                                   dates = c(sdate, edate))
        attr(wth, "longitude") <- paste0("longitude = ", x)
        attr(wth, "latitude") <- paste0("latitude = ", y)
        attr(wth, "tav") <- paste0("tav = ", mean((wth$maxt+wth$mint)/2))
        attr(wth, "amp") <- paste0("amp = ", (max(wth$maxt)-min(wth$mint))/2)
        wth <- impute_apsim_met(wth)
        wth
      },
      error = function(e){
        wth <- apsimx::get_power_apsim_met(c(0,0), dates = c(sdate, edate))
        attr(wth, "site") <- paste0("WHTE", formatC(width = 4, (as.integer(pnt)-1), flag = "0"))
        attr(wth, "longitude") <- paste0("longitude = ", x)
        attr(wth, "latitude") <- paste0("latitude = ", y)
        attr(wth, "tav") <- paste0("tav = NA")
        attr(wth, "amp") <- paste0("amp = NA")
        wth[,] <- NA
        return(wth)
      }
    )
    apsimx::write_apsim_met(w, wrt.dir = ".", filename = paste0("WHTE", formatC(width = 4, (as.integer(pnt)-1), flag = "0"), ".met"))
    edit_apsimx(paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '.apsimx'),
                node = "Weather", value = paste0("WHTE", formatC(width = 4, (as.integer(pnt)-1), flag = "0"), ".met"),
                overwrite = TRUE)
    setwd(path.to.extdata)
    gc()
  }
  rm(list=ls(name = foreach:::.foreachGlobals), pos = foreach:::.foreachGlobals)
}
