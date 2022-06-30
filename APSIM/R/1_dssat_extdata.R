dssat.extdata <- function(xmin,xmax,ymin,ymax,res,sdate,edate,jobs,ex.name,path.to.extdata){
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  #Set working directory (where the file is)
  setwd(path.to.extdata)
  # Create grid
  grid = matrix(nrow = 0, ncol = 2)
  for (x in seq(xmin,xmax,res)) {for (y in seq(ymin,ymax,res)) {grid <- rbind(grid, c(x,y))}}
  # Create experiment directory
  dir.create(file.path(paste(path.to.extdata, ex.name, sep = "/")))
  # Process soil & weather
  foreach::foreach(pnt=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "apsimx")) %dopar% {
    dir.create(file.path(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, (as.integer(pnt)-1), flag = "0")), sep = "/")))
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, (as.integer(pnt)-1), flag = "0")), sep = "/"))
    # read coordinates of the point
    x = grid[pnt,1]
    y = grid[pnt,2]
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
        prec <- chirps::get_chirps(object = data.frame(lon = x, lat = y),
                                   dates = c(sdate, edate),
                                   server = "ClimateSERV")
        wth$rain <- round(prec$chirps, 2)
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
}
