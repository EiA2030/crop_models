# ## required libraries
# library(Rwofost)
# library(doParallel)
# 
# # Set number of parallel workers (max. 16)
# cls <- parallel::makePSOCKcluster(10)
# doParallel::registerDoParallel(cls)
# 
# # Points to simulate over
# pts <- unique(weather,by = c('X','Y'))[,c(2,3)]
# # Parallelize the creation of weather data
# wofost.pts <- foreach::foreach(p=1:nrow(pts), .packages = c("data.table")) %dopar% {
#   x <- pts$X[p]
#   y <- pts$Y[p]
#   w <- weather[ weather$X == x & weather$Y == y, ]
#   w <- w[, (c("wind.U", "wind.V")) := NULL]
#   w <- setcolorder(w, c("date", "X", "Y", "srad", "tmin", "tmax", "vapr", "wind", "prec"))
# }
# 
# Parallelize the WOFOST simulations in each point
Twofost <- function(weather, cultivar, soil.type, sdate, edate, jobs){
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  # Points to simulate over
  pts <- unique(weather,by = c('X','Y'))[,c(2,3)]
  # Parallelize the creation of weather data
  wofost.pts <- foreach::foreach(p=1:nrow(pts), .packages = c("data.table")) %dopar% {
    x <- pts$X[p]
    y <- pts$Y[p]
    w <- weather[ weather$X == x & weather$Y == y, ]
    w <- w[, (c("wind.U", "wind.V")) := NULL]
    w <- setcolorder(w, c("date", "X", "Y", "srad", "tmin", "tmax", "vapr", "wind", "prec"))
  }
  wofost.Ps <- list()
  foreach::foreach(p=seq_along(wofost.pts), .packages = c("data.table", "Rwofost")) %do% {
    # Create point name
    wofost.Ps[paste('point',p,sep = '_')] <- p
    # Create point coordinates
    wofost.Ps[[paste('point',p,sep = '_')]] <- list(X = wofost.pts[[p]]$X[1], Y = wofost.pts[[p]]$Y[1])
    # Create point weather data
    wth <- setnames(data.table(wofost.pts[[p]][, !c("X", "Y")]), c("date", "srad", "tmin", "tmax", "vapr", "wind", "prec"))
    wth$date <- as.Date(wth$date)
    # Define WOFOST model params:
    # # Define crop
    crop <- wofost_crop(cultivar)
    # # Define soil type
    soil <- wofost_soil(soil.type)
    # # Define control settings
    contr <- wofost_control()
    contr$modelstart <- as.Date(sdate)
    # Run single simulation for p-nth
    pdates <- as.character(seq(as.Date(sdate), as.Date(edate), "days")) # Create sequence of planting dates
    # Execute the WOFOST model for each of the selected planting dates (pdates)
    m <- 1
    for(d in pdates){
      # Define start date of the model:
      contr$modelstart <- as.Date(d)
      # Execute WOFOST model:
      out <- wofost(crop, wth, soil, contr)
      # Attach output to list
      wofost.Ps[[c(p,m+2)]] <- out
      # Rename data.frames to the date
      names(wofost.Ps[[p]])[[m+2]] <- paste('pdate',format(as.Date(d),'%Y%m%d'),sep = '_')
      m <- m + 1
    }
  }
  return(wofost.Ps)
}
# wofost.Ps <- Twofost(weather = weather, cultivar = 'maize_1', soil.type = 'ec1', sdate = '2019-10-01', edate = '2020-05-31')
# wofost.Ps
