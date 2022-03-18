sp_Twofost <- function(weather, cultivar, soil.type, sdate, edate, parameter, jobs){
  require(terra)
  require(sf)
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  # Run WOFOST
  wofost.Ps <- Twofost(weather, cultivar, soil.type, sdate, edate, jobs)
  # Spatial output
  intermediate <- data.table::data.table()
  for (p in seq_along(wofost.Ps)) {
    i <- wofost.Ps[[p]]
    c <- foreach::foreach(d=seq_along(i), .export = '.GlobalEnv', .combine = 'rbind', .inorder = TRUE, .packages = c("data.table")) %dopar% {
      pd <- names(i[d])
      if(nchar(pd) > 1) { # Skip XY sub-elements in the list
        xp <- data.table(i[[1]]) # Add X column
        yp <- data.table(i[[2]]) # Add Y column
        pldate <- data.table(format(as.Date(as.character(pd), '%Y-%m-%d'))) # Add planting date (works as kind of an ID)
        pldate[,1] <- substr(pd, nchar(pd)-8+1, nchar(pd))
        pldate <- cbind(pldate,xp,yp) # Combine the three keys: planting date, X and Y
        wofost.param <- data.table(i[[d]][,c("date", parameter)]) # Add the simulated date, together with the parameter of interest
        wofost.param <- cbind(pldate, wofost.param) # Combine keys and selected model outputs
      }
    }
    intermediate <- rbind(intermediate, c) # Append to intermediate DF
  }
  colnames(intermediate) <- c("sowing_date", "X", "Y", "sim_date", parameter) # Format intermediate output
  # Finding optimal planting date to maximize WSO ()
  n <- 1
  output <- data.frame() # Generate empty DF
  s <- foreach::foreach(p=seq_along(c(dplyr::distinct(intermediate,X,Y)[,1])[[1]]), .export = '.GlobalEnv', .combine = 'rbind', .inorder = TRUE, .packages = c("data.table")) %dopar% {
    x <- c(dplyr::distinct(intermediate,X,Y)[,1])[[1]][p]
    y <- c(dplyr::distinct(intermediate,X,Y)[,2])[[1]][p]
    new <- as.data.frame(subset(intermediate , X == x | Y == y)) # Search in each of the points
    val <- as.integer(new$sowing_date[which.max(new[, parameter])]) # Which is the simulated date with the maximum parameter value? --> Select that planting date
    output1 <- if(length(val) != 0){
      cbind.data.frame(val,x,y) # Put results together
    }
  }
  output <- rbind.data.frame(output,s) # Append to final DF
  # # # Spatial outputs
  # # # First create a grid
  grd <- st_join(st_as_sf(st_make_grid(st_as_sfc(st_bbox(st_as_sf(output,
                                                                  coords = c("x", "y"),
                                                                  crs = 4326)) + c(-0.05/2, -0.05/2, 0.05/2, 0.05/2)), # This is to create the cells with the points as centroids
                                       what="polygons",
                                       cellsize=0.05)), # This is the selected target resolution
                 st_as_sf(output,
                          coords = c("x", "y"),
                          crs = 4326))
  # # # # Rasterize the grid
  r <- terra::rasterize(terra::vect(grd),
                        terra::rast(ext = terra::ext(terra::vect(grd)) + 0.05,
                                    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                                    resolution = 0.05),
                        field = grd$val)
  return(list(table = output, points = grd, raster = r))
  # return(list(table = intermediate))
}
# source("2_format_data_RW.R")
# source("3_run_wofost_RW.R")
# a <- spwofost_RW <- sp_Twofost('maize_1', 'ec1', '2019-12-01', '2020-05-01', 'LAI')
# rwa <- st_read("gadm36_RWA_shp/gadm36_RWA_0.shp")
# plot(a$raster, add = T)
# plot(rwa$geometry)
