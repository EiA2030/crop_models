#test
sp.wofost <- function(crop, soil, sdate, edate, parameter){
    wofost.Ps <- list() # Start an empty list
    n <- 1
    # Create a list with all the points and simulations
    for (p in wofost.pts) {
      wofost.Ps[paste('point',n,sep = '_')] <- n # Give a label to each point ('point_n')
      wofost.Ps[[paste('point',n,sep = '_')]] <- list(X = pts$X[n], Y = pts$Y[n]) # Add XY coordinates as values for each point_n element
      # Variable obj with weather parameters:
      wth <- p
      wth$date <- as.Date(wth$date)
      colnames(wth) <- c("date", "srad", "tmin", "tmax", "vapr", "wind", "prec")  
      # Define WOFOST model params:
      # Define crop
      crop <- wofost_crop(crop)
      # Define soil type
      soil <- wofost_soil(soil)
      # Define control settings
      contr <- wofost_control()
      contr$modelstart <- as.Date(sdate)
      ###################################
      # These are nt used aATM
      # # Define max model duration
      # # contr$max_duration <- 500
      # # Set the model to continue running after reaching senescence
      # # contr$stop_maturity <- 1
      ####################################
      # Run single simulation for p-nth
      pdates <- as.character(seq(format(as.Date(sdate), "%Y%m%d"), format(as.Date(edate), "%Y%m%d"), "days")) # Create sequence of planting dates
      # Execute the WOFOST model for each of the selected planting dates (pdates)
      m <- 1
      for(d in pdates){
        # Define start date of the model:
        contr$modelstart <- as.Date(d)
        # Execute WOFOST model:
        out <- wofost(crop, wth, soil, contr)
        # Add XY coordinates
        out$X <- pts$X[m]
        out$Y <- pts$Y[m]
        # Attach output to list
        wofost.Ps[[c(n,m+2)]] <- out
        # Rename data.frames to the date
        names(wofost.Ps[[n]])[[m+2]] <- paste('pdate',format(as.Date(d),'%Y%m%d'),sep = '_')
        m <- m +1
      }
      n <- n + 1
    }
    # Spatial output
    intermediate <- data.frame() # Generate empty DF
    n <- 1
    for(i in wofost.Ps){ # Loop through all the points in the list 
      m <- 1
      for(j in i){ # Loop through all the planting dates
       pd <- names(wofost.Ps[[n]][m]) # Get the DF outputs of each simulated planting date
        if(nchar(pd) > 2) { # Skip XY sub-elements in the list
          xp <- data.frame(j[,14]) # Add X column
          xp <- i[1]
          yp <- data.frame(j[,15]) # Add Y column
          yp <- i[2]
          pldate <- data.frame(format(as.Date(as.character(j[,1]), '%Y-%m-%d'))) # Add planting date (works as kind of an ID)
          pldate[,1] <- substr(pd, nchar(pd)-8+1, nchar(pd)) 
          pldate <- cbind(pldate,xp,yp) # Combine the three keys: planting date, X and Y
          wofost.param <- data.frame(j[,c("date",parameter)]) # Add the simulated date, together with the parameter of interest
          wofost.param <- cbind(pldate, wofost.param) # Combine keys and selected model outputs
          intermediate <- rbind(intermediate, wofost.param) # Append to intermediate DF
        }
        m <- m + 1
      }
      n <- n +1
    }
    colnames(intermediate) <- c("sowing_date", "X", "Y", "sim_date",parameter) # Format intermediate output
    # Finding optimal planting date to maximize WSO ()
    n <- 1
    output <- data.frame() # Generate empty DF
    for (x in dplyr::distinct(a,X,Y)[,1]) { # Loop through all the individual points and simulated dates
      x <- dplyr::distinct(a,X,Y)[n,1]
      y <- dplyr::distinct(a,X,Y)[n,2]
      new <- subset(a , X == x | Y == y) # Search in each of the points
      val <- as.integer(new$plant_date[which.max(new$WSO)]) # Which is the simulated date with the maximum parameter value? --> Select that planting date
      output1 <- cbind.data.frame(val,x,y) # Put results together
      output <- rbind.data.frame(output,output1) # Append to final DF
      n <- n + 1
    }
    # Spatial outputs
    # First create a grid
    grd <- st_join(st_as_sf(st_make_grid(st_as_sfc(st_bbox(st_as_sf(output,
                                                                    coords = c("x", "y"),
                                                                    crs = 4326)) + c(-0.05/2, -0.05/2, 0.05/2, 0.05/2)), # This is to create the cells with the points as centroids
                                         what="polygons",
                                         cellsize=0.05)), # This is the selected target resolution
                   st_as_sf(output,
                            coords = c("x", "y"),
                            crs = 4326))
    # Rasterize the grid
    r <- rasterize(terra::vect(as(grd, "Spatial")),
                   terra::rast(ext = terra::ext(terra::vect(as(grd, "Spatial"))) + 0.05,
                               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                               resolution = 0.05),
                   field = output$val, fun="last")    
    return(output)
}