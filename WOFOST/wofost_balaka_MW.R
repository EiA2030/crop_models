library(Rwofost)
library(data.table)

# Fetch Balaka (MW) weather data
weather <- data.table(read.csv("/home/jovyan/work/git/weather_wofost_balaka_MW.csv"))

# Convert weather variables to the correct units
weather$srad_kJ.m2 <- weather$srad_J.m2/1000
weather$tmin_C <- weather$tmin_K-273
weather$tmax_C <- weather$tmax_K-273
weather$vapr_kPa <- weather$vapr_kg.m2*0.00980665
weather$wind_ms <- sqrt(weather$windU_m.s^2 + weather$windV_m.s^2) # https://stackoverflow.com/questions/21484558/how-to-calculate-wind-direction-from-u-and-v-wind-components-in-r

# Delete columns of the old units
cols2delete <- c("X", "Y", "srad_J.m2", "tmin_K", "tmax_K", "vapr_kg.m2", "windU_m.s", "windV_m.s")

# Create a temporal sub-dataset for each spatial unit (point)
pts <- unique(weather,by = c('X','Y'))[,c(2,3)]
n <- 1
for(p in 1:nrow(pts)){
  x <- pts$X[p]
  y <- pts$Y[p]
  w <- weather[ weather$X == x & weather$Y == y, ]
  w <- w[, (cols2delete) := NULL]
  w <- setcolorder(w, c("date", "srad_kJ.m2", "tmin_C", "tmax_C", "vapr_kPa", "wind_ms", "prec_mm"))
  assign(paste0('p',n,sep = ''), w , envir = .GlobalEnv)
  n <- n + 1
}

# Simulation for point #1
##########################################################
wth <- p1
wth$date <- as.Date(wth$date)
colnames(wth) <- c("date", "srad", "tmin", "tmax", "vapr", "wind", "prec")
head(wth)

crop <- wofost_crop("maize_1")

soil <- wofost_soil('ec1')

contr <- wofost_control()
contr$modelstart <- as.Date("2010-11-01")
# contr$max_duration <- 500
contr$stop_maturity <- 1

# Run scenarios
pdates <- as.character(seq(as.Date("2010-08-31"), as.Date("2014-08-31"), "days"))
wofost.sims <- list() # Create an empty list for all the simulations

n <- 1
for(d in pdates){
    contr$modelstart <- as.Date(d)
    out <- wofost(crop, wth, soil, contr)
    assign(paste('pdate',d,sep = '_'), wofost(crop, wth, soil, contr) , envir = .GlobalEnv)
    wofost.sims[[n]] <- out # Add output simulation to list
    n <- n +1
}

cols <- rainbow(365*4)

# Lines
for(d in pdates){
    sim <- get(paste('pdate',d,sep = '_'), envir = .GlobalEnv)
    if(d == "2010-08-31"){
        plot(sim$date, sim$WSO, type="l",col=cols, xlim = c(as.Date("2010-10-01"),as.Date("2014-12-31")), ylim = c(0, 10000))
    } else {
        lines(sim$date,sim$WSO, col=cols)
    }
}

# Max yield (WSO) for each planting date
for(d in pdates){
    sim <- get(paste('pdate',d,sep = '_'), envir = .GlobalEnv)
    if(d == "2010-08-31"){
        plot(as.Date(d), max(sim$WSO), type="p",pch=20,cex=0.5,col=cols, xlim = c(as.Date("2010-08-31"),as.Date("2014-08-31")), ylim = c(0, 10000))
    } else {
        points(as.Date(d), max(sim$WSO), pch=20,cex=1, col=cols)
    }
}