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
  foreach::foreach(pnt=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "lubridate")) %dopar% {
    dir.create(file.path(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, (as.integer(pnt)-1), flag = "0")), sep = "/")))
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, (as.integer(pnt)-1), flag = "0")), sep = "/"))
    # read coordinates of the point
    x = grid[pnt,1]
    y = grid[pnt,2]
    ##########################################
    # Get soil ISRIC data
    s <- tryCatch(
      expr = {
        apsimx::get_isric_soil_profile(lonlat = c(x,y))
      },
      error = function(e){
        return(list(soil=data.frame(LL15=as.integer(-99),
                                    DUL=as.integer(-99),
                                    SAT=as.integer(-99),
                                    SKS=as.integer(-99),
                                    SSS=as.integer(-99),
                                    BDM=as.integer(-99),
                                    LOC=as.integer(-99),
                                    LCL=as.integer(-99),
                                    LSI=as.integer(-99),
                                    LNI=as.integer(-99),
                                    LHW=as.integer(-99),
                                    CEC=as.integer(-99))))
      })
    Depth<-c(5,15,30,60,100,200)
    LL15<-s$soil$LL15
    DUL<-s$soil$DUL
    SAT<-s$soil$SAT
    SKS<-s$soil$KS
    SSS<-round(as.numeric(SKS), digits = 1)
    BDM<-s$soil$BD
    LOC<-s$soil$Carbon
    LCL<-s$soil$ParticleSizeClay
    LSI<-s$soil$ParticleSizeSilt
    LNI<-as.numeric(s$soil$Nitrogen)*0.0001
    LHW<-s$soil$PH
    CEC<-s$soil$CEC
    sol <- DSSAT::read_sol("../../../../../base_data/soil.sol", id_soil = "IBPN910025")
    soilid <- sol %>%
      mutate(PEDON=paste0('TRAN', formatC(width = 6, (as.integer(pnt)-1), flag = "0")),
             SOURCE='ISRIC',
             TEXTURE=-99,
             DEPTH=max(Depth),
             DESCRIPTION=-99,
             SITE=-99,
             COUNTRY=-99,
             LAT=y,
             LONG=x,
             SLB=Depth,
             SLLL=LL15,
             SSAT=SAT,
             SDUL=DUL,
             SSKS=SSS,
             SBDM=BDM,
             SLOC=LOC,
             SLCL=LCL,
             SLSI=LSI,
             SLNI=LNI,
             SLHW=LHW,
             SCEC=CEC)
    DSSAT::write_sol(soilid, 'SOIL.SOL', append = FALSE)
    ##########################################
    # Get weather NASA POWER data
    w <- tryCatch(
      expr = {
        wth <- weathRman::get_nasa_power(lat = y, long = x,
                                         start = sdate, end = edate) %>%
          {attr(.,"comments") <- str_c("! ", attr(., "comments")); .}
        source("/home/jovyan/TRANSFORM/egb/chirps/chirps.R")
        prec <- chirps(startDate = sdate, endDate = edate, coordPoints = data.frame("X" = x, "Y" = y))
        wth$RAIN <- prec$RAIN
        g <- attr(wth, "GENERAL")
        g$INSI <- "N + C"
        attr(wth, "GENERAL") <- g
        wth
      },
      error = function(e){
        dates <- seq(as.Date(sdate), as.Date(edate), by = "day")
        err <- c("DATE","SRAD","TMAX","TMIN","RAIN","WIND","RHUM")
        wth <- matrix(-99, nrow = length(dates), ncol = length(err))
        wth <- as.data.frame(wth)
        names(wth) <- c("DATE","SRAD","TMAX","TMIN","RAIN","WIND","RHUM")
        wth[,names(wth) == "DATE"] <- dates
        t <- tibble("INSI" = "NULL", "LAT" = y, "LONG" = x, "ELEV" = -99, "TAV" = -99, "AMP" = -99, "REFHT" = -99, "WNDHT" = -99)
        attr(wth, "GENERAL") <- t
        return(wth)
      }
    )
    DSSAT::write_wth(w, paste0("WHTE", formatC(width = 4, (as.integer(pnt)-1), flag = "0"), ".WTH"))
    setwd(path.to.extdata)
    gc()
  }
}
