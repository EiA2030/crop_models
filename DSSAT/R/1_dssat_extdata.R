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
  foreach::foreach(pnt=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "apsimx","DSSAT")) %dopar% {
    dir.create(file.path(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, (as.integer(pnt)-1), flag = "0")), sep = "/")))
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, (as.integer(pnt)-1), flag = "0")), sep = "/"))
    # read coordinates of the point
    x = grid[pnt,1]
    y = grid[pnt,2]
    ##########################################
    # Get soil ISRIC data
    s <- tryCatch(
      expr = {
        get_isric_soil_profile(lonlat = c(x,y))
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
    sol <- read_sol("../../soil.sol", id_soil = "IBPN910025")
    write_sol(sol, "NEW.SOL", append = FALSE)
    ex_profile <- read_sol("../../NEW.SOL", id_soil = "IBPN910025")
    soilid <- ex_profile %>%
      mutate(PEDON=paste0('TRAN', formatC(width = 6, (as.integer(pnt)-1), flag = "0")),
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
    write_sol(soilid, 'SOIL.SOL', append = FALSE)
    ##########################################
    # Get weather NASA POWER data
    w <- tryCatch(
      expr = {
        wth <- weathRman::get_nasa_power(lat = y, long = x,
                                         start = sdate, end = edate) %>%
          {attr(.,"comments") <- str_c("! ", attr(., "comments")); .}
        prec <- chirps::get_chirps(object = data.frame(lon = x, lat = y),
                                   dates = c(sdate, edate),
                                   server = "ClimateSERV")
        wth$RAIN <- prec$chirps
        wth
      },
      error = function(e){
        wth <- weathRman::get_nasa_power(lat = 0, long = 0,
                                         start = sdate, end = edate) %>%
          {attr(.,"comments") <- str_c("! ", attr(., "comments")); .}
        wth[,names(wth) != "DATE"] <- -99
        t <- tibble(INSI = "NASA", LAT = y, LONG = x, ELEV = -99, TAV = -99, AMP = -99, REFHT = -99, WNDHT = -99)
        attr(wth, "GENERAL") <- t
        return(wth)
      }
    )
    write_wth(w, paste0("WHTE", formatC(width = 4, (as.integer(pnt)-1), flag = "0"), ".WTH"))
    setwd(path.to.extdata)
    gc()
  }
}
