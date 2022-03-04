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
  dir.create(file.path(paste(getwd(), ex.name, sep = "/")))
  # Process soil & weather
  foreach::foreach(pnt=0:nrow(grid), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "apsimx","DSSAT")) %dopar% {
    dir.create(file.path(paste(getwd(),ex.name,paste0('EXTE', formatC(width = 4, pnt, flag = "0")), sep = "/")))
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, pnt, flag = "0")), sep = "/"))
    # read coordinates of the point
    x = grid[pnt+1,1]
    y = grid[pnt+1,2]
    ##########################################
    # Get soil ISRIC data
    s <- get_isric_soil_profile(lonlat = c(x,y))
    Depth<-c(5,15,30,60,100,200)
    LL15<-s$soil$LL15
    DUL<-s$soil$DUL
    SAT<-s$soil$SAT
    SKS<-s$soil$KS
    SSS<-round(SKS, digits = 1)
    BDM<-s$soil$BD
    LOC<-s$soil$Carbon
    LCL<-s$soil$ParticleSizeClay
    LSI<-s$soil$ParticleSizeSilt
    LNI<-s$soil$Nitrogen*0.0001
    LHW<-s$soil$PH
    CEC<-s$soil$CEC
    sol <- read_sol("../../soil.sol", id_soil = "IBPN910025")
    write_sol(sol, "NEW.SOL", append = FALSE)
    ex_profile <- read_sol("../../NEW.SOL", id_soil = "IBPN910025")
    soilid <- ex_profile %>%
      mutate(PEDON=paste0('TRAN', formatC(width = 6, pnt, flag = "0")),
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
    write_sol(soilid, 'SOIL.SOL', append=F)
    ##########################################
    # Get weather NASA POWER data
    weathRman::get_nasa_power(lat = y, long = x,
                              start = sdate, end = edate) %>%
      {attr(.,"comments") <- str_c("! ", attr(., "comments")); .} %>%
      DSSAT::write_wth(paste0("WHTE", formatC(width = 4, pnt, flag = "0"), ".WTH"))
    setwd(path.to.extdata)
  }
}

# dssat.extdata(xmin = 36.66, xmax = 37.43, ymin = -1.35, ymax = -0.5, res = 0.5,
#               sdate = "1990-02-20", edate = "2015-02-20",
#               jobs = 4, ex.name = "test_simulation", path.to.extdata = "/path/to/extdata/")