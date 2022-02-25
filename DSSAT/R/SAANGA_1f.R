dssat.extdata <- function(xmin,xmax,ymin,ymax,res,sdate,edate,jobs,ex.name){
  require(doParallel)
  require(foreach)
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  #Set working directory (where the file is)
  setwd(here::here())
  # Create grid
  grid = matrix(nrow = 0, ncol = 2)
  for (x in seq(xmin,xmax,res)) {for (y in seq(ymin,ymax,res)) {grid <- rbind(grid, c(x,y))}}
  # Create experiment directory
  dir.create(file.path(paste(getwd(), ex.name, sep = "/")))
  # Process soil & weather
  foreach::foreach(pnt=1:nrow(grid), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "here", "DSSAT")) %dopar% {
    dir.create(file.path(paste(getwd(),ex.name,paste0('EXTE', formatC(width = 4, pnt, flag = "0")), sep = "/")))
    setwd(paste(getwd(),ex.name,paste0('EXTE', formatC(width = 4, pnt, flag = "0")), sep = "/"))
    # read coordinates of the point
    x = grid[pnt,1]
    y = grid[pnt,2]
    ##########################################
    Depth<-c(5,15,30,60,100,200)
    LL15<-c(0.15,0.2,0.2,0.3,0.35,0.2)
    DUL<-c(0.3,0.34,0.4,0.5,0.5,0.375)
    SAT<-c(0.5,0.5,0.5,0.5,0.5,0.5)
    SKS<-c(0.06,0.06,0.06,0.06,0.06,0.06)
    SSS<-round(SKS, digits = 1)
    BDM<-c(1.29,1.45,1.44,1.44,1.44,1.44)
    LOC<-c(0.71,0.59,0.59,0.42,0.32,0.3)
    LCL<-c(42,43,47,49,47,49)
    LSI<-c(20,19,19,19,19,19)
    LNI<-c(0.06,0.05,0.05,0.04,0.03,0.02)
    LHW<-c(5.7,5.6,5.6,5.8,5.8,5.8)
    sol <- read_sol("../../soil.sol", id_soil = "IBPN910025")
    write_sol(sol, "NEW.SOL", append = FALSE)
    ex_profile <- read_sol("../../NEW.SOL", id_soil = "IBPN910025")
    soilid <- ex_profile %>%
      mutate(PEDON=paste0('SOTE', formatC(width = 6, pnt, flag = "0")),
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
             SLHW=LHW)
    write_sol(soilid, 'SOIL.SOL', append=TRUE)
    ##########################################
    # Get weather NASA POWER data
    weathRman::get_nasa_power(lat = y, long = x,
                              start = sdate, end = edate) %>%
      {attr(.,"comments") <- str_c("! ", attr(., "comments")); .} %>%
      DSSAT::write_wth(paste0("WHTE", formatC(width = 4, pnt, flag = "0"), ".WTH"))
    setwd(here::here())
  }
}

dssat.extdata(xmin = 36.66, xmax = 37.43, ymin = -1.35, ymax = -0.5, res = 0.1,
              sdate = "2020-02-20", edate = "2022-02-22",
              jobs = 8, ex.name = "test_simulation")
