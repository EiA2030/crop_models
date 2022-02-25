dssat.expfile <- function(xmin,xmax,ymin,ymax,res,jobs,ex.name){
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
  # Process Experimental Files
  foreach::foreach(pnt=1:nrow(grid), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "here", "DSSAT")) %dopar% {
    #Read sample cultivar file and filter to only cultivar IF0014
    cul <- read_cul("MZCER047.CUL") %>%
      filter(`VAR#` == "IF0014")
    #Read sample cultivar file and filter to only ecotype IB0002
    eco <- read_eco("MZCER047.ECO") %>%
      filter(`ECO#` == "IB0002")
    #Read in original FileX
    file_x <- read_filex("ETBA8304.MZX")
    # Set the experimental directory
    setwd(paste(getwd(),ex.name,paste0('EXTE', formatC(width = 4, pnt, flag = "0")), sep = "/"))
    #Make proposed chnages to FileX
    file_x$FIELDS$WSTA<-paste0("WHTE", formatC(width = 4, pnt, flag = "0"))
    file_x$FIELDS$ID_SOIL<-paste0('SOTE', formatC(width = 6, pnt, flag = "0"))
    #Overwrite original FileX with new values
    write_filex(file_x,paste0('EXTE', formatC(width = 4, pnt, flag = "0"),'.MZX'))
    setwd(here::here())
  }
}

dssat.extdata(xmin = 36.66, xmax = 37.43, ymin = -1.35, ymax = -0.5, res = 0.1,
              jobs = 8, ex.name = "test_simulation")
