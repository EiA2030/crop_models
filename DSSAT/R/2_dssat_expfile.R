dssat.expfile <- function(xmin,xmax,ymin,ymax,res,jobs,ex.name,path.to.extdata){
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
  # Process Experimental Files
  foreach::foreach(pnt=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
    #Read sample cultivar file and filter to only cultivar IF0014
    cul <- read_cul("MZCER047.CUL") %>%
      filter(`VAR#` == "IF0014")
    #Read sample cultivar file and filter to only ecotype IB0002
    eco <- read_eco("MZCER047.ECO") %>%
      filter(`ECO#` == "IB0002")
    #Read in original FileX
    file_x <- read_filex("ETBA8304.MZX")
    # Set the experimental directory
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, pnt, flag = "0")), sep = "/"))
    #Make proposed chnages to FileX
    file_x$FIELDS$WSTA<-paste0("WHTE", formatC(width = 4, pnt, flag = "0"))
    file_x$FIELDS$ID_SOIL<-paste0('TRAN', formatC(width = 6, pnt, flag = "0"))
    #Overwrite original FileX with new values
    write_filex(file_x,paste0('EXTE', formatC(width = 4, pnt, flag = "0"),'.MZX'))
    setwd(path.to.extdata)
  }
}

dssat.expfile(xmin = 37, xmax = 38, ymin = 0, ymax = 1, res = 0.5,
              jobs = 4, ex.name = "test_simulation", path.to.extdata = "/media/TRANSFORM-EGB/eia2030/TRANSFORM2030_CropModels/crop_models/DSSAT/R/extdata/")
