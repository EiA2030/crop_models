dssat.exec <- function(xmin,xmax,ymin,ymax,res,jobs,ex.name,path.to.extdata){
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
<<<<<<< HEAD
  foreach::foreach(pnt=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
=======
  foreach::foreach(pnt=0:nrow(grid), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
>>>>>>> 89601bb82a6910b66c43b9f0486f19e03d4e716d
    # Set the experimental directory
    setwd(paste(getwd(),ex.name,paste0('EXTE', formatC(width = 4, pnt, flag = "0")), sep = "/"))
    # Generate a DSSAT batch file using a tibble
    options(DSSAT.CSM="/DSSAT47/dssat-csm-os/build/bin/dscsm047")
    tibble(FILEX=paste0('EXTE', formatC(width = 4, pnt, flag = "0"),'.MZX'), TRTNO=1:9, RP=1, SQ=0, OP=0, CO=0) %>%
      write_dssbatch()
    # Run DSSAT-CSM
    run_dssat()
    # Change output file name
    # file.rename(list.files(pattern = "Summary.*"), paste0(paste0('EXTE', formatC(width = 4, pnt, flag = "0"), '.OUT')))
    setwd(path.to.extdata)
  }
}

<<<<<<< HEAD
dssat.exec(xmin = 37, xmax = 38, ymin = 0, ymax = 1, res = 0.5,
           jobs = 4, ex.name = "test_simulation", path.to.extdata = "/media/TRANSFORM-EGB/eia2030/TRANSFORM2030_CropModels/crop_models/DSSAT/R/extdata/")
=======
# dssat.exec(xmin = 36, xmax = 38, ymin = -1, ymax = 1, res = 0.5,
#            jobs = 4, ex.name = "test_simulation", path.to.extdata = "/path/to/extdata/")
>>>>>>> 89601bb82a6910b66c43b9f0486f19e03d4e716d
