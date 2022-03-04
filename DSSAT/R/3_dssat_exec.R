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
  foreach::foreach(pnt=0:nrow(grid), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
    # Set the experimental directory
    setwd(paste(getwd(),ex.name,paste0('TRAN', formatC(width = 4, pnt, flag = "0")), sep = "/"))
    # Generate a DSSAT batch file using a tibble
<<<<<<< HEAD
    options(DSSAT.CSM="/DSSAT47/dssat-csm-os/build/bin/dscsm047")
    tibble(FILEX=paste0('EXTE', formatC(width = 4, pnt, flag = "0"),'.MZX'), TRTNO=1:9, RP=1, SQ=0, OP=0, CO=0) %>%
=======
    options(DSSAT.CSM="/media/TRANSFORM-EGB/eia2030/TRANSFORM2030_CropModels/docker/DSSAT/dssat-csm-os/build/bin/dscsm047")
    tibble(FILEX=paste0('TRAN', formatC(width = 4, pnt, flag = "0"),'.MZX'), TRTNO=1:9, RP=1, SQ=0, OP=0, CO=0) %>%
>>>>>>> d12117b24ea37d1082284defd0976a7639e8ee98
      write_dssbatch()
    # Run DSSAT-CSM
    run_dssat()
    # Change output file name
<<<<<<< HEAD
    # file.rename(list.files(pattern = "Summary.*"), paste0(paste0('EXTE', formatC(width = 4, pnt, flag = "0"), '.OUT')))
    setwd(path.to.extdata)
=======
    file.rename(list.files(pattern = "Summary.*"), paste0(paste0('TRAN', formatC(width = 4, pnt, flag = "0"), '.OUT')))
    setwd(here::here())
>>>>>>> d12117b24ea37d1082284defd0976a7639e8ee98
  }
}

# dssat.exec(xmin = 36.66, xmax = 37.43, ymin = -1.35, ymax = -0.5, res = 0.5,
#            jobs = 8, ex.name = "test_simulation", path.to.extdata = "/path/to/extdata/")
