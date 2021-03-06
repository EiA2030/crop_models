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
  foreach::foreach(pnt=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "DSSAT")) %dopar% {
    # Set the experimental directory
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0")), sep = "/"))
    # Generate a DSSAT batch file using a tibble
    options(DSSAT.CSM="/DSSAT47/dssat-csm-os/build/bin/dscsm047")
    tibble(FILEX=paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"),'.MZX'), TRTNO=1:9, RP=1, SQ=0, OP=0, CO=0) %>%
      write_dssbatch()
    # Run DSSAT-CSM
    run_dssat(suppress_output = TRUE)
    # Change output file name
    file.rename(list.files(pattern = "Summary.*", full.names = TRUE), paste0(path.to.extdata, '/', ex.name, '/', 'EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '/', 'EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '.OUT'))
    setwd(path.to.extdata)
    gc()
  }
}
