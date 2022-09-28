apsimx.exec <- function(xmin,xmax,ymin,ymax,res,jobs,ex.name,path.to.extdata){
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
  foreach::foreach(pnt=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "apsimx")) %dopar% {
    # Set the experimental directory
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0")), sep = "/"))
    # Set executable path
    apsimx::apsimx_options(exe.path = "/usr/local/lib/apsim/bin/Release/netcoreapp3.1/Models")
    # Execute APSIMX
    apsimx::apsimx(paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '.apsimx'))
    setwd(path.to.extdata)
    gc()
  }
}
