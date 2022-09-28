apsimx.expfile <- function(xmin,xmax,ymin,ymax,res,sdate=NULL,edate=NULL,jobs,ex.name,path.to.extdata){
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
  # Format dates
  sdate <- format(as.Date(sdate, "%Y-%m-%d"), "%Y-%m-%dT%H:%M:%S")
  edate <- format(as.Date(edate, "%Y-%m-%d"), "%Y-%m-%dT%H:%M:%S")
  # Function to add months
  add.months <- function(date,n) seq(as.Date(date), by = paste (n, "months"), length = 2)[2]
  # Process Experimental Files
  foreach::foreach(pnt=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "apsimx")) %dopar% {
    # Set the experimental directory
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0")), sep = "/"))
    # #Read sample cultivar file and filter to only cultivar IF0014
    # cul <- read_cul("MZCER047.CUL") %>%
    #   filter(`VAR#` == "IB0063")
    # DSSAT::write_cul(cul = cul, )
    # #Read sample cultivar file and filter to only ecotype IB0002
    # eco <- read_eco("MZCER047.ECO") %>%
    #   filter(`ECO#` == "IB0001")
    # Set start and end of simulation
    apsimx::edit_apsimx(paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"),'.apsimx'),
                        node = "Clock",
                        parm = c("Start", "End"),
                        value = c(sdate, edate),
                        overwrite = TRUE)
    # Define planting date
    apsimx::edit_apsimx(paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '.apsimx'), node = "Crop",
                        parm = "StartDate", value = tolower(sub("^0+", "", format(as.Date(sdate, "%Y-%m-%d"), "%d-%b"))))
    # Define end date
    apsimx::edit_apsimx(paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"), '.apsimx'), node = "Crop",
                        parm = "EndDate", value = tolower(sub("^0+", "", format(as.Date(add.months(sdate,6), "%Y-%m-%d"), "%d-%b"))))
    ####################################################################################################################
# 
# 
#     apsimx::edit_apsimx(paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"),'.apsimx'),
#                         node = "Clock",
#                         parm = c("Start", "End"),
#                         value = c(sdate, edate),
#                         overwrite = TRUE)
# 
# 
# 
# 
#     pdates <- data.frame(seq(1,length(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week")), by = 1),
#                          format(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week"), "%y%j"),
#                          -99,5.3,5.3,"S","R",75,-99,3,-99,-99,-99,-99,-99,
#                          paste0("PD", seq(1:length(seq(as.Date(sdate, "%Y-%m-%d") + 1, as.Date(edate, "%Y-%m-%d") - 1, by = "1 week")))))
#     colnames(pdates) <- colnames(file_x$`PLANTING DETAILS`)
#     file_x$`PLANTING DETAILS` <- pdates
#     # # Define fertilizer amounts
#     # file_x$`FERTILIZERS (INORGANIC)` <- as_DSSAT_tbl(tibble::tibble("F" = rep(1:2, each = (60/20)+1),
#     #                                                                 "FDATE" = rep(seq(as.Date(sdate, "%Y-%m-%d"), as.Date(sdate, "%Y-%m-%d") + 60, by = "20 days"), 2),
#     #                                                                 "FMCD" = rep(-99, length(rep(1:2, each = (60/20)+1))),
#     #                                                                 "FACD" = rep(-99, length(rep(1:2, each = (60/20)+1))),
#     #                                                                 "FDEP" = rep(-99, length(rep(1:2, each = (60/20)+1))),
#     #                                                                 "FAMN" = rep(-99, length(rep(1:2, each = (60/20)+1))),
#     #                                                                 "FAMP" = rep(-99, length(rep(1:2, each = (60/20)+1))),
#     #                                                                 "FAMK" = rep(-99, length(rep(1:2, each = (60/20)+1))),
#     #                                                                 "FAMC" = rep(-99, length(rep(1:2, each = (60/20)+1))),
#     #                                                                 "FAMO" = rep(-99, length(rep(1:2, each = (60/20)+1))),
#     #                                                                 "FOCD" = rep(-99, length(rep(1:2, each = (60/20)+1))),
#     #                                                                 "FERNAME" = rep(-99, length(rep(1:2, each = (60/20)+1)))))
#     # Change inputs to FileX
#     file_x$FIELDS$WSTA<-paste0("WHTE", formatC(width = 4, as.integer((pnt-1)), flag = "0"))
#     file_x$FIELDS$ID_SOIL<-paste0('TRAN', formatC(width = 6, as.integer((pnt-1)), flag = "0"))
#     #Overwrite original FileX with new values
#     DSSAT::write_filex(file_x,paste0('EXTE', formatC(width = 4, as.integer((pnt-1)), flag = "0"),'.MZX'))
    setwd(path.to.extdata)
    gc()
  }
}

# dssat.expfile(xmin = 30.749060, xmax = 31.311379, ymin = 30.579158, ymax = 31.159431, res = 0.05,
#               jobs = 4, ex.name = "EGY1", path.to.extdata = "/media/TRANSFORM-EGB/eia2030/TRANSFORM2030_CropModels/crop_models/APSIM/R/tests/extdata/")
# dssat.exec(xmin = 30.749060, xmax = 31.311379, ymin = 30.579158, ymax = 31.159431, res = 0.05,
#            jobs = 4, ex.name = "EGY1", path.to.extdata = "/media/TRANSFORM-EGB/eia2030/TRANSFORM2030_CropModels/crop_models/APSIM/R/tests/extdata/")
