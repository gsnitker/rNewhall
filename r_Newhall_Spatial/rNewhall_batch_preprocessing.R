# pre-processing rNewhall met data (prcp, min temp, max temp), MODIS data, and soil data for use with rNewhall
# this data will be uploaded to git hub and tehn downloaded during the pre-processing stage of rNewhall

rm(list = ls())
source("./dependent_scripts/rNewhall_v8_0_libs.R") # all required libraries
cores = detectCores() - 4 #recommended that 1-2 cores are reserved for other CPU functions


# load conus grid
conus.grid = shapefile("./rNewhall_data_preprocessing/conus_grid/rNewhall_data_processing_grid_conus_EPSG4269.shp")
# load study area
study.area = shapefile("./data/Rio_grande_watershed_sub_EPSG4269.shp") #Red_river_watershed_study_area_EPSG4269.shp")   #testing_extent_3_EPSG4269 payne_county_oklahoma_EPSG4269.shp") # if rNewhall.mode = "spatial", input must by a polygon; if rNewhall.mode = "point-based", input must by point(s) 


#determine which grid cells overlap with study area (so they can be processed)
process.chunks <-conus.grid[study.area,]

# load the data prep functions needed to preprocess each chunk
source("./dependent_scripts/rNewhall_v8_0_data_preprocessing_functions.R") # data prep functions
source("./dependent_scripts/rNewhall_v8_0_rNewhall_functions.R")

# set all needed user variables
#####################
# 2. Set user inputs
# 2.1 rNewhall Mode
rNewhall.mode = "spatial" # options: "spatial" or "point-based"
# 2.3 Date range to model (currently only offered in 1 year intervals)
date.years = c(2016)
# 2.4 Length of model spin-up (years)
spinup.length = 1 # in years
# 2.8 Set operating system
os = "Mac"  # Options: PC or Mac
#####################

#####################
# 3. Pre-process all data for RR watershed
# 3.1 Set data range for pre-processing
rNewhall.date.range(date.years = date.years)

# 3.2 Pre-process all PRISM data and save to drive for upload into GitHub
# ppt, tmin, and tmax
if (os == "Mac") {
  # mac version
  rNewhall.runs <- proc.time()
  mclapply(1:length(process.chunks), rNewhall.PRISM.Preprocess, buffer = 0, output.type = "raster", save.output = T, mc.cores = cores)
  print.time(rNewhall.runs, mode = "model runs")
}

if (os == "PC") {
  # PC version
  rNewhall.runs <- proc.time()
  cl = makeCluster(cores)
  clusterExport(cl, varlist = ls())
  invisible(clusterEvalQ(cl, source("./dependent_scripts/rNewhall_v8_0_libs.R"))) # all required libraries
  ptm <- proc.time() # start time
  invisible(parLapply(cl,1:length(process.chunks),rNewhall.PRISM.Preprocess, buffer = 0, data.type = "tmax", output.type = "dataframe", save.output = T))
  stopCluster(cl)
  print.time(rNewhall.runs, mode = "processing")
}

# 3.3 MODIS pre-processing
if (os == "Mac") {
  # mac version
  rNewhall.runs <- proc.time()
  mclapply(1:length(process.chunks), rNewhall.MODIS.Preprocess, reproject.to.PRISM = TRUE, save.output = TRUE, mc.cores = cores)
  print.time(rNewhall.runs, mode = "model runs")
}

if (os == "PC") {
  # PC version
  rNewhall.runs <- proc.time()
  cl = makeCluster(cores)
  clusterExport(cl, varlist = ls())
  invisible(clusterEvalQ(cl, source("./dependent_scripts/rNewhall_v8_0_libs.R"))) # all required libraries
  ptm <- proc.time() # start time
  invisible(parLapply(cl,1:length(process.chunks),rNewhall.MODIS.Preprocess,reproject.to.PRISM = TRUE, save.output = TRUE))
  stopCluster(cl)
  print.time(rNewhall.runs, mode = "processing")
}

# 3.$ Soils pre-processing
if (os == "Mac") {
  # mac version
  rNewhall.runs <- proc.time()
  #mclapply(1:length(process.chunks), rNewhall.Soils.Preprocess, save.output = TRUE, parallel = TRUE, cores = cores)
  mclapply(1:length(process.chunks), rNewhall.Soils.Preprocess, save.output = TRUE, mc.cores = cores)
  print.time(rNewhall.runs, mode = "model runs")
}

if (os == "PC") {
  # PC version
  rNewhall.runs <- proc.time()
  cl = makeCluster(cores)
  clusterExport(cl, varlist = ls())
  invisible(clusterEvalQ(cl, source("./dependent_scripts/rNewhall_v8_0_libs.R"))) # all required libraries

  #invisible(parLapply(cl,1:length(process.chunks), rNewhall.Soils.Preprocess, save.output = TRUE, save.output = TRUE))
  invisible(parLapply(cl,1:length(process.chunks), rNewhall.Soils.Preprocess, save.output = TRUE))
  stopCluster(cl)
  print.time(rNewhall.runs, mode = "processing")
  
  rNewhall.runs <- proc.time()
  lapply(1:length(process.chunks), rNewhall.Soils.Preprocess, save.output = TRUE)
  print.time(rNewhall.runs, mode = "processing")

  
  }