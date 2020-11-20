# rNewhall Final Development Version
# rNewhall v8 - Spatial and Point-based
# 08/12/2020
# Grant Snitker, University of Georgia
# grant.snitker@uga.edu

#####################
# 1. Clear workspace, load libraries, and set number of cores to use
rm(list = ls())
source("./dependent_scripts/rNewhall_v8_0_libs.R") # all required libraries
cores = detectCores() - 1 #recommended that 1-2 cores are reserved for other CPU functions
#####################


#####################
# 2. Set user inputs
# 2.1 rNewhall Mode
rNewhall.mode = "spatial" # options: "spatial" or "point-based" (output function still to be developed)
# 2.2 rNewhall study area (shapefile only) * Note: this is a small test area in the Red River Watersehd. Only teh Red River Wtaershed and the northern porton of teh Rio Grande have been pre processed.
study.area = shapefile("./data/Red_river_watershed_test_EPSG4269.shp") #if rNewhall.mode = "spatial", input must by a polygon; if rNewhall.mode = "point-based", input must by point(s) 
# 2.3 Date range to model (currently only offered in 1 year intervals)
date.years = c(2016) # only 2016 pre-processed as of now
# 2.4 Length of model spin-up (years)
spinup.length = 1 # in years
# 2.5 Method for calculating ET in rNewhall
ET.type = "Thornthwaite" # options: "Thornthwaite" "MODIS" "Measured"
# 2.6 rNewhall Moistre Matrix specifications
row.depth = 10 #soil depth represneted in ecah row (cm)
num.rows = 10 #number of rows in soil profile
num.col = 10 # number of columns in soil profile (not including saturation and ret column)
# 2.7 rNewhall output type 
output.type = "VWC" # Options: VWC, PAW
# 2.8 Set operating system
os = "PC"  # Options: PC or Mac
#####################


#####################
# 3. Load all required functions for data prep and rNewhall
source("./dependent_scripts/rNewhall_v8_0_data_prep_functions.R") # data prep functions
source("./dependent_scripts/rNewhall_v8_0_rNewhall_functions.R")
source("./dependent_scripts/rNewhall_v8_0_rNewhall_Daily_and_Viz_functions.R")
#####################


#####################
# 4. Prep all datasets for model runs and ceate data range for modeling
rNewhall.processing <- proc.time()
# 4.1 Create date range for rNewhall base on user inputs
rNewhall.date.range(date.years = date.years)
# 4.2 Prep data for rNewhall
# 4.2.1 Prep data for rNewhall
# load conus grid
conus.grid = shapefile("./rNewhall_data_preprocessing/conus_grid/rNewhall_data_processing_grid_conus_EPSG4269.shp")
#determine which grid cells overlap with study area (so they can be processed)
process.chunks <- conus.grid[study.area,]
process.chunks.list = process.chunks$id
# 4.3 Set all adjustable constants for fine-tuning rNewhall 
rNewhall.fine.tune.constants() # default values are indicated in "rNewhall_v8_0_rNewhall_functions.R"
# 4.5 Print total processing time
print.time(rNewhall.processing, mode = "processing")
#####################


#####################
# 5. Run rNewhall
if (os == "Mac") {
# # mac version
rNewhall.runs <- proc.time()
output = mclapply(1:length(process.chunks.list),Run.rNewhall.Daily.Batch, mc.cores = cores)
print.time(rNewhall.runs, mode = "model runs")
 }

if (os == "PC") {
# PC version
rNewhall.runs <- proc.time()
cl = makeCluster(cores)
clusterExport(cl, varlist = ls())
invisible(clusterEvalQ(cl, source("./dependent_scripts/rNewhall_v8_0_libs.R"))) # all required libraries
ptm <- proc.time() # start time
output = parLapply(cl,1:length(process.chunks.list),Run.rNewhall.Daily.Batch)
stopCluster(cl)
print.time(rNewhall.runs, mode = "model runs")
}

# optional save results
save(output, file = "./output/red_river_test.Rdata")
#####################


#####################
# 6. Process, visualize, and export results (*Note: only 10cm intervals can be visualized and exported right now)
rNewhall.vis(rNewhall.output = output, start.date = "2016-1-01", end.date = "2016-12-31", depth.top = 0, depth.bottom = 10, plot.raster = T, export.raster = F) 
#####################

