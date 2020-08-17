# Improving/optimizing rNewhall-point for testing across the OK mesonet 
# rNewhall v7_2 - Point Based
# 7/12/2020

# Preamble ### ------------------------------------------------------------------------
# Clear workspace and load required libraries 
rm(list = ls())
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(demogR)
library(geosphere)
library(scales)
library(lubridate)
library(raster)
library(rgdal)
library(Evapotranspiration)
library(SPEI)
library(lubridate)
library(geosphere)
library(rgdal)
library(demogR)
library(reshape2)
library(sigmoid)
library(scales)
library(sp)
library(zoo)
library(dplyr)
library(doParallel)
library(parallel)
library(pbapply)
library(MODISTools)
library(MODIS)
library(leaflet)
library(rpart)
library(soilwater)
library(Metrics)
library(cowplot)
library(hydroGOF)
library(soiltexture)
library(tidyr)
library(imputeTS)
library(aqp)
library(soilDB)
library(latticeExtra)
library(plyr)
library(reshape2)
library(grid)
# Set number of cores to use in parallel functions
cores = detectCores() - 2


# Part 1: Set up all required inputs ### ------------------------------------------------------------------------
# 1.1 Import modeling area extent (shapefile) NOTE: must be same CRS as raster inputs
all.sites = shapefile("./data/mesonet_sites_shape/ok_mesonet_sites_active_20181031.shp")# PRISM CONUS is too big to create a raster stack at this phase, so raster is clipped to study area; Load study area shapefile here
met.data.all <<- read.csv("./data/mesonet_met_sm_data_all_stations_2000_2020.csv")

# 1.1.1 Set all adjustable variables (for sensitivity testing)
source("./dependent_scripts/rNewhall_v7_2_pt_adjustable_variables.R")


#1.1.2 Choose site(s) of interest 
site = "MARE" # good exmaples: BEAV 2009; CAMA 2009;  MARE 2009; OKMU 2009; WIST 2009 "OKEM", "BIXB", "BRIS"

#1.1.3 Subset sites
model.sites = subset(all.sites, stid==site) # use all.sites$stid to see all possible site names

# 1.2 Choose rNewhall output type
output.type = "VWC" # Options: VWC, Se, PAW, FAW

# 1.3 Set rNewhall moisture matrix properties (based on soil properties of testing location)
# The new version of rNewhall utilizes layers of the soil profile subdivided into layers/rows
row.depth = 10 #cm
num.rows = 10 #number of rows 
num.col = 10 # number of columns (not including saturation and ret column)

# 1.4 Set date range for model (in years only)
date.years = c(2016)
spinup.length = 1 # in years

# 1.5 Load and set-up the required soil and precip data for model run
source("./dependent_scripts/rNewhall_v7_2_pt_soil_precip_data.R")

# 1.5.1 Show site map
#site.map

# 1.6 MM setup
source("./dependent_scripts/rNewhall_v7_2_pt_mm_setup.R")

# 1.7 Load the Thornthwaite equation and the  remainder of the constants via the rNewhall_thornthwaite_constants.R script
source("./dependent_scripts/rNewhall_v7_2_pt_ET.R")

# 1.8 Load rNewhall Daily Function
source("./dependent_scripts/rNewhall_v7_2_pt_functions.R")

# 1.9 Generate MODIS-based Kc to adjust ET throughout the year
source("./dependent_scripts/rNewhall_v7_2_pt_MODIS_kc.R")

# 1.10 Set and tehn observe the initial Mositure Matrix conditions 
# observe starting maxtrix
# set mm spinup conditions
mm = rep(0, length(mm.max))
#mm = mm.max
matrix(mm, nrow = num.rows, ncol = (num.col + 2), byrow = T)
mm.matrix.max


#Part 2: Run the pt-based rNewhall model -----------------------------------------------------------
# 2.1 Spin-up
source("./dependent_scripts/rNewhall_v7_2_pt_spinup_setup.R")

spinup.out =  lapply(spinup.range, rNewhall.pt.daily)


# 2.2 Run Daily rNewhall for Mesonet site
source("./dependent_scripts/rNewhall_v7_2_pt_run_setup.R")

pt.out = lapply(date.range, rNewhall.pt.daily)

# Part 3: Process Results ### ---------------------------------------------------------
# 3.1 Query the output s4 object to build a dataframe from the results
pt.out.df = data.frame()
for (i in (1:length(pt.out))){
  df = c(as.Date(pt.out[[i]]@day),
         pt.out[[i]]@prcp,
         pt.out[[i]]@ET,
         pt.out[[i]]@moisture.0.10,
         pt.out[[i]]@moisture.10.20,
         pt.out[[i]]@moisture.20.30,
         pt.out[[i]]@moisture.30.40,
         pt.out[[i]]@moisture.40.50,
         pt.out[[i]]@moisture.50.60,
         pt.out[[i]]@moisture.60.70,
         pt.out[[i]]@moisture.70.80,
         pt.out[[i]]@moisture.80.90,
         pt.out[[i]]@moisture.90.100)
  pt.out.df = rbind(pt.out.df, df)
} 

# 3.2 Rename result columns
colnames(pt.out.df) = c("Date","PRCP", "ET","SM_0_10cm","SM_10_20cm","SM_20_30cm",
                     "SM_30_40cm","SM_40_50cm","SM_50_60cm",
                     "SM_60_70cm", "SM_70_80cm", "SM_80_90cm",
                     "SM_90_100cm")
pt.out.df$Date = as.Date(pt.out.df$Date)
pt.out.df$kc = as.vector(kc[1:length(pt.out.df$Date)])


# Part 4: Plot Results and Measured values ### ---------------------------------------------------------

# 4.1  load and subset measured SM values 
source("./dependent_scripts/rNewhall_v7_2_pt_measured_data.R")

# 4.2 Plot all measured SM data for station
measured.moisture.plot = ggplot(measured.SM, aes(x = Date)) +
  geom_line(aes(y = (SM05), color = "Measured SM 05cm")) +
  geom_line(aes(y = (SM25), color = "Measured SM 25cm")) +
  geom_line(aes(y = (SM60), color = "Measured SM 60cm")) +
  geom_line(aes(y = (SM75), color = "Measured SM 75cm")) +
  ylim(c(0,.5)) + 
  theme_bw() + labs(title = paste(site,": Measured Soil Moisture", sep = ""),
                    subtitle = "rNewhall Pt v.6 ", x = "Date", y = "VWC\n", color = "Depth") 
measured.moisture.plot

# 4.2 all modeled SM data for station
modeled.moisture.plot = ggplot(pt.out.df, aes(x = Date)) +
  geom_line(aes(y = (SM_0_10cm), color = "Modeled SM 0-10cm")) +
  geom_line(aes(y = (SM_20_30cm), color = "Modeled SM 20-30cm")) +
  geom_line(aes(y = (SM_60_70cm), color = "Modeled SM 60-70cm")) +
  geom_line(aes(y = (SM_70_80cm), color = "Modeled SM 70-80cm")) +
 ylim(c(0,.5)) + 
  theme_bw() + labs(title = paste(site,": Modeled Soil Moisture", sep = ""),
                    subtitle = "rNewhall Pt v.6 ", x = "Date", y = "VWC\n", color = "Depth") 
modeled.moisture.plot

# 4.3 PRCP, ET, Kc, etc used in model run
modeled.prcp.et.plot = ggplot(pt.out.df, aes(x = Date)) +
  #geom_line(aes(y = (PRCP), color = "PRCP")) +
  geom_line(aes(y = (ET), color = "Modeled ET")) +
  geom_line(aes(y = (PRCP - ET), color = "NMA")) +
  #geom_line(aes(y = kc, color = "kc")) +
  geom_line(data = MODIS.PET.filled, aes(x = DATE, y = PET/10, color = "MODIS PET")) +
  geom_line(data = MODIS.ET.filled, aes(x = DATE, y = ET/10, color = "MODIS ET")) +
  #geom_line(data = summ.MARE.ET, aes(x = Date, y = ET/10, color = "Filled Measured ET")) +
  geom_line(data = MARE.Measured.ET, aes(x = as.Date(Date), y = ET.filled/10, color = "Measured ET" ))+
  theme_bw() + labs(title = paste(site,": Meteorological and ET inputs ", sep = ""),
                    subtitle = "rNewhall Pt v.7.2 ", x = "Date", y = "(cm/day)\n", color = "Variable") + 
  scale_x_date(limits = as.Date(c(date.range[1],date.range[length(date.range)])))
modeled.prcp.et.plot

# 4.4 Comparison of modeled vs measured SM from 0-10cm 
moisture.plot.0.10 = ggplot(pt.out.df, aes(x = Date)) +
 # geom_col(aes(y = (PRCP)), color = "Blue", alpha = 0.5) +
  #geom_line(aes(y = (PRCP - ET)), color = "red") +
  geom_line(aes(y = WC.15[1]/10), color = "orange", linetype =2) +
  geom_line(aes(y = WC.033[1]/10), color = "blue", linetype =2) +
  annotate("text", x =date.range[30] , y = (WC.15[1]/10)-.03 , label = "PWP", color = "orange") +
  annotate("text", x =date.range[30] , y = (WC.033[1]/10)+.07 , label = "FC", color = "blue") +
  geom_line(aes(y = (SM_0_10cm), color = "Modeled SM 0-10cm")) +
  #geom_line(aes(y = (kc), color = "kc")) +
  geom_line(data = measured.SM, aes(x = Date, y = (SM05), color = "Measured SM 05cm")) +
  scale_color_manual(values = c("grey55", "#CA7676")) +
  ylim(c(0,.5)) + 
  theme_bw() + labs(title = paste(site,": Modeled and Measured Soil Moisture", sep = ""),
                    subtitle = "Depth: 0-10 cm", x = "Date", y = "VWC\n", color = "Depth") 
moisture.plot.0.10


# 4.4 Comparison of modeled vs measured SM from 20-30cm 
moisture.plot.20.30 = ggplot(pt.out.df, aes(x = Date)) +
  geom_line(aes(y = WC.15[3]/10), color = "orange", linetype =2) +
  geom_line(aes(y = WC.033[3]/10), color = "blue", linetype =2) +
  annotate("text", x =date.range[30] , y = (WC.15[3]/10)-.03 , label = "PWP", color = "orange") +
  annotate("text", x =date.range[30] , y = (WC.033[3]/10)+.03 , label = "FC", color = "blue") +
  geom_line(aes(y = (SM_20_30cm), color = "Modeled SM 20-30cm")) +
  geom_line(data = measured.SM, aes(x = Date, y = (SM25), color = "Measured SM 25cm")) +
  scale_color_manual(values = c("grey55", "#469932")) +
  ylim(c(0,.5)) + 
  theme_bw() + labs(title = paste(site,": Modeled and Measured Soil Moisture", sep = ""),
                         subtitle = "Depth: 20-30 cm", x = "Date", y = "VWC\n", color = "Depth") 
moisture.plot.20.30

