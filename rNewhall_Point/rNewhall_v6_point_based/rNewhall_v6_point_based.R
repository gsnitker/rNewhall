# Improving/optimizing rNewhall-point for testing across the OK mesonet 
# rNewhall v6 - Point Based
# 3/12/2020

# needs:

# Need to figure out why teh top layers are behaving strangely
# add the "saturation column" back in entirely, or get rid of it entirely
# make sure all of teh units, particularly ET, square up
# perhaps do K values for each layer instead of the entire mm




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
library(soiltexture)
cores = detectCores() - 2

# Part 1: Set up all required inputs ### ------------------------------------------------------------------------
# 1.1 Import modeling area extent (shapefile) NOTE: must be same CRS as raster inputs
all.sites = shapefile("./data/mesonet_sites_shape/ok_mesonet_sites_active_20181031.shp")# PRISM CONUS is too big to create a raster stack at this phase, so raster is clipped to study area; Load study area shapefile here

#1.1.1 Choose site(s) of interest 
site = "MARE"

#1.1.2 Subset sites
model.sites = subset(all.sites, stid==site) # use all.sites$stid to see all possible site names

# 1.2 Choose the number of days to be aggregated for rNewhall (ex. 1  = Daily, 7 = weekly, 8 = MODIS)
agg.interval = 1

# 1.3 Set rNewhall moisture matrix properties (based on soil properties of testing location)
# The new version of rNewhall utilizes layers of the soil profile subdivided into layers/rows
row.depth = 10 #cm
num.rows = 10 #number of rows 
num.col = 10 # number of columns (not including satuartion column)

interception.rate = .2

# 1.4 Set date range for model (in years only)
date.years = c(2008)
spinup.length = 1 # in years
  
# 1.5 Load and set-up the required soil and precip data for model run
source("./dependent_scripts/rNewhall_v6_pt_soil_precip_data.R")

# 1.5.1 Show site map
#site.map

# 1.5.2 Load measure SM data if comparing\


# 1.6 Data aggregation
source("./dependent_scripts/rNewhall_v6_pt_data_aggregation.R")

# 1.7 MM setup
source("./dependent_scripts/rNewhall_v6_pt_mm_setup.R")

# 1.8 Load the Thornthwaite equation and the  remainder of the constants via the rNewhall_thornthwaite_constants.R script
source("./dependent_scripts/rNewhall_v6_pt_thorthwaite.R")

# 1.9 Load rNewhall Daily Function
source("./dependent_scripts/rNewhall_v6_pt_functions.R")

# 1.10 Generate MODIS-based Kc to adjust ET trhoughout the year
source("./dependent_scripts/rNewhall_v6_pt_MODIS_kc.R")

# observe starting maxtrix
matrix(mm, nrow = num.rows, ncol = 11, byrow = T)

#Part 2: Run the pt-based rNewhall model -----------------------------------------------------------
# 2.1 Spin-up
source("./dependent_scripts/rNewhall_v6_pt_spinup_setup.R")

spinup.out =  lapply(spinup.range, rNewhall.pt.daily)

# initial conditions
spinup.out[[length(spinup.out)]]@vol.moisture.0.10
spinup.out[[length(spinup.out)]]@vol.moisture.20.30
spinup.out[[length(spinup.out)]]@vol.moisture.50.60
spinup.out[[length(spinup.out)]]@vol.moisture.70.80

# 2.2 Run Daily rNewhall for Mesonet site
source("./dependent_scripts/rNewhall_v6_pt_run_setup.R")

pt.out = lapply(date.range, rNewhall.pt.daily)

# Part 3: Process Results ### ---------------------------------------------------------
# 3.1 Query the output s4 object to build a dataframe from the results
pt.out.df = data.frame()
for (i in (1:length(pt.out))){
  df = c(as.Date(pt.out[[i]]@day),
         pt.out[[i]]@prcp,
         pt.out[[i]]@ET,
         pt.out[[i]]@vol.moisture.0.10,
         pt.out[[i]]@vol.moisture.10.20,
         pt.out[[i]]@vol.moisture.20.30,
         pt.out[[i]]@vol.moisture.30.40,
         pt.out[[i]]@vol.moisture.40.50,
         pt.out[[i]]@vol.moisture.50.60,
         pt.out[[i]]@vol.moisture.60.70,
         pt.out[[i]]@vol.moisture.70.80,
         pt.out[[i]]@vol.moisture.80.90,
         pt.out[[i]]@vol.moisture.90.100)
  pt.out.df = rbind(pt.out.df, df)
} 

# 3.2 Rename result columns
colnames(pt.out.df) = c("Date","PRCP", "ET","SM_0_10cm","SM_10_20cm","SM_20_30cm",
                     "SM_30_40cm","SM_40_50cm","SM_50_60cm",
                     "SM_60_70cm", "SM_70_80cm", "SM_80_90cm",
                     "SM_90_100cm")
pt.out.df$Date = as.Date(pt.out.df$Date)

# Part 4: Plot Results and Measured values ### ---------------------------------------------------------
# load and subset measured SM values 
# *Note: these measured values were calculated using "./Dropbox/Smoke/UGA_Postdoc/R_scripts/Soil_Moisture_OK_Mesonet/Convert_DeltaT_to Vol_SM.R" and associated project

load("/Users/grantsnitker/Dropbox/Smoke/UGA_Postdoc/R_scripts/Soil_Moisture_OK_Mesonet/data/Marena_Mesonet_Vol_SM_2000_2018.Rdata")
#measured.SM = subset(measured.SM , year(measured.SM$Date) >= 2008 & year(measured.SM$Date) <= 2009)
measured.SM = subset(measured.SM , year(measured.SM$Date) == year(date.range[1]))



# all measured
measured.moisture.plot = ggplot(measured.SM, aes(x = Date)) +
  geom_line(aes(y = (SM05), color = "Measured SM 05cm")) +
  geom_line(aes(y = (SM25), color = "Measured SM 25cm")) +
  geom_line(aes(y = (SM60), color = "Measured SM 60cm")) +
  geom_line(aes(y = (SM75), color = "Measured SM 75cm")) +
  ylim(c(0,.5)) + 
  theme_bw() + labs(title = "Measured Soil Moisture at Marena, OK Mesonet Station",
                    subtitle = "rNewhall Pt v.6 ", x = "Date", y = "Soil Moisture (%)\n", color = "Depth") 
measured.moisture.plot

# all modeled
modeled.moisture.plot = ggplot(pt.out.df, aes(x = Date)) +
  geom_line(aes(y = (SM_0_10cm), color = "Modeled SM 0-10cm")) +
  geom_line(aes(y = (SM_20_30cm), color = "Modeled SM 20-30cm")) +
  geom_line(aes(y = (SM_60_70cm), color = "Modeled SM 60-70cm")) +
  geom_line(aes(y = (SM_70_80cm), color = "Modeled SM 70-80cm")) +
 ylim(c(0,.5)) + 
  theme_bw() + labs(title = "Modeled Soil Moisture at Marena, OK Mesonet Station - Interpolated MM",
                    subtitle = "rNewhall Pt v.6 ", x = "Date", y = "Soil Moisture (%)\n", color = "Depth") 
modeled.moisture.plot
# PRCP and ET
modeled.prcp.et.plot = ggplot(pt.out.df, aes(x = Date)) +
  geom_line(aes(y = (PRCP), color = "PRCP")) +
  geom_line(aes(y = (ET), color = "Modeled ET")) +
  theme_bw() + labs(title = "Modeled PRCP and ET at Marena, OK Mesonet Station",
                    subtitle = "rNewhall Pt v.6 ", x = "Date", y = "cm\n", color = "Variable") 
modeled.prcp.et.plot





