# Visulaizations for rNewhall outputs, specifically for submission of NRCS proposal
# ***NOTE: This using the front half of the rNewhall Model 
# 08/06/2019

# rNewhall water balance figure

# Clear workspace and load required libraries 
rm(list = ls())
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(dplyr)

# load and summarize data
meso.data  = read.csv("/Users/grantsnitker/Dropbox/Smoke/UGA_Postdoc/R_scripts/rNewhall/data/Marena_OK_mesonet_data.csv")
meso.data.sub = subset(meso.data, YEAR == '2009')
meso.data.monthly = meso.data.sub  %>% dplyr::group_by(MONTH) %>% 
  dplyr::summarise(
    precp = (sum(RAIN) *25.4),
    temp = (mean(TAVG) - 32) * 5/9)
meso.data.monthly

# Create dataset for the model
date.interval = "Jan 2009 - Dec 2009"
name = 'Marena Mesonet Site, OK'
country = 'USA'
latitude = 36.0643
longitude = -97.2127
nsHemisphere = 'N'
ewHemisphere = 'W'
#precipitation = c(5.33, 57.66, 110.74,110.74,74.68,49.53,143.26,159.77,92.20,159.77,10.16,15.49)
#temperature = c(1.89,-1.33,2.33,5.94,12.72,18.22,17.11,18.44,15.50,7.50,5.33,-4.67)
precipitation = round(meso.data.monthly$precp, 2)
temperature = round(meso.data.monthly$temp, 2)




# Load the remainder of the constants via the rNewhall_constants.R script
source("./dependent_scripts/rNewhall_constants_visualizations.R")

# Calculate PE based on Thornthwaite eq.
year.PE = c()
for (m in 1:12){
I.m = (temperature/5)^1.514# need to account for negative temperatures
I.m[is.nan(I.m)] <- 0
I = sum(I.m) #Note that this value is calculated for the whole year, but then used in each monthly calculation
a = (6.75*(10^-7)*(I^3)) - (7.71*(10^-5)*(I^2)) + (1.792*(10^-2)*I) + 0.49239
if (temperature[m] > 0 &  temperature[m] < 26.5){
  raw.PE = 16 * (10 * temperature[m] / I)^a
} else if (temperature[m] >= 38) { # If temp is over 38C, the PE value is fixed at 185.0mm
  raw.PE = 185.0
} else if (temperature[m] < 0) { # If temp is over 38C, the PE value is fixed at 185.0mm
  raw.PE = 0
} else {
  raw.PE =   pe.bins[findInterval(temperature[m], temp.bins)] #Use look up tables (temp.bins and pe.bins to assign a pe based on temperature)
}
# Asjust raw.PE using the K correction factor for daylight hours based on latitude
if (nsHemisphere == 'N'){
  k = knorth[findInterval(latitude,as.numeric(dimnames(knorth)[[1]])),m] # look up k value by latitude and month in appropriate lookup table (n vs S hemis)
} else {
  k = ksouth[findInterval(latitude,as.numeric(dimnames(ksouth)[[1]])),m]
}
PE = raw.PE * k
year.PE = append(year.PE, PE)
}


# Create climograph replication in ggplot2
# compile data
months.name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
months = c(1:12)
climograph.data = as.data.frame(cbind(precipitation, year.PE,months))
#create plot
p = ggplot(climograph.data, aes(x = months)) +
  geom_line(aes(y = precipitation, col = 'a')) +
  geom_line(aes(y = year.PE, col = 'b')) +
  geom_point(aes(y = precipitation, col = 'a')) +
  geom_point(aes(y = year.PE, col = 'b')) +
  geom_point(aes(y = 180), alpha = 0.0) + # dummy points to make scale allign with jNSM
  geom_area(aes(y = precipitation, fill = '#3732BB'), alpha = .5) +
  geom_area(aes(y = year.PE,fill = '#BA283B'), alpha = .5) +
  scale_y_continuous(name = "Precipitation/Potential Evapotranspiration (mm)\n", breaks = seq(0,200,by = 20), labels = c("0","20","40","60","80","100","120","140","160", "180", "200"),sec.axis = sec_axis(~., name = "Precipitation/Potential Evapotranspiration (mm)\n",breaks = seq(0,200,by = 20), labels = c("0","20","40","60","80","100","120","140","160", "180", "200")) ) +
  scale_x_continuous(name ="",breaks=c(1:12),labels = months.name)+
  scale_color_manual(name = "", values = c('#3732BB','#BA283B'),  labels = c("Precipitation", "Potential Evapotranspriation")) +
  scale_fill_manual(name = "  ", values = c('#3732BB','#BA283B'),  labels = c("Surplus", "Utilization (PET > Precipitation)")) +
  theme_bw() + theme(legend.position="bottom", legend.box = "vertical") + 
  ggtitle(paste("Station: ", name, sep=""), subtitle = paste(date.interval))

  
jpeg("./Output/Climograph_Marena_OK_2009.jpeg", width = 8, height = 6.5, units = 'in', res = 300)
p
dev.off()
  
  
  
