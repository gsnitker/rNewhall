# # # # #download modis
# # # # *** Note, only run if changing site loaction or modleing beyon 2000-2019
# # # # # Otherwise load data from R data object
# MODIS <- mt_subset(product = "MOD16A2",
#                       lat = lat,
#                       lon = long,
#                       band = c("ET_500m", "PET_500m"),
#                       start = "2000-01-01",
#                       end = "2019-12-31",
#                       progress = T)
# 
# MODIS$value[MODIS$value == 32765 | MODIS$value == 32764  ] = NA
# MODIS$value[MODIS$value == 32767 | MODIS$value == 32766  | MODIS$value == 32762 | MODIS$value == 32761] = NA #Might need to mak this into NA
# save(MODIS, file = paste("./data/MODIS_ET_", site,"_2000_2019.Rdata", sep = ""))
# 
# MODIS.LAI <- mt_subset(product = "MCD15A2H",
#                    lat = lat,
#                    lon = long,
#                    band = c("Lai_500m"),
#                    start = "2000-01-01",
#                    end = "2019-12-31",
#                    progress = T)
# 
# MODIS$value[MODIS$value == 32765 | MODIS$value == 32764  ] = NA
# MODIS$value[MODIS$value == 32767 | MODIS$value == 32766  | MODIS$value == 32762 | MODIS$value == 32761] = NA #Might need to mak this into NA
# save(MODIS.LAI, file = paste("./data/MODIS_LAI_", site,"_2000_2019.Rdata", sep = ""))

# create compiled measured ET values for MARE (testing)
MARE.Measured.ET = read.csv("./data/MOISST_Daily_ET_2013_2017.csv")
MARE.Measured.ET$ET.filled = na_interpolation(MARE.Measured.ET$Mean.Daily.ET..mm., option = "stine", maxgap = Inf)
MARE.Measured.ET$DATE = as.Date(MARE.Measured.ET$Date)

load(paste("./data/MODIS_ET_", site,"_2000_2019.Rdata", sep = ""))
load(file = paste("./data/MODIS_LAI_", site,"_2000_2019.Rdata", sep = ""))


# MODIS.LAI <- mt_subset(product = "MCD15A2H",
#                        lat = 34.36,
#                        lon = -106.69,
#                        band = c("Lai_500m"),
#                        start = "2000-01-01",
#                        end = "2019-12-31",
#                        progress = T)
# 
# MODIS$value[MODIS$value == 32765 | MODIS$value == 32764  ] = NA
# MODIS$value[MODIS$value == 32767 | MODIS$value == 32766  | MODIS$value == 32762 | MODIS$value == 32761] = NA #Might need to mak this into NA
# save(MODIS.LAI, file = paste("./data/MODIS_LAI_", "SEVILLETA","_2000_2019.Rdata", sep = ""))
# 
# MODIS.LAI <- mt_subset(product = "MCD15A2H",
#                        lat = 35.17,
#                        lon = -102.1,
#                        band = c("Lai_500m"),
#                        start = "2000-01-01",
#                        end = "2019-12-31",
#                        progress = T)
# 
# MODIS$value[MODIS$value == 32765 | MODIS$value == 32764  ] = NA
# MODIS$value[MODIS$value == 32767 | MODIS$value == 32766  | MODIS$value == 32762 | MODIS$value == 32761] = NA #Might need to mak this into NA
# save(MODIS.LAI, file = paste("./data/MODIS_LAI_", "BUSHLAND","_2000_2019.Rdata", sep = ""))
# 
# MODIS.LAI <- mt_subset(product = "MCD15A2H",
#                        lat = 32.56,
#                        lon = -106.7,
#                        band = c("Lai_500m"),
#                        start = "2000-01-01",
#                        end = "2019-12-31",
#                        progress = T)
# 
# MODIS$value[MODIS$value == 32765 | MODIS$value == 32764  ] = NA
# MODIS$value[MODIS$value == 32767 | MODIS$value == 32766  | MODIS$value == 32762 | MODIS$value == 32761] = NA #Might need to mak this into NA
# save(MODIS.LAI, file = paste("./data/MODIS_LAI_", "JORNADA","_2000_2019.Rdata", sep = ""))
# 
# MODIS.LAI <- mt_subset(product = "MCD15A2H",
#                        lat = 33.45,
#                        lon = -99.87,
#                        band = c("Lai_500m"),
#                        start = "2000-01-01",
#                        end = "2019-12-31",
#                        progress = T)
# 
# MODIS$value[MODIS$value == 32765 | MODIS$value == 32764  ] = NA
# MODIS$value[MODIS$value == 32767 | MODIS$value == 32766  | MODIS$value == 32762 | MODIS$value == 32761] = NA #Might need to mak this into NA
# save(MODIS.LAI, file = paste("./data/MODIS_LAI_", "KNOX","_2000_2019.Rdata", sep = ""))