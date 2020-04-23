# # #download modis 
# # *** Note, only run if changing site loaction or modleing beyon 2000-2019
# # Otherwise load data from R data object
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
# save(MODIS, file = "./data/MODIS_ET_MARE_2000_2019.Rdata")



load("./data/MODIS_ET_MARE_2000_2019.Rdata")
