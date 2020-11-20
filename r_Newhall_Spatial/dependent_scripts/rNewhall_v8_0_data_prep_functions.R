# Download and process all data needed for rNewhall

print.time = function(t, mode){
time = proc.time() - t
mins = round(time[3] / 60, digits = 2)
cat(paste("\nAll rNewhall ", mode,  " complete. \nTotal Time: ",mins, " mins\n", sep = ""))}

#create date ranges 
#*****NOTE***** Temp set up to run for only 3 months during testing
rNewhall.date.range = function(date.years){
  date.range <<- seq(as.Date(paste(date.years, "-1-1", sep = "")), as.Date(paste(date.years, "-12-31", sep = "")), "days" ) # create date range for model
spinup.range <<- seq(as.Date(paste(date.years-spinup.length, "-1-1", sep = "")), as.Date(paste(date.years-spinup.length, "-12-31", sep = "")), "days" ) # create date range for model
}
###################################################
#PRISM data prep

rNewhall.Data.Prep = function(chunk.id, download.data = F){ # need to feed it the ID
  # create rNewhall.Processed.Chunks object
  setClass("gg")
  setClass("rNewhall.Processed.Chunks", slots = representation(date.years = "numeric",
                                                           chunk.id = "numeric",
                                                           ppt = "data.frame",
                                                           tmin = "data.frame",
                                                           tmax = "data.frame",
                                                           MODIS.ET = "data.frame",
                                                           MODIS.PET = "data.frame",
                                                           MODIS.LAI = "data.frame",
                                                           soils = "data.frame"))
  
    load(paste("./rNewhall_data_preprocessing/prcp/PRISM_ppt", date.years, chunk.id, "dataframe.RData", sep = "_"))
    load(paste("./rNewhall_data_preprocessing/tmin/PRISM_tmin", date.years, chunk.id, "dataframe.RData", sep = "_"))
    load(paste("./rNewhall_data_preprocessing/tmax/PRISM_tmax", date.years, chunk.id, "dataframe.RData", sep = "_"))
    load(paste("./rNewhall_data_preprocessing/MODIS/MODIS_ET", date.years, chunk.id, "dataframe.RData", sep = "_"))
    load(paste("./rNewhall_data_preprocessing/MODIS/MODIS_PET", date.years, chunk.id, "dataframe.RData", sep = "_"))
    load(paste("./rNewhall_data_preprocessing/MODIS/MODIS_LAI", date.years, chunk.id, "dataframe.RData", sep = "_"))
    load(paste("./rNewhall_data_preprocessing/soils/soil", date.years, chunk.id, "dataframe.RData", sep = "_"))
    
     return(new("rNewhall.Processed.Chunks", 
        date.years = date.years,
        chunk.id = chunk.id,
        ppt = PRISM.ppt,
        tmin = PRISM.tmin,
        tmax = PRISM.tmin,
        MODIS.ET = MODIS.ET,
        MODIS.PET = MODIS.PET,
        MODIS.LAI = MODIS.LAI,
        soils = study.area.soils))
  } 
  
#####################
# rosetta.approx - need this for later data prep, so might as well load it now so that it exists when it is needed
rosetta.approx = function(sand, silt, clay) {
  rosetta.matrix = matrix(c(
    0.098, 0.459, 0.0150, 1.25, 14.76,  2.952, -1.531, 0.12,
    0.079, 0.442, 0.0158, 1.42, 8.184,  4.992, -0.763, 0.20,
    0.061, 0.399, 0.0111, 1.47, 12.048,  3.696, -0.371, 0.15,
    0.049, 0.390, 0.0348, 1.75, 105.192,  24.312, -0.874, 0.99,
    0.053, 0.375, 0.0352, 3.18, 642.696, 24.480,  -0.930, 1.00,
    0.117, 0.385, 0.0334, 1.21, 11.352,  4.344, -3.665, 0.18,
    0.063, 0.384, 0.0211, 1.33, 13.176,  6.936, -1.280, 0.28,
    0.039, 0.387, 0.0267, 1.45, 38.280,  15.480, -0.861, 0.63,
    0.05,  0.489, 0.0066, 1.68, 43.752,  3.336,  0.624, 0.14,
    0.111, 0.481, 0.0162, 1.32, 9.624,  3.168, -1.287, 0.13,
    0.090, 0.482, 0.0084, 1.52, 11.112,  2.232, -0.156, 0.09,
    0.065, 0.439, 0.0051, 1.66, 18.240,  1.752,  0.365, 0.07), nrow = 12, ncol = 8, byrow=TRUE, dimnames =
      list(c('Cl','ClLo','Lo','LoSa','Sa', 'SaCl', 'SaClLo', 'SaLo','Si','SiCl', 'SiClLo', 'SiLo'),
           c('Theta_res [cm3/cm3]', 'theta_sat [cm3/cm3]', 'alpha [1/cm]', 'n', 'Ksat [cm/day]', 'Ko [cm/day]', 'L', 'Rel. Percolation')))
  class = TT.points.in.classes(data.frame("CLAY" = c(clay), "SILT" = c(silt), "SAND" = c(sand)), "USDA.TT")
  final.class = names(class[, colSums(class != 0) > 0])
  rosetta.attr = as.data.frame(rbind(rosetta.matrix[final.class,]))
  return(rosetta.attr)}
#####################
#####################

#####################
# set defualt values for adjustable varibales in rNewhall
rNewhall.fine.tune.constants = function(soil.memory.days = 5, 
                                        percolation.percent = 0.4, 
                                        ET.multiplier = 1.0, 
                                        k.scale = 11, 
                                        sat.column.ksat = .8,
                                        rootzone.high = 4.0, 
                                        rootzone.low = .75){
  soil.memory.days <<- soil.memory.days
  percolation.percent <<- percolation.percent 
  ET.multiplier <<- ET.multiplier
  k.scale <<- k.scale
  sat.column.ksat <<-  sat.column.ksat
  rootzone.high <<- rootzone.high
  rootzone.low <<- rootzone.low 
}












#rNewhall.Soils.Prep(1)


# # Step 2: Aggregate into 10cm slices
# soil.data = data.frame()
# i = 0
# #for (i in 0:9){
# build.profile  =  function(r){
#   a = ((r-1)*10)
#   b = ((r-1)+(9*(r)))
#   slice.0 <<- aqp::slice(query, 0:9 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
#   slice.0 = slice[complete.cases(slice), ]
#   
#   soils.mu.0 = slice.0  %>% group_by(mukey) %>%
#     summarise(sand = weighted.mean(sand,comppct, na.rm = T),
#               silt = weighted.mean(silt,comppct, na.rm = T),
#               clay = weighted.mean(clay,comppct, na.rm = T),
#               pwp = weighted.mean(pwp,comppct, na.rm = T),
#               fc = weighted.mean(fc,comppct, na.rm = T))
#   
#   soils.mu.0 = merge.data.frame(soils.mu.0, res)
#   
#   soils.cell.0 = soils.mu.0 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
#                                       silt = weighted.mean(silt,pct, na.rm = T),
#                                       clay = weighted.mean(clay,pct, na.rm = T),
#                                       pwp = weighted.mean(pwp,pct, na.rm = T),
#                                       fc = weighted.mean(fc,pct, na.rm = T))
#   
#   soils.cell.0$depth = "0-10"
#   soils.cell.0$cell = study.area.cells@data$cell[[x]]
#   
#   
#   soil.data <-rbind(soils.cell.0,soils.cell.1, soils.cell.2, soils.cell.3, soils.cell.4, soils.cell.5, soils.cell.6, soils.cell.7, soils.cell.8, soils.cell.9)
#   
#   
#   
#   
#   }
#   return(soils.cell)}
# build.profile(1)
# r = 1
