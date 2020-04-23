options(warn=-1)
# Load datasets
# Met data
met.data = read.csv(paste("./data/",site, "_mesonet_data_complete_cases.csv", sep = ""))
met.data$DATE = as.Date(with(met.data, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d") # create a date field
met.data$RAIN = met.data$RAIN * 25.4 #convert inches to  mm
met.data$RAIN[met.data$RAIN < 0] = 0 # for now, convert NAs to 0
# Soil Data (all units in cm derived rates)
soil.data = read.csv("./data/MesoSoilv1_3.csv")

# create date range
date.sub = subset(met.data, YEAR == date.years)
date.range = as.Date(as.vector(date.sub$DATE))
# create spinup range
spinup.sub = subset(met.data, YEAR <= date.years-1 & YEAR >= date.years - spinup.length )
spinup.range = as.Date(as.vector(spinup.sub$DATE))
# subset soil data
soil.data.sub = subset(soil.data, Site==site & Sand != -9.9)


# Process all necessary soil data from OK Mesonet
AWS.mesonet = (soil.data.sub$Th33 - soil.data.sub$Th1500) * row.depth  # Only ten here due to the fact it is a 10 cm slice of soil; now this is cm3 / 10 cm3
BD.mesonet = soil.data.sub$BulkD * row.depth # Only ten here due to the fact it is a 10 cm slice of soil; now this is cm3 / 10 cm3
WC.033.mesonet = soil.data.sub$Th33 * row.depth  # Only ten here due to the fact it is a 10 cm slice of soil; now this is cm3 / 10 cm3
WC.15.mesonet = soil.data.sub$Th1500 * row.depth  # Only ten here due to the fact it is a 10 cm slice of soil; now this is cm3 / 10 cm3
WC.sat.mesonet  = soil.data.sub$Theta_s  * row.depth # Only ten here due to the fact it is a 10 cm slice of soil; now this is cm3 / 10 cm3
WC.ret.mesonet = soil.data.sub$Theta_r * row.depth # Only ten here due to the fact it is a 10 cm slice of soil; now this is cm3 / 10 cm3
Ksat.mesonet = soil.data.sub$Ks 
L.mesonet = soil.data.sub$L
N.mesonet = soil.data.sub$N
sand = soil.data.sub$Sand
silt = soil.data.sub$Silt
clay = soil.data.sub$Clay

#build the variable lists needed toconstruct the mm later
AWS = c(AWS.mesonet[1], AWS.mesonet[2], AWS.mesonet[2], AWS.mesonet[2], AWS.mesonet[3], AWS.mesonet[3], AWS.mesonet[4], AWS.mesonet[5], AWS.mesonet[5], AWS.mesonet[5])
BD = c(BD.mesonet[1], BD.mesonet[2], BD.mesonet[2],BD.mesonet[2],BD.mesonet[3],BD.mesonet[3],BD.mesonet[4],BD.mesonet[5],BD.mesonet[5],BD.mesonet[5] )
WC.033 = c(WC.033.mesonet[1], WC.033.mesonet[2],WC.033.mesonet[2],WC.033.mesonet[2],WC.033.mesonet[3],WC.033.mesonet[3],WC.033.mesonet[4],WC.033.mesonet[5],WC.033.mesonet[5],WC.033.mesonet[5])
WC.15  = c(WC.15.mesonet[1],WC.15.mesonet[2],WC.15.mesonet[2],WC.15.mesonet[2],WC.15.mesonet[3],WC.15.mesonet[3],WC.15.mesonet[4],WC.15.mesonet[5],WC.15.mesonet[5],WC.15.mesonet[5])
WC.ret = c(c(WC.ret.mesonet[1],WC.ret.mesonet[2],WC.ret.mesonet[2],WC.ret.mesonet[2],WC.ret.mesonet[3],WC.ret.mesonet[3],WC.ret.mesonet[4],WC.ret.mesonet[5],WC.ret.mesonet[5],WC.ret.mesonet[5]))
WC.sat = c(WC.sat.mesonet[1],WC.sat.mesonet[2],WC.sat.mesonet[2],WC.sat.mesonet[2],WC.sat.mesonet[3],WC.sat.mesonet[3],WC.sat.mesonet[4],WC.sat.mesonet[5],WC.sat.mesonet[5],WC.sat.mesonet[5])
Ksat = c(Ksat.mesonet[1],Ksat.mesonet[2],Ksat.mesonet[2],Ksat.mesonet[2],Ksat.mesonet[3],Ksat.mesonet[3],Ksat.mesonet[4],Ksat.mesonet[5],Ksat.mesonet[5],Ksat.mesonet[5])
L = c(L.mesonet[1],L.mesonet[2],L.mesonet[2],L.mesonet[2],L.mesonet[3],L.mesonet[3],L.mesonet[4],L.mesonet[5],L.mesonet[5],L.mesonet[5])
N = c(N.mesonet[1],N.mesonet[2],N.mesonet[2],N.mesonet[2],N.mesonet[3],N.mesonet[3],N.mesonet[4],N.mesonet[5],N.mesonet[5],N.mesonet[5])


# Set latitude and longitude for later use
long = model.sites$elon
lat = model.sites$nlat

# create display plot
# site.map <- leaflet(model.sites) %>% # addTiles() %>% # Add default OpenStreetMap map tiles
#   addProviderTiles(providers$Thunderforest.Outdoors, options = providerTileOptions(apikey = "c82c34e6c8e542adac6e7d0229f9ccb2")) %>% 
#   addMarkers(lng = ~elon, lat = ~nlat, popup = "Marena, OK Mesonet Station")


# # Create Rosetta approximation function (from the Okalhoma State Univ. Soil Physics lab)
# soil_class <- function(clay, sand, silt = NULL) {
#   if(is.null(clay)) stop("Clay percentage is required", call. = FALSE)
#   if(is.null(sand) & is.null(silt)) {
#     stop("You must provide at either sand or silt percentages (or both)", 
#          call. = FALSE)
#   }
#   if(is.null(sand) & !is.null(silt)) sand <- 100 - (clay + silt)
#   if(is.null(silt)) silt <- 100 - (clay + sand)
#   tri.data <- cbind.data.frame("CLAY" = clay, "SILT" = silt, "SAND" = sand)   
#   if(any(round(rowSums(tri.data), 1) != 100)) stop("Textures don't add up")
#   texclass <- tt_class(tri.data = tri.data, PiC.type = "n")
#   ind <- which(texclass > 0)
#   tclass <- apply(texclass, 1, function(x) colnames(texclass)[which(x > 0)[1]])
#   return(tclass)
# }
# 
# 
# 

 rosetta.approx = function(sand, silt, clay) {
   rosetta.matrix = matrix(c(
      0.098, 0.459, 0.0150, 1.25, 14.76,  2.952, -1.531,
      0.079, 0.442, 0.0158, 1.42, 8.184,  4.992, -0.763,
      0.061, 0.399, 0.0111, 1.47, 12.048,  3.696, -0.371,
      0.049, 0.390, 0.0348, 1.75, 105.192,  24.312, -0.874,
      0.053, 0.375, 0.0352, 3.18, 642.696, 24.480,  -0.930,
      0.117, 0.385, 0.0334, 1.21, 11.352,  4.344, -3.665,
      0.063, 0.384, 0.0211, 1.33, 13.176,  6.936, -1.280,
      0.039, 0.387, 0.0267, 1.45, 38.280,  15.480, -0.861,
      0.05,  0.489, 0.0066, 1.68, 43.752,  3.336,  0.624,
      0.111, 0.481, 0.0162, 1.32, 9.624,  3.168, -1.287,
      0.090, 0.482, 0.0084, 1.52, 11.112,  2.232, -0.156,
      0.065, 0.439, 0.0051, 1.66, 18.240,  1.752,  0.365), nrow = 12, ncol = 7, byrow=TRUE, dimnames = 
        list(c('Cl','ClLo','Lo','LoSa','Sa', 'SaCl', 'SaClLo', 'SaLo','Si','SiCl', 'SiClLo', 'SiLo'), 
             c('Theta_res [cm3/cm3]', 'theta_sat [cm3/cm3]', 'alpha [1/cm]', 'n', 'Ksat [cm/day]', 'Ko [cm/day]', 'L')))
   class = TT.points.in.classes(data.frame("CLAY" = c(clay), "SILT" = c(silt), "SAND" = c(sand)), "USDA.TT")
   final.class = names(class[, colSums(class != 0) > 0])
   rosetta.attr = as.data.frame(rbind(rosetta.matrix[final.class,]))
   return(rosetta.attr)}

 options(warn=0)
