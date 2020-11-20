# Download and process all data needed for rNewhall

print.time = function(t, mode){
time = proc.time() - t
mins = round(time[3] / 60, digits = 2)
cat(paste("\nAll rNewhall ", mode,  " complete. \nTotal Time: ",mins, " mins\n", sep = ""))}

#create date ranges 
rNewhall.date.range = function(date.years){
  date.range <<- seq(as.Date(paste(date.years, "-1-1", sep = "")), as.Date(paste(date.years, "-12-31", sep = "")), "days" ) # create date range for model
spinup.range <<- seq(as.Date(paste(date.years-spinup.length, "-1-1", sep = "")), as.Date(paste(date.years-spinup.length, "-12-31", sep = "")), "days" ) # create date range for model
}
###################################################
#PRISM precip data
# x = study.area
# buffer = 0.01
rNewhall.PRISM.Preprocess = function(chunk.num, buffer, data.type = "ppt",  output.type = "dataframe", save.output = F){ # options output.type  = "dataframe" or "raster"
  options(warn=-1)
  ptm <- proc.time()
  chunk = process.chunks[chunk.num,]
  daily.PRISM.stack = stack() # Initiate raster brick
  # Process spin up data
  if (os == "PC"){upload.path = paste("F:/PRISM_data/an81/",data.type,"/daily/",year(spinup.range[1]),"/",sep = "")}
  if (os == "Mac"){upload.path = paste("/Volumes/Elements/PRISM_data/an81/",data.type,"/daily/",year(spinup.range[1]),"/",sep = "")}
  PRISM.list= list.files(upload.path, pattern =  "\\.bil$", full.names=T)
  chunk.buffer = buffer(chunk, width=buffer)
  message("PRISM PRCP processing for model spin-up period")
  pb = txtProgressBar(min = 0, max = length(PRISM.list), style = 3) 
  for (i in 1:length(PRISM.list)){
    daily.PRISM.rast = raster(PRISM.list[i]) # read it daily PRISM .bil from archive folder
    daily.PRISM.rast.c = crop(daily.PRISM.rast, chunk.buffer)#, snap = "near") # clip to study area
    daily.PRISM.stack <- stack(daily.PRISM.stack,daily.PRISM.rast.c) # stack these data into a CONUS extent raster stack
    setTxtProgressBar(pb,i)
  }
  # Process date range data
  if (os == "PC"){upload.path = paste("F:/PRISM_data/an81/",data.type,"/daily/",year(date.range[1]),"/",sep = "")}
  if (os == "Mac"){upload.path = paste("/Volumes/Elements/PRISM_data/an81/",data.type,"/daily/",year(date.range[1]),"/",sep = "")}
  PRISM.list= list.files(upload.path, pattern =  "\\.bil$", full.names=T)
  cat("\n")# spacer
  message("PRISM PRCP processing for model period")
  pb = txtProgressBar(min = 0, max = length(PRISM.list), style = 3) 
  for (i in 1:length(PRISM.list)){
    daily.PRISM.rast = raster(PRISM.list[i]) # read it daily PRISM .bil from archive folder
    daily.PRISM.rast.c = crop(daily.PRISM.rast, chunk.buffer)#, snap = "near") # clip to study area
    daily.PRISM.stack <- stack(daily.PRISM.stack,daily.PRISM.rast.c) # stack these data into a CONUS extent raster stack
    setTxtProgressBar(pb,i)
  }
  
  # Extract all cells to make a dataframe of values
  PRISM.cells = unlist(cellFromPolygon(daily.PRISM.stack, chunk))
  if (data.type == "ppt"){
    PRISM.values = (as.data.frame(t(as.data.frame(raster::extract(daily.PRISM.stack, chunk, cellnumbers = F))))/10) # convert ppt from mm to cm
    colnames(PRISM.values) = PRISM.cells 
    PRISM.values = cbind(date = as.Date(spinup.range[1]:date.range[length(date.range)]), PRISM.values)
    rownames(PRISM.values) <- NULL
    if (output.type == "dataframe"){
      PRISM.ppt <<- PRISM.values
      if (save.output == T){
        save(PRISM.ppt, file = paste("./rNewhall_data_preprocessing/prcp/PRISM_",data.type,"_", date.years,"_",chunk$id, "_dataframe.RData", sep = ""))
      }}
    if (output.type == "raster"){
      PRISM.ppt.stack <<- daily.PRISM.stack
      if (save.output == T){
        save(PRISM.ppt.stack, file = paste("./rNewhall_data_preprocessing/prcp/PRISM_",data.type,"_", date.years, "_",chunk$id,"_raster.RData", sep = ""))
      }}
    if (output.type != "dataframe" & output.type !="raster" ) {stop("Wrong output type...you wasted all that processing time.")}
  } 
  if (data.type == "tmax"){
   PRISM.values = (as.data.frame(t(as.data.frame(raster::extract(daily.PRISM.stack, chunk, cellnumbers = F))))) # convert ppt from mm to cm
   colnames(PRISM.values) = PRISM.cells 
   PRISM.values = cbind(date = as.Date(spinup.range[1]:date.range[length(date.range)]), PRISM.values)
   rownames(PRISM.values) <- NULL
   if (output.type == "dataframe"){
     PRISM.tmax <<- PRISM.values
     if (save.output == T){
       save(PRISM.tmax, file = paste("./rNewhall_data_preprocessing/tmax/PRISM_",data.type,"_", date.years,"_",chunk$id, "_dataframe.RData", sep = ""))
     }}
   if (output.type == "raster"){
     PRISM.tmax.stack <<- daily.PRISM.stack
     if (save.output == T){
       save(PRISM.tmax.stack, file = paste("./rNewhall_data_preprocessing/tmax/PRISM_",data.type,"_", date.years, "_",chunk$id,"_raster.RData", sep = ""))
     }}
   if (output.type != "dataframe" & output.type !="raster" ) {stop("Wrong output type...you wasted all that processing time.")}
  } 
  if (data.type == "tmin"){
    PRISM.values = (as.data.frame(t(as.data.frame(raster::extract(daily.PRISM.stack, chunk, cellnumbers = F))))) # convert ppt from mm to cm
    colnames(PRISM.values) = PRISM.cells 
    PRISM.values = cbind(date = as.Date(spinup.range[1]:date.range[length(date.range)]), PRISM.values)
    rownames(PRISM.values) <- NULL
    if (output.type == "dataframe"){
      PRISM.tmin <<- PRISM.values
      if (save.output == T){
        save(PRISM.tmin, file = paste("./rNewhall_data_preprocessing/tmin/PRISM_",data.type,"_", date.years,"_",chunk$id, "_dataframe.RData", sep = ""))
      }}
    if (output.type == "raster"){
      PRISM.tmin.stack <<- daily.PRISM.stack
      if (save.output == T){
        save(PRISM.tmin.stack, file = paste("./rNewhall_data_preprocessing/tmin/PRISM_",data.type,"_", date.years, "_",chunk$id,"_raster.RData", sep = ""))
      }}
    if (output.type != "dataframe" & output.type !="raster" ) {stop("Wrong output type...you wasted all that processing time.")}
  } 
  
 
  # Create template for use in later processing steps and generate figure for review
  # PRISM.Template <<- daily.PRISM.prcp.stack[[185]]
  # plot(PRISM.Template, main = "PRISM Precip. extent with study area overlay")
  # plot(chunk, add =T)
  
  time = proc.time() - ptm 
  mins = round(time[3] / 60, digits = 2)
  options(warn=0)
  return(cat(paste("\nPRISM data prep complete for study area. \nTotal Time: ",mins, " mins\n", sep = "")))}

chunk.num = 10
###################################################
# MODIS Data
# 1. Prep sudy area, extract centroids, and calculate length/width of area to be extracted
rNewhall.MODIS.Preprocess = function(chunk.num, reproject.to.PRISM = TRUE, save.output = TRUE){
  ptm <- proc.time() # start time
  chunk = process.chunks[chunk.num,]
  e = extent(chunk)
  centroid = rgeos::gCentroid(chunk,byid=TRUE)
  # 2.Extarct and download MODIS data
  # 2.1 MODIS ET and PET
  MODIS.Evap <- mt_subset(product = "MOD16A2",
                          lat = centroid@coords[1,2],
                          lon = centroid@coords[1,1],
                          band = c("ET_500m", "PET_500m"),
                          km_lr = ((distGeo(c(e@xmin, e@ymin), c(e@xmax, e@ymin)))/600), # this value need to be less than 1000 to expand the size of the MODIS raster so that when it is clipped after reprojection it fills the entire chunk
                          km_ab = ((distGeo(c(e@xmin, e@ymin), c(e@xmin, e@ymax)))/600),
                          start = spinup.range[1],
                          end = date.range[length(date.range)],
                          progress = T)
  # 2.2 MODIS LAI
  MODIS.Leaf <- mt_subset(product = "MCD15A2H",
                          lat = centroid@coords[1,2],
                          lon = centroid@coords[1,1],
                          band = c("Lai_500m"),
                          km_lr = ((distGeo(c(e@xmin, e@ymin), c(e@xmax, e@ymin)))/600),
                          km_ab = ((distGeo(c(e@xmin, e@ymin), c(e@xmin, e@ymax)))/600),
                          start = spinup.range[1],
                          end = date.range[length(date.range)],
                          progress = T)
  
  # 3. Remove clouds and other obstructions
  MODIS.Evap$value[MODIS.Evap$value > 32760]  = NA
  MODIS.Leaf$value[MODIS.Leaf$value > 32760]  = NA
  
  # 3.1 Scale MODIS values form 8 day to 1 day
  MODIS.Evap$value = ((MODIS.Evap$value * 0.1)/8)
  MODIS.Leaf$value = ((MODIS.Leaf$value * 0.1)/8)
  
  # 4. Subset the data
  MODIS.ET <- MODIS.Evap %>% subset(band == "ET_500m") %>% na_interpolation(value, option = "linear", maxgap = Inf)
  MODIS.PET <- MODIS.Evap %>% subset(band == "PET_500m") %>% na_interpolation(value, option = "linear", maxgap = Inf)
  MODIS.LAI <- MODIS.Leaf %>% subset(band == "Lai_500m") %>% na_interpolation(value, option = "linear", maxgap = Inf)
  MODIS.ET.dates  = as.Date(unique(MODIS.ET$calendar_date))
  MODIS.PET.dates  = as.Date(unique(MODIS.PET$calendar_date))
  MODIS.LAI.dates  = as.Date(unique(MODIS.LAI$calendar_date))
  
  all.dates = data.frame(date = as.Date(spinup.range[1]:date.range[length(date.range)]))
  
  if (reproject.to.PRISM == TRUE){
    MODIS.ET.raster <- mt_to_raster(df = MODIS.ET, reproject = F)
    MODIS.PET.raster <- mt_to_raster(df = MODIS.PET, reproject = F)
    MODIS.LAI.raster <- mt_to_raster(df = MODIS.LAI, reproject = F)
    MODIS.ET.raster.prism = MODIS.ET.raster %>% projectRaster(res = 0.008333333,crs = crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"), method="bilinear") 
    MODIS.PET.raster.prism = MODIS.PET.raster %>% projectRaster(res = 0.008333333,crs = crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"), method="bilinear")
    MODIS.LAI.raster.prism = MODIS.LAI.raster %>% projectRaster(res = 0.008333333,crs = crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"), method="bilinear")
    #plot(MODIS.ET.raster.prism[[1]], main = "MODIS ET (resampled) extent with study area overlay")
    #plot(study.area, add =T)
    
    # get all cell numbers from the study area
    MODIS.cells = 1:3600

    # Reformat ET
    MODIS.ET.values = as.data.frame(t(as.data.frame(raster::extract(MODIS.ET.raster.prism, extent(chunk), cellnumbers = F))))
    MODIS.ET.values = MODIS.ET.values[,1:3600]
    colnames(MODIS.ET.values) = MODIS.cells 
    MODIS.ET.values$date = MODIS.ET.dates
    rownames(MODIS.ET.values) <- NULL
    MODIS.ET <- full_join(x= all.dates, y= MODIS.ET.values, by = 'date') %>% fill_(., names(.))
    # Reformat PET
    MODIS.PET.values = as.data.frame(t(as.data.frame(raster::extract(MODIS.PET.raster.prism, extent(chunk), cellnumbers = F))))
    MODIS.PET.values = MODIS.PET.values[,1:3600]
    colnames(MODIS.PET.values) = MODIS.cells 
    MODIS.PET.values$date = MODIS.PET.dates
    rownames(MODIS.PET.values) <- NULL
    MODIS.PET <- full_join(x= all.dates, y= MODIS.PET.values, by = 'date') %>% fill_(., names(.))
    # Reformat LAI
    MODIS.LAI.values = as.data.frame(t(as.data.frame(raster::extract(MODIS.LAI.raster.prism, extent(chunk), cellnumbers = F))))
    MODIS.LAI.values = MODIS.LAI.values[,1:3600]
    colnames(MODIS.LAI.values) = MODIS.cells 
    MODIS.LAI.values$date = MODIS.LAI.dates
    rownames(MODIS.LAI.values) <- NULL
    MODIS.LAI <- full_join(x= all.dates, y= MODIS.LAI.values, by = 'date') %>% fill_(., names(.))}

  if (save.output == T){
    save(MODIS.ET, file = paste("./rNewhall_data_preprocessing/MODIS/MODIS_ET", date.years, chunk$id,"dataframe.RData", sep = "_"))
    save(MODIS.PET, file = paste("./rNewhall_data_preprocessing/MODIS/MODIS_PET", date.years, chunk$id,"dataframe.RData", sep = "_"))
    save(MODIS.LAI, file = paste("./rNewhall_data_preprocessing/MODIS/MODIS_LAI", date.years, chunk$id,"dataframe.RData", sep = "_"))
  }
  time = proc.time() - ptm 
  mins = round(time[3] / 60, digits = 2)
  return(cat(paste("MODIS data prep complete for study area. \nTotal Time: ",mins, " mins\n", sep = "")))}
###################################################



###################################################
#Soils
# download and compile soil data
rNewhall.Single.Cell.Soil.Prep= function(cell){
  box= bbox(study.area.cells@polygons[[cell]]@Polygons[[1]]@coords)
  bbox.sp <- as(extent(box), 'SpatialPolygons')
  proj4string(bbox.sp) <- '+proj=longlat +datum=WGS84'
  bbox.wkt <- writeWKT(as(extent(box), 'SpatialPolygons'))
  res <- SDA_spatialQuery(bbox.sp, what = 'geom', geomIntersection = TRUE)
  res$area = area(res) 
  res$pct = res$area/sum(res$area)
  # plot(res)
  # lines(bbox.sp, col='red', lwd=2)
  
  # get soils data for the cell 
  q <- sprintf("SELECT mukey, co.cokey, compname, comppct_r AS comppct,
            chkey, hzname, hzdept_r, hzdepb_r,  (hzdepb_r - hzdept_r) AS thick,
                 wsatiated_r / 100.0 AS sat,
                 wthirdbar_r / 100.0 AS fc,
                 wfifteenbar_r / 100.0 as pwp,
                 awc_r as awc, sandtotal_r AS sand, silttotal_r AS silt, claytotal_r AS clay
            FROM component AS co
            JOIN chorizon AS hz ON co.cokey = hz.cokey
            WHERE mukey IN (SELECT DISTINCT mukey FROM SDA_Get_Mukey_from_intersection_with_WktWgs84('%s') )
            ORDER BY mukey, co.cokey, comppct_r DESC", bbox.wkt)
  
  # send query, results are tabular data only
  query <- SDA_query(q)
  
  # error handling
  if (is.null(query) == T) {
    soil.data = data.frame(sand = rep(NA, 10), silt = rep(NA, 10), clay = rep(NA, 10), pwp = rep(NA, 10), fc = rep(NA, 10), depth = c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100"),cell = rep(study.area.cells@data$cell[[cell]], 10))
    } else {
  aqp::depths(query) <- chkey ~ hzdept_r + hzdepb_r
  
  # build the soil profile
  slice.0 <- aqp::slice(query, 0:9 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
  slice.0 = slice.0[complete.cases(slice.0),]
  
  soils.mu.0 = slice.0  %>% group_by(mukey) %>%
    summarise(sand = weighted.mean(sand,comppct, na.rm = T),
              silt = weighted.mean(silt,comppct, na.rm = T),
              clay = weighted.mean(clay,comppct, na.rm = T),
              pwp = weighted.mean(pwp,comppct, na.rm = T),
              fc = weighted.mean(fc,comppct, na.rm = T))
  
  soils.mu.0 = merge.data.frame(soils.mu.0, res)
  
  soils.cell.0 = soils.mu.0 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
                                          silt = weighted.mean(silt,pct, na.rm = T),
                                          clay = weighted.mean(clay,pct, na.rm = T),
                                          pwp = weighted.mean(pwp,pct, na.rm = T),
                                          fc = weighted.mean(fc,pct, na.rm = T))
  
  soils.cell.0$depth = "0-10"
  soils.cell.0$cell = study.area.cells@data$cell[[cell]]
  
  
  
  slice.1 <- aqp::slice(query, 10:19 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
  slice.1 = slice.1[complete.cases(slice.1), ]
  
  soils.mu.1 = slice.1  %>% group_by(mukey) %>%
    summarise(sand = weighted.mean(sand,comppct, na.rm = T),
              silt = weighted.mean(silt,comppct, na.rm = T),
              clay = weighted.mean(clay,comppct, na.rm = T),
              pwp = weighted.mean(pwp,comppct, na.rm = T),
              fc = weighted.mean(fc,comppct, na.rm = T))
  
  soils.mu.1 = merge.data.frame(soils.mu.1, res)
  
  soils.cell.1 = soils.mu.1 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
                                          silt = weighted.mean(silt,pct, na.rm = T),
                                          clay = weighted.mean(clay,pct, na.rm = T),
                                          pwp = weighted.mean(pwp,pct, na.rm = T),
                                          fc = weighted.mean(fc,pct, na.rm = T))
  
  soils.cell.1$depth = "10-20"
  soils.cell.1$cell = study.area.cells@data$cell[[cell]]
  
  
  slice.2 <- aqp::slice(query, 20:29 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
  slice.2 = slice.2[complete.cases(slice.2), ]
  
  soils.mu.2 = slice.2  %>% group_by(mukey) %>%
    summarise(sand = weighted.mean(sand,comppct, na.rm = T),
              silt = weighted.mean(silt,comppct, na.rm = T),
              clay = weighted.mean(clay,comppct, na.rm = T),
              pwp = weighted.mean(pwp,comppct, na.rm = T),
              fc = weighted.mean(fc,comppct, na.rm = T))
  
  soils.mu.2 = merge.data.frame(soils.mu.2, res)
  
  soils.cell.2 = soils.mu.2 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
                                          silt = weighted.mean(silt,pct, na.rm = T),
                                          clay = weighted.mean(clay,pct, na.rm = T),
                                          pwp = weighted.mean(pwp,pct, na.rm = T),
                                          fc = weighted.mean(fc,pct, na.rm = T))
  
  soils.cell.2$depth = "20-30"
  soils.cell.2$cell = study.area.cells@data$cell[[cell]]
  
  
  slice.3 <- aqp::slice(query, 30:39 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
  slice.3 = slice.3[complete.cases(slice.3), ]
  
  soils.mu.3 = slice.3  %>% group_by(mukey) %>%
    summarise(sand = weighted.mean(sand,comppct, na.rm = T),
              silt = weighted.mean(silt,comppct, na.rm = T),
              clay = weighted.mean(clay,comppct, na.rm = T),
              pwp = weighted.mean(pwp,comppct, na.rm = T),
              fc = weighted.mean(fc,comppct, na.rm = T))
  
  soils.mu.3 = merge.data.frame(soils.mu.3, res)
  
  soils.cell.3 = soils.mu.3 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
                                          silt = weighted.mean(silt,pct, na.rm = T),
                                          clay = weighted.mean(clay,pct, na.rm = T),
                                          pwp = weighted.mean(pwp,pct, na.rm = T),
                                          fc = weighted.mean(fc,pct, na.rm = T))
  
  soils.cell.3$depth = "30-40"
  soils.cell.3$cell = study.area.cells@data$cell[[cell]]
  
  slice.4 <- aqp::slice(query, 40:49 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
  slice.4 = slice.4[complete.cases(slice.4), ]
  
  soils.mu.4 = slice.4  %>% group_by(mukey) %>%
    summarise(sand = weighted.mean(sand,comppct, na.rm = T),
              silt = weighted.mean(silt,comppct, na.rm = T),
              clay = weighted.mean(clay,comppct, na.rm = T),
              pwp = weighted.mean(pwp,comppct, na.rm = T),
              fc = weighted.mean(fc,comppct, na.rm = T))
  
  soils.mu.4 = merge.data.frame(soils.mu.4, res)
  
  soils.cell.4 = soils.mu.4 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
                                          silt = weighted.mean(silt,pct, na.rm = T),
                                          clay = weighted.mean(clay,pct, na.rm = T),
                                          pwp = weighted.mean(pwp,pct, na.rm = T),
                                          fc = weighted.mean(fc,pct, na.rm = T))
  
  soils.cell.4$depth = "40-50"
  soils.cell.4$cell = study.area.cells@data$cell[[cell]]
  
  slice.5 <- aqp::slice(query, 50:59 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
  slice.5 = slice.5[complete.cases(slice.5), ]
  
  soils.mu.5 = slice.5  %>% group_by(mukey) %>%
    summarise(sand = weighted.mean(sand,comppct, na.rm = T),
              silt = weighted.mean(silt,comppct, na.rm = T),
              clay = weighted.mean(clay,comppct, na.rm = T),
              pwp = weighted.mean(pwp,comppct, na.rm = T),
              fc = weighted.mean(fc,comppct, na.rm = T))
  
  soils.mu.5 = merge.data.frame(soils.mu.5, res)
  
  soils.cell.5 = soils.mu.5 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
                                          silt = weighted.mean(silt,pct, na.rm = T),
                                          clay = weighted.mean(clay,pct, na.rm = T),
                                          pwp = weighted.mean(pwp,pct, na.rm = T),
                                          fc = weighted.mean(fc,pct, na.rm = T))
  
  soils.cell.5$depth = "50-60"
  soils.cell.5$cell = study.area.cells@data$cell[[cell]]
  
  slice.6 <- aqp::slice(query, 60:69 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
  slice.6 = slice.6[complete.cases(slice.6), ]
  
  soils.mu.6 = slice.6  %>% group_by(mukey) %>%
    summarise(sand = weighted.mean(sand,comppct, na.rm = T),
              silt = weighted.mean(silt,comppct, na.rm = T),
              clay = weighted.mean(clay,comppct, na.rm = T),
              pwp = weighted.mean(pwp,comppct, na.rm = T),
              fc = weighted.mean(fc,comppct, na.rm = T))
  
  soils.mu.6 = merge.data.frame(soils.mu.6, res)
  
  soils.cell.6 = soils.mu.6 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
                                          silt = weighted.mean(silt,pct, na.rm = T),
                                          clay = weighted.mean(clay,pct, na.rm = T),
                                          pwp = weighted.mean(pwp,pct, na.rm = T),
                                          fc = weighted.mean(fc,pct, na.rm = T))
  
  soils.cell.6$depth = "60-70"
  soils.cell.6$cell = study.area.cells@data$cell[[cell]]
  
  slice.7 <<- aqp::slice(query, 70:79 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
  slice.7 = slice.7[complete.cases(slice.7), ]
  
  soils.mu.7 = slice.7  %>% group_by(mukey) %>%
    summarise(sand = weighted.mean(sand,comppct, na.rm = T),
              silt = weighted.mean(silt,comppct, na.rm = T),
              clay = weighted.mean(clay,comppct, na.rm = T),
              pwp = weighted.mean(pwp,comppct, na.rm = T),
              fc = weighted.mean(fc,comppct, na.rm = T))
  
  soils.mu.7 = merge.data.frame(soils.mu.7, res)
  
  soils.cell.7 = soils.mu.7 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
                                          silt = weighted.mean(silt,pct, na.rm = T),
                                          clay = weighted.mean(clay,pct, na.rm = T),
                                          pwp = weighted.mean(pwp,pct, na.rm = T),
                                          fc = weighted.mean(fc,pct, na.rm = T))
  
  soils.cell.7$depth = "70-80"
  soils.cell.7$cell = study.area.cells@data$cell[[cell]]
  
  
  slice.8 <- aqp::slice(query, 80:89 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
  slice.8 = slice.8[complete.cases(slice.8), ]
  
  soils.mu.8 = slice.8  %>% group_by(mukey) %>%
    summarise(sand = weighted.mean(sand,comppct, na.rm = T),
              silt = weighted.mean(silt,comppct, na.rm = T),
              clay = weighted.mean(clay,comppct, na.rm = T),
              pwp = weighted.mean(pwp,comppct, na.rm = T),
              fc = weighted.mean(fc,comppct, na.rm = T))
  
  soils.mu.8 = merge.data.frame(soils.mu.8, res)
  
  soils.cell.8 = soils.mu.8 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
                                          silt = weighted.mean(silt,pct, na.rm = T),
                                          clay = weighted.mean(clay,pct, na.rm = T),
                                          pwp = weighted.mean(pwp,pct, na.rm = T),
                                          fc = weighted.mean(fc,pct, na.rm = T))
  
  soils.cell.8$depth = "80-90"
  soils.cell.8$cell = study.area.cells@data$cell[[cell]]
  
  
  slice.9 <- aqp::slice(query, 90:99 ~ mukey + compname + hzname + sand + silt + clay + fc + pwp + awc + comppct , just.the.data=T) # maybe use slab? #
  slice.9 = slice.9[complete.cases(slice.9), ]
  
  soils.mu.9 = slice.9  %>% group_by(mukey) %>%
    summarise(sand = weighted.mean(sand,comppct, na.rm = T),
              silt = weighted.mean(silt,comppct, na.rm = T),
              clay = weighted.mean(clay,comppct, na.rm = T),
              pwp = weighted.mean(pwp,comppct, na.rm = T),
              fc = weighted.mean(fc,comppct, na.rm = T))
  
  soils.mu.9 = merge.data.frame(soils.mu.9, res)
  
  soils.cell.9 = soils.mu.9 %>% summarise(sand = weighted.mean(sand,pct, na.rm = T),
                                          silt = weighted.mean(silt,pct, na.rm = T),
                                          clay = weighted.mean(clay,pct, na.rm = T),
                                          pwp = weighted.mean(pwp,pct, na.rm = T),
                                          fc = weighted.mean(fc,pct, na.rm = T))
  
  soils.cell.9$depth = "90-100"
  soils.cell.9$cell = study.area.cells@data$cell[[cell]]
  soil.data <-rbind(soils.cell.0,soils.cell.1, soils.cell.2, soils.cell.3, soils.cell.4, soils.cell.5, soils.cell.6, soils.cell.7, soils.cell.8, soils.cell.9)
    }
  return(soil.data)
  }


rNewhall.Soils.Preprocess = function(chunk.num, save.output = TRUE){
    ptm <- proc.time()
    chunk = process.chunks[chunk.num,]
    study.area.cells= raster(extent(chunk), resolution = c(0.008333333, 0.008333333), crs = proj4string(chunk)) %>% rasterToPolygons() 
    study.area.cells$cell = 1:length(study.area.cells$layer)
    study.area.cells <<- study.area.cells

    start <- proc.time()
    study.area.soils = lapply(1:length(study.area.cells),rNewhall.Single.Cell.Soil.Prep) %>% bind_rows()
    proc.time() - start
    
    if (save.output == T){
      save(study.area.soils, file = paste("./rNewhall_data_preprocessing/soils/soil", date.years, chunk$id,"dataframe.RData", sep = "_"))}
    time = proc.time() - ptm 
    mins = round(time[3] / 60, digits = 2)
    return(cat(paste("\nSoil data preprocessing for rNewhall is completed. \nTotal Time: ",mins, " mins\n", sep = "")))
    }





