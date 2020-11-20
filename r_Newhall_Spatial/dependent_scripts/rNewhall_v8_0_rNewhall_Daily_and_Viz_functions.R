# rNewhall Daily function
Run.rNewhall.Daily.Batch  = function(chunk.num){ #wrapper function run all the data prep for each chunk before it is fed to rNewhall.Daily
  chunk = process.chunks[chunk.num,]
  study.area.cells= raster(extent(chunk), resolution = c(0.008333333, 0.008333333), crs = proj4string(chunk)) %>% rasterToPolygons() 
  study.area.cells$cell = 1:length(study.area.cells)
  study.area.cells = study.area.cells[study.area,]
  study.area.cells <<- study.area.cells
  
  chunk.id = process.chunks.list[chunk.num]
  
  load(paste("./rNewhall_data_preprocessing/prcp/PRISM_ppt", date.years, chunk.id, "dataframe.RData", sep = "_"))
  load(paste("./rNewhall_data_preprocessing/tmin/PRISM_tmin", date.years, chunk.id, "dataframe.RData", sep = "_"))
  load(paste("./rNewhall_data_preprocessing/tmax/PRISM_tmax", date.years, chunk.id, "dataframe.RData", sep = "_"))
  load(paste("./rNewhall_data_preprocessing/MODIS/MODIS_ET", date.years, chunk.id, "dataframe.RData", sep = "_"))
  load(paste("./rNewhall_data_preprocessing/MODIS/MODIS_PET", date.years, chunk.id, "dataframe.RData", sep = "_"))
  load(paste("./rNewhall_data_preprocessing/MODIS/MODIS_LAI", date.years, chunk.id, "dataframe.RData", sep = "_"))
  load(paste("./rNewhall_data_preprocessing/soils/soil", date.years, chunk.id, "dataframe.RData", sep = "_"))
  
  #extract data from preprocessed chunk
  PRISM.prcp <<- PRISM.ppt
  PRISM.tmin <<- PRISM.tmin
  PRISM.tmax <<- PRISM.tmin
  MODIS.ET <<- MODIS.ET
  MODIS.PET <<- MODIS.PET
  MODIS.LAI <<- MODIS.LAI
  study.area.soils <<- study.area.soils
  out = lapply(study.area.cells$cell[1:length(study.area.cells)],rNewhall.Daily)
  return(out)
}



# Essentially a wrapper around the other rNewhall functions that allow a mm to e created and run for each cell in the study area
rNewhall.Daily = function(cell){
  # define cell's coordinates for ET calculation
  define.location(cell.num = cell)
  
  # subset met and MODIS data
  subset.met.data(cell.num = cell)
  subset.MODIS.data(cell.num = cell)
  
  # Create moisture matrix and other soil sttributes for each cell
  # ***Note: generates soils varibale unsed in the following functions
  t <- try(create.mm.soil.attr(cell.num = cell))
  # an error catch to deal with areas that do not have soils data
  if("try-error" %in% class(t)) {
    setClass("rNewhall.multi.output", slots = representation(day = "Date",
                                                             prcp = "numeric",
                                                             ET = "numeric",
                                                             mm.value = "matrix",
                                                             mm.max = "matrix",
                                                             moisture.0.10 = "numeric",
                                                             moisture.10.20 = "numeric",
                                                             moisture.20.30 = "numeric",
                                                             moisture.30.40 = "numeric",
                                                             moisture.40.50 = "numeric",
                                                             moisture.50.60 = "numeric",
                                                             moisture.60.70 = "numeric",
                                                             moisture.70.80 = "numeric",
                                                             moisture.80.90 = "numeric",
                                                             moisture.90.100 = "numeric"))

    cell.results = list()
    for (i in 1:length(date.range)){
    cell.output = new("rNewhall.multi.output", 
                      day = date.range[i], 
                      prcp = NaN, ET = NaN, 
                      mm.value = matrix(NaN, nrow = 1, ncol = 1),
                      mm.max = matrix(NaN, nrow = 1, ncol = 1),
                      moisture.0.10 = NaN,
                      moisture.10.20 = NaN,
                      moisture.20.30 = NaN,
                      moisture.30.40 = NaN,
                      moisture.40.50 = NaN,
                      moisture.50.60 = NaN,
                      moisture.60.70 = NaN,
                      moisture.70.80 = NaN,
                      moisture.80.90 = NaN,
                      moisture.90.100 = NaN)
cell.results = append(cell.results, cell.output)}

    
    } else {
  # create accretion and depletion costs, as well as other varibales impacting soil water dynamics
  create.accretion.depletion(soil.input = soils)
  create.kc()
  create.interception()
  
  # create initial condition for the mositure matrix
  mm <<- rep(0, length(mm.max))
  
  # run rNewhall function for each day for this cell - spinup phase
  spinup.out =  lapply(spinup.range, rNewhall.model.run, run.type = "spin-up")
  rm(spinup.out) # clear spin-up from memory
  
  # run rNewhall function for each day for this cell - spinup phase
  cell.results =  lapply(date.range, rNewhall.model.run, run.type = "model")}
return(cell.results)
}


# rNewhall visualization function
rNewhall.vis = function(rNewhall.output, start.date, end.date, depth.top, depth.bottom, plot.raster = T, export.raster = T){
  # Part 4: Map Results ### ---------------------------------------------------------
  # 4.1 Create a map of the results
  # 4.1.1 Query the output s4 object to build a raster from the results
  ptm <- proc.time() # start time


  
  # function to extract the desired values
  extract.values = function(x,y){
    out = slot(rNewhall.output.sub[[x]][[y]], paste("moisture.",depth.top,".",depth.bottom,sep = ""))
    return(out)
  }
  
 
  output.raster.list = list()
  pb = invisible(txtProgressBar(min = 0, max = length(rNewhall.output), initial = 0, style =3)) # create progress bar
  for(i in 1:length(rNewhall.output)){
    rNewhall.output.sub = rNewhall.output[[i]]
  
    # create raster template
    Chunk.Template =  raster(extent(process.chunks[i,]), resolution = c(0.008333333, 0.008333333), crs = proj4string(process.chunks[i,]))
    Chunk.Template[] = NA
    vector.out = vector()
    # create dataframe for querying the results. This will be fed to mapply
    dates = unique(seq(match(c(as.Date(start.date)),date.range),match(c(as.Date(end.date)),date.range)))
    depths  = seq(depth.top,depth.bottom, by = row.depth)
    
    # subset the cells that are within the study area and set the order for filling teh raster
    chunk = process.chunks[i,]
    study.area.cells= raster(extent(chunk), resolution = c(0.008333333, 0.008333333), crs = proj4string(chunk)) %>% rasterToPolygons() 
    study.area.cells$cell = 1:length(study.area.cells)
    study.area.cells = study.area.cells[study.area,]
    study.area.cells = study.area.cells
    cells.chunk = study.area.cells$cell
    cells = 1:length(rNewhall.output.sub)
    query.cells = rep(cells, each = length(dates)) 
    query.dates = rep(dates,length(cells))
    
    # Run extraction to build vector from whihc raster will be generated
    vector.initial = mapply(extract.values, query.cells, query.dates)
    vector.out = append(vector.out, vector.initial)
  
    # create vactors for filling the rasters
    fill.cells = rep(cells, length(dates))
    fill.dates = rep(dates,each = length(cells))
    # function to build a raster stack (each day as a layer)
    results.stack = stack()

  for( g in 1:length(dates)){
    fill = vector.out[seq(g, length(vector.out), length(dates))]
    
    for (h in 1: length(cells)){
      Chunk.Template[cells.chunk[h]] = fill[h]}
    #result.raster = crop(Chunk.Template, study.area)
    result.raster =Chunk.Template
    results.stack = stack(results.stack,result.raster)
    names(results.stack)[g] = paste(output.type, date.range[g], sep = ".")
    
  }
  assign(paste("results.stack",i,sep = "."), results.stack)
  output.raster.list <- append(get(paste("results.stack", i, sep = ".")), output.raster.list)
  setTxtProgressBar(pb,i)
  }
  
 rNewhall.results.stack <- do.call(raster::merge, unname(output.raster.list))
 names(rNewhall.results.stack) =  seq(as.Date(start.date), as.Date(end.date), "days" ) # need to see if this still breaks
 rNewhall.results.stack <<-  rNewhall.results.stack 
  
  if (plot.raster == T){
    plot(calc(rNewhall.results.stack, mean), main = "rNewhall results with study area overlay", zlim=c(0,.5),sub = paste("Dates: ", start.date, " - ", end.date, "        Depth: ", depth.top, " - ", depth.bottom, "cm        Type: Mean ",output.type, sep = ""))
    plot(study.area, add =T)
  }

  if (export.raster == T){
  save.file.dir = tk_choose.dir()
  writeRaster(rNewhall.results.stack, filename = paste(save.file.dir,"/rNewhall_results", study.area.name, ".tif", sep =""), format = "GTiff", overwrite = T)
  }

  time = proc.time() - ptm
  mins = round(time[3] / 60, digits = 2)
  #return(cat(paste("\nrNewhall visualization complete for study area. \nTotal Time: ",mins, " mins\n", sep = "")))
  cat(paste("\nrNewhall visualization complete for study area. \nTotal Time: ",mins, " mins\n", sep = ""))

}

