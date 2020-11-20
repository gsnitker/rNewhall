# Functions defined for operating rNewhall over a 1 year period

#####################
define.location = function(cell.num){
#define lat and long data
  focal.cell = study.area.cells[study.area.cells$cell == cell.num,]
centroid = rgeos::gCentroid(focal.cell,byid=TRUE) # need to change [1,] to [x,]  or some otehr variable for the daily rNewhall function
lat <<- centroid@coords[1,2]
long <<- centroid@coords[1,1]
}
#####################
#####################

#####################
create.interception = function(){
  # apply Aston (1979) regression equation to calculate how much prcp is being intercepted due to vegetation (note this is a very generalized equation, but based on empirical study)
  interception.rate.spinup <<- MODIS.spinup$LAI * .0653 
  interception.rate.model <<- MODIS.model$LAI * .0653
}
#####################
#####################

#####################
create.kc = function(){
  kc.spinup = MODIS.spinup$ET/MODIS.spinup$PET
  kc.model = MODIS.model$ET/MODIS.model$PET
  # return Kc values
  kc.spinup <<-kc.spinup[!is.na(kc.spinup)]
  kc.model <<-kc.model[!is.na(kc.model)]
}
#####################
#####################

#####################
# create the accrestion and depletion requirements for each mm for each cell
create.accretion.depletion = function(soil.input){
  # create Ksat values for calculating depletion cost
  # note saturation clumn purposely left out isnce it has no cost to remove water from
  K.all = c()
  for (i in 1:num.rows){
    vwc = c()
    for (g in 1:(num.col +2)){
      vwc.raw = soil.input$ret[i] + sum(mm.matrix.max[i,1:g])
      vwc = append(vwc, vwc.raw) 
    }
    Se = (vwc-soil.input$ret[i])/(soil.input$sat[i]-soil.input$ret[i]) # effective saturation, unitless
    K = soil.input$ksat[i]*(Se^soil.input$l[i])*(1-(1-(Se^(1/(1-(1/soil.input$n[i])))))^(1-(1/soil.input$n[i])))^2 # hydraulic conductivity, cm/d from van Genuchten (1980)
    K.all = append(K.all, K)
  }
  K.all= as.vector(K.all)
  K.all[is.nan(K.all)] <- 0# remove NaN from mm.max. These are casued by shallow soil depth
  K.matrix = matrix(K.all, nrow = num.rows, ncol = (num.col +2), byrow = T)
  #K.matrix
  
  K.rev = c()
  for (q in 1:num.rows){
    K.rev.raw = (1-K.matrix[q,]) * soil.input$perc[q] * k.scale
    K.rev = append(K.rev, K.rev.raw )
  }
  K.rev= as.vector(K.rev)
  K.rev[is.nan(K.rev)] <- 0# remove NaN from mm.max. These are casued by shallow soil depth
  K.rev.matrix= matrix(K.rev, nrow = num.rows, ncol = (num.col +2), byrow = T)
  K.rev.matrix = cbind(K.rev.matrix[,1:(num.col +1)],c(rep(sat.column.ksat, num.rows))) # add sat column back in with the same value as FC
  #K.rev.matrix
  # # creation depletion requires
  root.zone.req = (0-log(10:1)+(log(10)+1)) # inversed log with depth from 1 to 3.3
  root.zone.req  = rescale(root.zone.req, c(rootzone.low,rootzone.high))
  K.rev.matrix* root.zone.req
  depletion.req = as.vector(t(K.rev.matrix* root.zone.req))
  
  # # Depletion order - create "slants" or diagonals per original Newhall 
  depletion.order = c()
  ordered.matrix = matrix(1:(num.rows*(num.col +2)),nrow = num.rows,ncol =  (num.col +2), byrow = T)
  ordered.matrix = ordered.matrix[,-(num.col +2)]
  for (i in num.rows:-num.rows){
    add = odiag(ordered.matrix, i)
    depletion.order = c(depletion.order, add)
  }
  depletion.order = c(seq(12,length(mm.max), by = 12),depletion.order)
  
  # create accretion order
  accretion.order <<- c(1:(num.rows  * (num.col +2)))
  
  # final outputs
  accretion.order <<- accretion.order
  depletion.order <<- depletion.order
  depletion.req <<- depletion.req

}
#####################
#####################


#####################
create.mm.soil.attr = function(cell.num){ 
  soils = subset(study.area.soils, cell == cell.num)
  # not that the units here are in cm to tie tehm into the mm, hence the multiplication by row.depth
  soils$aws = (soils$fc - soils$pwp) * row.depth
  soils$pwp = soils$pwp * row.depth
  soils$fc = soils$fc * row.depth
  # create coumins fro remaining values, then fill with a for loop
  soils$sat = NA
  soils$ret = NA
  soils$ksat = NA
  soils$l = NA
  soils$n = NA
  soils$perc = NA
  for (i in 1:num.rows) {
    out = rosetta.approx(soils$sand[i], soils$silt[i], soils$clay[i])
    soils$sat[i] = out$`theta_sat [cm3/cm3]` * row.depth
    soils$ret[i] = out$`Theta_res [cm3/cm3]` * row.depth
    soils$ksat[i] = out$`Ksat [cm/day]`
    soils$l[i] = out$L 
    soils$n[i] = out$n
    soils$perc[i] = out$`Rel. Percolation`
  }
  # create moisture matrix mm
  mm.max = c()
  for (m in 1:num.rows){
    mm.raw = c((soils$pwp[m] - soils$ret[m]), rep((soils$aws[m]/num.col),  times = num.col), (soils$sat[m] - soils$fc[m])) 
    mm.max = append(mm.max, mm.raw)
  }
  
  mm.max = as.vector(mm.max)
  mm.max[is.nan(mm.max)] <- 0# remove NaN from mm.max. These are casued by shallow soil depth
  mm.matrix.max = matrix(mm.max, nrow = num.rows, ncol = (num.rows + 2), byrow = T)
  
  # final outputs
  soils <<- soils
  mm.max <<- mm.max
  mm.matrix.max <<- matrix(mm.max, nrow = num.rows, ncol = (num.rows + 2), byrow = T)
}
#####################
#####################


#####################
# subset met data for each cell
subset.met.data = function(cell.num){ 
  prcp.all = PRISM.prcp[,c('date',cell.num)]
  prcp.spinup <-  subset(prcp.all, date < as.Date(paste(date.years,"-1-1", sep = "")))
  prcp.model <-  subset(prcp.all, date >= as.Date(paste(date.years,"-1-1", sep = "")))
 
  tmin.all = PRISM.tmin[,c('date',cell.num)]
  tmin.spinup <-  subset(tmin.all, date < as.Date(paste(date.years,"-1-1", sep = "")))
  tmin.model <-  subset(tmin.all, date >= as.Date(paste(date.years,"-1-1", sep = "")))

  tmax.all = PRISM.tmax[,c('date',cell.num)]
  tmax.spinup <-  subset(tmax.all, date < as.Date(paste(date.years,"-1-1", sep = "")))
  tmax.model <-  subset(tmax.all, date >= as.Date(paste(date.years,"-1-1", sep = "")))
  
  met.data.spinup =  prcp.spinup %>% full_join(tmin.spinup, by = 'date') %>% full_join(tmax.spinup, by = 'date') 
  colnames(met.data.spinup) = c("date", "prcp", "tmin", "tmax")
  met.data.spinup$month = month(met.data.spinup$date)
  
  met.data.model =  prcp.model %>% full_join(tmin.model, by = 'date') %>% full_join(tmax.model, by = 'date') 
  colnames(met.data.model) = c("date", "prcp", "tmin", "tmax")
  met.data.model$month = month(met.data.model$date)
  
  met.data.monthly.raw = cbind(aggregate(met.data.model, by=list(met.data.model$month),FUN=mean, na.rm=TRUE), aggregate(met.data.spinup, by=list(met.data.spinup$month),FUN=mean, na.rm=TRUE))
  
  met.data.monthly = data.frame("spin-up.monthly" = (met.data.monthly.raw[,10] + met.data.monthly.raw[,11])/2, "model.monthly" = (met.data.monthly.raw[,4] + met.data.monthly.raw[,5])/2, "month" = met.data.monthly.raw[,1])
  
  met.data.monthly <<- met.data.monthly
  met.data.spinup <<- met.data.spinup
  met.data.model <<- met.data.model
  }
#####################
#####################


#####################
# subset modis for each cell
subset.MODIS.data = function(cell.num){ 
  MODIS.ET.all = MODIS.ET[,c('date',cell.num)]
  MODIS.ET.spinup <-  subset(MODIS.ET.all, date < as.Date(paste(date.years,"-1-1", sep = "")))
  MODIS.ET.model <-  subset(MODIS.ET.all, date >= as.Date(paste(date.years,"-1-1", sep = "")))
  
  MODIS.PET.all = MODIS.PET[,c('date',cell.num)]
  MODIS.PET.spinup <-  subset(MODIS.PET.all, date < as.Date(paste(date.years,"-1-1", sep = "")))
  MODIS.PET.model <-  subset(MODIS.PET.all, date >= as.Date(paste(date.years,"-1-1", sep = "")))
  
  MODIS.LAI.all = MODIS.LAI[,c('date',cell.num)]
  MODIS.LAI.spinup <-  subset(MODIS.LAI.all, date < as.Date(paste(date.years,"-1-1", sep = "")))
  MODIS.LAI.model <-  subset(MODIS.LAI.all, date >= as.Date(paste(date.years,"-1-1", sep = "")))
  
  MODIS.spinup =  MODIS.ET.spinup %>% full_join(MODIS.PET.spinup, by = 'date') %>% full_join(MODIS.LAI.spinup, by = 'date') 
  colnames(MODIS.spinup) = c("date", "ET", "PET", "LAI")
  
  MODIS.model =  MODIS.ET.model %>% full_join(MODIS.PET.model, by = 'date') %>% full_join(MODIS.LAI.model, by = 'date') 
  colnames(MODIS.model) = c("date", "ET", "PET", "LAI")
  
  MODIS.spinup <<-MODIS.spinup
  MODIS.model <<- MODIS.model
}
#####################
#####################

#####################
# Updated daily Thorthwaite
Thornthwaite.Daily =  function(d, lat, tmax, tmin){
  # calculate potential evapotranspiration using Thornthwaite (1948) equation, modified for daily estimations
  month = month(d)
  day.hours = daylength(lat, d)
  temp.ef =(3*tmax - tmin)* 0.5 * 0.69 # this is the temp effective calculation via Camargo et al. (1999) a corrcetion of 0.72 can also be used, but other author have suggested 0.69 is better (see Pereira and Pruitt 2004)
  temp = (tmax + tmin)/2#*(day.hours/(24-day.hours))
  #temp = tmin
  I.m = (met.data.monthly[,type.index]/5)^1.514# need to account for negative temperatures
  I.m[is.nan(I.m)] <- 0
  I = sum(I.m) #Note that this value is calculated for the whole year, but then used in each monthly calculation
  a = (6.75*(10^-7)*(I^3)) - (7.71*(10^-5)*(I^2)) + (1.792*(10^-2)*I) + 0.49239
  if (temp > 0 &  temp < 26.5){
    raw.PE = 16 * (10 * temp / I)^a
  } else if (temp <= 0) { # If temp is over 38C, the PE value is fixed at 185.0mm
    raw.PE = 0
  } else {
    raw.PE = -415.85 + (32.24 * temp) - (0.43 * temp^2) #Use look up tables (temp.bins and pe.bins to assign a pe based on temperature)
  }
  # Adjust raw.PE using the K correction factor for daylight hours based on latitude and numbe of days per year (from SPEI Thornthwaite)
  tanLat <- tan(lat/57.2957795)
  tanDelta <- c(-0.37012566, -0.23853358, -0.04679872, 0.16321764,
                0.32930908, 0.40677729, 0.3747741, 0.239063, 0.04044485,
                -0.16905776, -0.33306377, -0.40743608)
  tanLatM <- matrix(tanLat, nrow = 12, ncol = length(tanLat),
                    byrow = TRUE) * tanDelta
  tanLatM[tanLatM < {
    -1
  }] <- -1
  tanLatM[tanLatM > 1] <- 1
  omega <- acos(-tanLatM)
  k = omega[month]
  # Adjusted to account for the number of daylight hours fpr that day - correction factor l
  month.days = seq((floor_date(d, unit = "month")),(ceiling_date(d, unit = "month") - 1), "days")
  month.hours = daylength(lat, month.days)
  l = day.hours / sum(month.hours)
  
  # final cacluation of PE
  PE = (raw.PE * k * l) /10 # convert from mm to cm per day 
  return(PE)
}
#####################
#####################


#####################
# rNewhall.model.run
rNewhall.model.run =  function(model.date, run.type){ # run.type = "spin-up" or "model"
  if (run.type == "spin-up"){
    met.data = met.data.spinup
    interception.rate = interception.rate.spinup
    type.index <<- 1
    kc = kc.spinup
    #extract current day's met data
    prcp.range<-subset(met.data, date <= model.date & date > (model.date- soil.memory.days))
    
    # 2.1.1 create an avg daily precipitation
    prcp = mean(prcp.range$prcp) * (1 - interception.rate[yday(model.date)]) # current apporximation for vegetation to intercep prcp
    
    # 2.1.2 calculate potential evapotranspiration using Thornthwaite (1948) equation
    current.met<<-subset(met.data, date == model.date)
  
    # 2.1.3 # multiple by FAO58 crop coefficient
    if (ET.type == "Thornthwaite") {
      PE = Thornthwaite.Daily(model.date, lat, current.met$tmax, current.met$tmin)
      ET = PE * (kc[yday(model.date)])  * ET.multiplier #maybe need to scale this to three days as well
    } 
    if (ET.type == "MODIS") {
      sub =  subset(MODIS.ET.filled, DATE == model.date)#maybe need to scale this to three days as well
      ET = sub$ET/10
    } 
    # if (ET.type == "Measured") {
    #   sub =  subset(MARE.Measured.ET, DATE == d)#maybe need to scale this to three days as well
    #   ET = sub$ET/10 
    # }
    # 2.1.4 Calculate effect of PE; called Net moisture activity (NMA) in Newhall and Berdanier (1996) and net potential
    # evapotranspiration (NPE) in Van Wambeke (2000). If NPE > 0, accretion will take place during this period; otherwise, 
    # water will be extracted from the profile
    # NMA = (prcp - ET)
    # 
    # # 2.2 Model changes in water content during each day (both accretion and depletion)
    #Add Water
    current.PRCP = prcp
    for (i in 1:length(accretion.order)){
      accretion.pos = accretion.order[i]
      m.in.slot = mm[accretion.pos]
      m.slot = mm.max[accretion.pos]
      m.to.fill.slot = m.slot - (m.in.slot)
      if ( current.PRCP > m.to.fill.slot) {
        mm[accretion.pos] = m.in.slot + m.to.fill.slot
        current.PRCP =  current.PRCP - m.to.fill.slot 
      } else {
        mm[accretion.pos] = m.in.slot + current.PRCP
        current.PRCP =  current.PRCP - current.PRCP
      }
      if ( current.PRCP == 0) break # if there is no more water from preip, stop
      if (sum(mm) == sum(mm.max)) break # if the soil profile is full, stop
    }
    
    #Remove water
    current.ET = ET
    for (i in 1:length(depletion.order)){
      depletion.pos = depletion.order[i]
      m.in.slot = mm[depletion.pos]
      energy.to.drain.slot = m.in.slot * depletion.req[depletion.pos]
      if (mm[depletion.pos] == 0) next
      if (current.ET > energy.to.drain.slot) {
        mm[depletion.pos] = 0
        current.ET = current.ET - energy.to.drain.slot
      } else {
        remaining.energy.proportion = current.ET / energy.to.drain.slot
        mm[depletion.pos] = m.in.slot - (remaining.energy.proportion * m.in.slot)
        current.ET = current.ET - current.ET
      }
      if (current.ET == 0) break
      if (sum(mm) == 0) break
    }
    
    
    # 2.3 Evaluate the condition of the mm after adding and subtracting water
    mm.matrix.val = matrix(mm, nrow = num.rows, ncol = (num.col + 2), byrow = T)
    
    # 2.4 Move water down trhough the profile each day. Starting with 10%  moving down from each layer every day
    # 1. calculate what 10% of teh given row is
    # 2. Move that water down the profile
    # 3. Subtract that water from the original row
    for (i in num.rows:1){
      #grav.water = (sum(mm.matrix.max[i, ]) - sum(mm.matrix.val[i, ])) * per.infil[i]  # amount of water to move down the profile with infiltration
      grav.water = sum(mm.matrix.val[i, ]) * (percolation.percent * soils$perc[i])  # amount of water to move down the profile with infiltration
      # vwc = WC.ret[i] + sum(mm.matrix.val[i,])
      # Se = (vwc-WC.ret[i])/(WC.sat[i]-WC.ret[i]) # effective saturation, unitless
      # grav.water = Ksat[i]*(Se^L[i])*(1-(1-(Se^(1/(1-(1/N[i])))))^(1-(1/N[i])))^2 # hydraulic conductivity, cm/d from van Genuchten (1980)
      grav.order = accretion.order[-(1:(i * row.depth))]# modifiy the accretion order to only fill the areas below the current row with water
      if (length(grav.order) > 10){ 
        for (j in 1:length(grav.order)){
          grav.pos = grav.order[j]
          m.in.slot = mm[grav.pos]
          m.slot = mm.max[grav.pos]
          m.to.fill.slot = m.slot - m.in.slot
          if (grav.water > m.to.fill.slot) {
            mm[grav.pos] = m.in.slot + m.to.fill.slot
            grav.water =  grav.water - m.to.fill.slot 
          } else {
            mm[grav.pos] = m.in.slot + grav.water
            grav.water =  grav.water - grav.water
          }
          mm.matrix.val = matrix(mm, nrow = num.rows, ncol = (num.col + 2), byrow = T)
          if (grav.water== 0) break 
          if (sum(mm) == sum(mm.max)) break}}} 
    
    
    for (i in num.rows:1){
      grav.water = sum(mm.matrix.val[i, ]) * (percolation.percent * soils$perc[i])  # amount of water to move down the profile with infiltration
      #grav.water = sum(mm.matrix.val[i, ]) * per.infil[i] # amount of water to move down the profile with infiltration
      # vwc = WC.ret[i] + sum(mm.matrix.val[i,])
      # Se = (vwc-WC.ret[i])/(WC.sat[i]-WC.ret[i]) # effective saturation, unitless
      # grav.water = Ksat[i]*(Se^L[i])*(1-(1-(Se^(1/(1-(1/N[i])))))^(1-(1/N[i])))^2 # hydraulic conductivity, cm/d from van Genuchten (1980)
      remove.order  = (i * (row.depth + 1)):((i * (row.depth + 1)) - 10)
      for (j in 1:length(remove.order)){
        remove.pos = remove.order[j]
        m.in.slot = mm[remove.pos]
        energy.to.drain.slot = m.in.slot * 1
        if (mm[remove.pos] == 0) next
        if (grav.water > energy.to.drain.slot) {
          mm[remove.pos] = 0
          grav.water = grav.water - energy.to.drain.slot
        } else {
          mm[remove.pos] = m.in.slot - grav.water
          grav.water = grav.water - grav.water
        }
        mm.matrix.val = matrix(mm, nrow = num.rows, ncol = (num.col + 2), byrow = T)
        if (grav.water == 0) break
        if (sum(mm) == 0) break
      }
    } 
    
    # 2.6 Carry over moisture matrix to the next day
    mm <<- mm
    
    } else if (run.type == "model"){
    met.data = met.data.model
    interception.rate = interception.rate.model
    type.index <<- 2
    kc = kc.model
    # Create a s4 class for rNewhall output, must be done here for it to work in parallel
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
    
    
    #extract current day's met data
    prcp.range<-subset(met.data, date <= model.date & date > (model.date- soil.memory.days))
    
    # 2.1.1 create an avg daily precipitation
    prcp = mean(prcp.range$prcp) * (1 - interception.rate[yday(model.date)]) # current apporximation for vegetation to intercep prcp
    
    # 2.1.2 calculate potential evapotranspiration using Thornthwaite (1948) equation
    current.met<<-subset(met.data, date == model.date)
    #  
    # PE = mean(PE.list)
    PE = Thornthwaite.Daily(model.date, lat, current.met$tmax, current.met$tmin)
    
    # 2.1.3 # multiple by FAO58 crop coefficient
    if (ET.type == "Thornthwaite") {
      ET = PE * (kc[yday(model.date)])  * ET.multiplier #maybe need to scale this to three days as well
    } 
    if (ET.type == "MODIS") {
      sub =  subset(MODIS.ET.filled, DATE == model.date)#maybe need to scale this to three days as well
      ET = sub$ET/10
    } 
    # if (ET.type == "Measured") {
    #   sub =  subset(MARE.Measured.ET, DATE == d)#maybe need to scale this to three days as well
    #   ET = sub$ET/10 
    # }
    # 2.1.4 Calculate effect of PE; called Net moisture activity (NMA) in Newhall and Berdanier (1996) and net potential
    # evapotranspiration (NPE) in Van Wambeke (2000). If NPE > 0, accretion will take place during this period; otherwise, 
    # water will be extracted from the profile
    # NMA = (prcp - ET)
    # 
    # # 2.2 Model changes in water content during each day (both accretion and depletion)
    #Add Water
    current.PRCP = prcp
    for (i in 1:length(accretion.order)){
      accretion.pos = accretion.order[i]
      m.in.slot = mm[accretion.pos]
      m.slot = mm.max[accretion.pos]
      m.to.fill.slot = m.slot - (m.in.slot)
      if ( current.PRCP > m.to.fill.slot) {
        mm[accretion.pos] = m.in.slot + m.to.fill.slot
        current.PRCP =  current.PRCP - m.to.fill.slot 
      } else {
        mm[accretion.pos] = m.in.slot + current.PRCP
        current.PRCP =  current.PRCP - current.PRCP
      }
      if ( current.PRCP == 0) break # if there is no more water from preip, stop
      if (sum(mm) == sum(mm.max)) break # if the soil profile is full, stop
    }
    
    #Remove water
    current.ET = ET
    for (i in 1:length(depletion.order)){
      depletion.pos = depletion.order[i]
      m.in.slot = mm[depletion.pos]
      energy.to.drain.slot = m.in.slot * depletion.req[depletion.pos]
      if (mm[depletion.pos] == 0) next
      if (current.ET > energy.to.drain.slot) {
        mm[depletion.pos] = 0
        current.ET = current.ET - energy.to.drain.slot
      } else {
        remaining.energy.proportion = current.ET / energy.to.drain.slot
        mm[depletion.pos] = m.in.slot - (remaining.energy.proportion * m.in.slot)
        current.ET = current.ET - current.ET
      }
      if (current.ET == 0) break
      if (sum(mm) == 0) break
    }
    
    
    # 2.3 Evaluate the condition of the mm after adding and subtracting water
    mm.matrix.val = matrix(mm, nrow = num.rows, ncol = (num.col + 2), byrow = T)
    
    # 2.4 Move water down trhough the profile each day. Starting with 10%  moving down from each layer every day
    # 1. calculate what 10% of teh given row is
    # 2. Move that water down the profile
    # 3. Subtract that water from the original row
    for (i in num.rows:1){
      #grav.water = (sum(mm.matrix.max[i, ]) - sum(mm.matrix.val[i, ])) * per.infil[i]  # amount of water to move down the profile with infiltration
      grav.water = sum(mm.matrix.val[i, ]) * (percolation.percent * soils$perc[i])  # amount of water to move down the profile with infiltration
      # vwc = WC.ret[i] + sum(mm.matrix.val[i,])
      # Se = (vwc-WC.ret[i])/(WC.sat[i]-WC.ret[i]) # effective saturation, unitless
      # grav.water = Ksat[i]*(Se^L[i])*(1-(1-(Se^(1/(1-(1/N[i])))))^(1-(1/N[i])))^2 # hydraulic conductivity, cm/d from van Genuchten (1980)
      grav.order = accretion.order[-(1:(i * row.depth))]# modifiy the accretion order to only fill the areas below the current row with water
      if (length(grav.order) > 10){ 
        for (j in 1:length(grav.order)){
          grav.pos = grav.order[j]
          m.in.slot = mm[grav.pos]
          m.slot = mm.max[grav.pos]
          m.to.fill.slot = m.slot - m.in.slot
          if (grav.water > m.to.fill.slot) {
            mm[grav.pos] = m.in.slot + m.to.fill.slot
            grav.water =  grav.water - m.to.fill.slot 
          } else {
            mm[grav.pos] = m.in.slot + grav.water
            grav.water =  grav.water - grav.water
          }
          mm.matrix.val = matrix(mm, nrow = num.rows, ncol = (num.col + 2), byrow = T)
          if (grav.water== 0) break 
          if (sum(mm) == sum(mm.max)) break}}} 
    
    
    for (i in num.rows:1){
      grav.water = sum(mm.matrix.val[i, ]) * (percolation.percent * soils$perc[i])  # amount of water to move down the profile with infiltration
      #grav.water = sum(mm.matrix.val[i, ]) * per.infil[i] # amount of water to move down the profile with infiltration
      # vwc = WC.ret[i] + sum(mm.matrix.val[i,])
      # Se = (vwc-WC.ret[i])/(WC.sat[i]-WC.ret[i]) # effective saturation, unitless
      # grav.water = Ksat[i]*(Se^L[i])*(1-(1-(Se^(1/(1-(1/N[i])))))^(1-(1/N[i])))^2 # hydraulic conductivity, cm/d from van Genuchten (1980)
      remove.order  = (i * (row.depth + 1)):((i * (row.depth + 1)) - 10)
      for (j in 1:length(remove.order)){
        remove.pos = remove.order[j]
        m.in.slot = mm[remove.pos]
        energy.to.drain.slot = m.in.slot * 1
        if (mm[remove.pos] == 0) next
        if (grav.water > energy.to.drain.slot) {
          mm[remove.pos] = 0
          grav.water = grav.water - energy.to.drain.slot
        } else {
          mm[remove.pos] = m.in.slot - grav.water
          grav.water = grav.water - grav.water
        }
        mm.matrix.val = matrix(mm, nrow = num.rows, ncol = (num.col + 2), byrow = T)
        if (grav.water == 0) break
        if (sum(mm) == 0) break
      }
    } 
    
    
    # 2.5 Assess the mositure in specified row  and prepare outputs
    if (output.type == "VWC"){
      moisture.0.10 = ((sum(mm.matrix.val[1,])/sum(mm.matrix.max[1,1:(num.col + 2)])) * (soils$sat[1]/10 - soils$ret[1]/10)) + (soils$ret[1]/10) # proportion of water in profile / amount of water taht could be in teh profile + % of water that is excluded form profile (less tahn wilting point).
      moisture.10.20 = ((sum(mm.matrix.val[2,])/sum(mm.matrix.max[2,1:(num.col + 2)])) * (soils$sat[2]/10- soils$ret[2]/10)) + (soils$ret[2]/10)
      moisture.20.30 = ((sum(mm.matrix.val[3,])/sum(mm.matrix.max[3,1:(num.col + 2)])) * (soils$sat[3]/10 - soils$ret[3]/10 )) + (soils$ret[3]/10)
      moisture.30.40 =  ((sum(mm.matrix.val[4,])/sum(mm.matrix.max[4,1:(num.col + 2)])) * (soils$sat[4]/10 - soils$ret[4]/10)) + (soils$ret[4]/10)
      moisture.40.50 =  ((sum(mm.matrix.val[5,])/sum(mm.matrix.max[5,1:(num.col + 2)])) * (soils$sat[5]/10- soils$ret[5]/10)) + (soils$ret[5]/10)
      moisture.50.60 =  ((sum(mm.matrix.val[6,])/sum(mm.matrix.max[6,1:(num.col + 2)])) * (soils$sat[6]/10- soils$ret[6]/10)) + (soils$ret[6]/10)
      moisture.60.70 =  ((sum(mm.matrix.val[7,])/sum(mm.matrix.max[7,1:(num.col + 2)])) * (soils$sat[7]/10- soils$ret[7]/10)) + (soils$ret[7]/10)
      moisture.70.80 =  ((sum(mm.matrix.val[8,])/sum(mm.matrix.max[8,1:(num.col + 2)])) * (soils$sat[8]/10 - soils$ret[8]/10)) + (soils$ret[8]/10)
      moisture.80.90 =  ((sum(mm.matrix.val[9,])/sum(mm.matrix.max[9,1:(num.col + 2)])) * (soils$sat[9]/10 - soils$ret[9]/10)) + (soils$ret[9]/10)
      moisture.90.100 =  ((sum(mm.matrix.val[10,])/sum(mm.matrix.max[10,1:(num.col + 2)])) * (soils$sat[10]/10 - soils$ret[10]/10)) + (soils$ret[10]/10)
    }
    
    if (output.type == "PAW"){
      moisture.0.10 = ((sum(mm.matrix.val[1,])/sum(mm.matrix.max[1,1:(num.col + 2)])) * (soils$sat[1]/10 - soils$ret[1]/10)) + (soils$ret[1]/10)
      moisture.10.20 = ((sum(mm.matrix.val[2,])/sum(mm.matrix.max[2,1:(num.col + 2)])) * (soils$sat[2]/10- soils$ret[2]/10)) + (soils$ret[2]/10)
      moisture.20.30 = ((sum(mm.matrix.val[3,])/sum(mm.matrix.max[3,1:(num.col + 2)])) * (soils$sat[3]/10 - soils$ret[3]/10 )) + (soils$ret[3]/10)
      moisture.30.40 =  ((sum(mm.matrix.val[4,])/sum(mm.matrix.max[4,1:(num.col + 2)])) * (soils$sat[4]/10 - soils$ret[4]/10)) + (soils$ret[4]/10)
      moisture.40.50 =  ((sum(mm.matrix.val[5,])/sum(mm.matrix.max[5,1:(num.col + 2)])) * (soils$sat[5]/10- soils$ret[5]/10)) + (soils$ret[5]/10)
      moisture.50.60 =  ((sum(mm.matrix.val[6,])/sum(mm.matrix.max[6,1:(num.col + 2)])) * (soils$sat[6]/10- soils$ret[6]/10)) + (soils$ret[6]/10)
      moisture.60.70 =  ((sum(mm.matrix.val[7,])/sum(mm.matrix.max[7,1:(num.col + 2)])) * (soils$sat[7]/10- soils$ret[7]/10)) + (soils$ret[7]/10)
      moisture.70.80 =  ((sum(mm.matrix.val[8,])/sum(mm.matrix.max[8,1:(num.col + 2)])) * (soils$sat[8]/10 - soils$ret[8]/10)) + (soils$ret[8]/10)
      moisture.80.90 =  ((sum(mm.matrix.val[9,])/sum(mm.matrix.max[9,1:(num.col + 2)])) * (soils$sat[9]/10 - soils$ret[9]/10)) + (soils$ret[9]/10)
      moisture.90.100 =  ((sum(mm.matrix.val[10,])/sum(mm.matrix.max[10,1:(num.col + 2)])) * (soils$sat[10]/10 - soils$ret[10]/10)) + (soils$ret[10]/10)
      
      moisture.0.10[moisture.0.10 <= soils$pwp[1]/10 ] = soils$pwp[1]/10
      moisture.0.10[moisture.0.10 >= soils$fc[1]/10 ] = soils$fc[1]/10
      moisture.10.20[moisture.10.20 <= soils$pwp[2]/10 ] = soils$pwp[2]/10
      moisture.10.20[moisture.10.20 >= soils$fc[2]/10 ] = soils$fc[2]/10
      moisture.20.30[moisture.20.30 <= soils$pwp[3]/10 ] = soils$pwp[3]/10
      moisture.20.30[moisture.20.30 >= soils$fc[3]/10 ] = soils$fc[3]/10
      moisture.30.40[moisture.30.40 <= soils$pwp[4]/10 ] = soils$pwp[4]/10
      moisture.30.40[moisture.30.40 >= soils$fc[4]/10 ] = soils$fc[4]/10
      moisture.40.50[moisture.40.50 <= soils$pwp[5]/10 ] = soils$pwp[5]/10
      moisture.40.50[moisture.40.50 >= soils$fc[5]/10 ] = soils$fc[5]/10
      moisture.50.60[moisture.50.60 <= soils$pwp[6]/10 ] = soils$pwp[6]/10
      moisture.50.60[moisture.50.60 >= soils$fc[6]/10 ] = soils$fc[6]/10
      moisture.60.70[moisture.60.70 <= soils$pwp[7]/10 ] = soils$pwp[7]/10
      moisture.60.70[moisture.60.70 >= soils$fc[7]/10 ] = soils$fc[7]/10
      moisture.70.80[moisture.70.80 <= soils$pwp[8]/10 ] = soils$pwp[8]/10
      moisture.70.80[moisture.70.80 >= soils$fc[8]/10 ] = soils$fc[8]/10
      moisture.80.90[moisture.80.90 <= soils$pwp[9]/10 ] = soils$pwp[9]/10
      moisture.80.90[moisture.80.90 >= soils$fc[9]/10 ] = soils$fc[9]/10
      moisture.90.100[moisture.90.100 <= soils$pwp[10]/10 ] = soils$pwp[10]/10
      moisture.90.100[moisture.90.100 >= soils$fc[10]/10 ] = soils$fc[10]/10
    }
    
    
    # 2.6 Carry over moisture matrix to the next day
    mm <<- mm
    
    # 2.7 Assess the daily soil moisture condition
    return(new("rNewhall.multi.output", 
               day = model.date, 
               prcp = prcp, 
               ET = ET, 
               mm.value = mm.matrix.val,
               mm.max =  mm.matrix.max,
               moisture.0.10 = moisture.0.10,
               moisture.10.20 = moisture.10.20,
               moisture.20.30 = moisture.20.30,
               moisture.30.40 = moisture.30.40,
               moisture.40.50 = moisture.40.50,
               moisture.50.60 =  moisture.50.60,
               moisture.60.70 = moisture.60.70,
               moisture.70.80 = moisture.70.80,
               moisture.80.90 = moisture.80.90,
               moisture.90.100 = moisture.90.100))
  } else {(stop("Please select an appropriate run type for rNewhall Daily"))}
}
#####################
#####################