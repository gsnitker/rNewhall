rNewhall.pt.daily =  function(d){

  
  # Create a s4 class for rNewhall output, must be done here for it to work in parallel
  setClass("gg")
  setClass("newhalloutputspatial", slots = representation(day = "Date",
                                                          prcp = "numeric",
                                                          ET = "numeric",
                                                          mm.value = "matrix",
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
  prcp.range<-subset(met.data.sub, DATE <= d & DATE > (d- memory.days))
  
  # 2.1.1 extract daily precipitation
  prcp = mean(prcp.range$RAIN) * (1 - interception.rate[yday(d)]) # current apporximation for vegetation to intercep prcp


  # 2.1.2 calculate potential evapotranspiration using Thornthwaite (1948) equation
  current.met<<-subset(met.data.sub, DATE == d)
  
  # PE.list = c()
  # for (i in 0:(memory.days-1)){
  #   PE.daily = Thornthwaite.Daily((d-i), lat)
  #   PE.list = append(PE.list,  PE.daily)
  # }
  #  
  # PE = mean(PE.list)
  PE = Thornthwaite.Daily(d, lat)
  PE
  # 2.1.3 # multiple by FAO58 crop coefficient
  if (ET.type == "Thornthwaite") {
  ET = PE * (kc[yday(d)])  * ET.multiplier #maybe need to scale this to three days as well
  } 
  if (ET.type == "MODIS") {
   sub =  subset(MODIS.ET.filled, DATE == d)#maybe need to scale this to three days as well
   ET = sub$ET/10
  } 
  if (ET.type == "Measured") {
    sub =  subset(MARE.Measured.ET, DATE == d)#maybe need to scale this to three days as well
    ET = sub$ET/10 
  }
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
      # if (accretion.pos == 10 & current.PRCP > K.matrix[1,11]) { current.PRCP = K.matrix[1,11]} else { current.PRCP = current.PRCP}
      # if (accretion.pos == 20 & current.PRCP > K.matrix[2,11]) { current.PRCP = K.matrix[2,11]} else { current.PRCP = current.PRCP}
      # if (accretion.pos == 30 & current.PRCP > K.matrix[3,11]) { current.PRCP = K.matrix[3,11]} else { current.PRCP = current.PRCP}
      # if (accretion.pos == 40 & current.PRCP > K.matrix[4,11]) { current.PRCP = K.matrix[4,11]} else { current.PRCP = current.PRCP}
      # if (accretion.pos == 50 & current.PRCP > K.matrix[5,11]) { current.PRCP = K.matrix[5,11]} else { current.PRCP = current.PRCP}
      # if (accretion.pos == 60 & current.PRCP > K.matrix[6,11]) { current.PRCP = K.matrix[6,11]} else { current.PRCP = current.PRCP}
      # if (accretion.pos == 70 & current.PRCP > K.matrix[7,11]) { current.PRCP = K.matrix[7,11]} else { current.PRCP = current.PRCP}
      # if (accretion.pos == 80 & current.PRCP > K.matrix[8,11]) { current.PRCP = K.matrix[8,11]} else { current.PRCP = current.PRCP}
      # if (accretion.pos == 90 & current.PRCP > K.matrix[9,11]) { current.PRCP = K.matrix[9,11]} else { current.PRCP = current.PRCP}
      # if (accretion.pos == 100 & current.PRCP > K.matrix[10,11]) { current.PRCP = K.matrix[10,11]} else { current.PRCP = current.PRCP}
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
    grav.water = sum(mm.matrix.val[i, ]) * (percolation.percent * perc.coeff[i])  # amount of water to move down the profile with infiltration
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
    grav.water = sum(mm.matrix.val[i, ]) * (percolation.percent * perc.coeff[i])  # amount of water to move down the profile with infiltration
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
  moisture.0.10 = ((sum(mm.matrix.val[1,])/sum(mm.matrix.max[1,1:(num.col + 2)])) * (WC.sat[1]/10 - WC.ret[1]/10)) + (WC.ret[1]/10) # proportion of water in profile / amount of water taht could be in teh profile + % of water that is excluded form profile (less tahn wilting point).
  moisture.10.20 = ((sum(mm.matrix.val[2,])/sum(mm.matrix.max[2,1:(num.col + 2)])) * (WC.sat[2]/10- WC.ret[2]/10)) + (WC.ret[2]/10)
  moisture.20.30 = ((sum(mm.matrix.val[3,])/sum(mm.matrix.max[3,1:(num.col + 2)])) * (WC.sat[3]/10 - WC.ret[3]/10 )) + (WC.ret[3]/10)
  moisture.30.40 =  ((sum(mm.matrix.val[4,])/sum(mm.matrix.max[4,1:(num.col + 2)])) * (WC.sat[4]/10 - WC.ret[4]/10)) + (WC.ret[4]/10)
  moisture.40.50 =  ((sum(mm.matrix.val[5,])/sum(mm.matrix.max[5,1:(num.col + 2)])) * (WC.sat[5]/10- WC.ret[5]/10)) + (WC.ret[5]/10)
  moisture.50.60 =  ((sum(mm.matrix.val[6,])/sum(mm.matrix.max[6,1:(num.col + 2)])) * (WC.sat[6]/10- WC.ret[6]/10)) + (WC.ret[6]/10)
  moisture.60.70 =  ((sum(mm.matrix.val[7,])/sum(mm.matrix.max[7,1:(num.col + 2)])) * (WC.sat[7]/10- WC.ret[7]/10)) + (WC.ret[7]/10)
  moisture.70.80 =  ((sum(mm.matrix.val[8,])/sum(mm.matrix.max[8,1:(num.col + 2)])) * (WC.sat[8]/10 - WC.ret[8]/10)) + (WC.ret[8]/10)
  moisture.80.90 =  ((sum(mm.matrix.val[9,])/sum(mm.matrix.max[9,1:(num.col + 2)])) * (WC.sat[9]/10 - WC.ret[9]/10)) + (WC.ret[9]/10)
  moisture.90.100 =  ((sum(mm.matrix.val[10,])/sum(mm.matrix.max[10,1:(num.col + 2)])) * (WC.sat[10]/10 - WC.ret[10]/10)) + (WC.ret[10]/10)
  }

if (output.type == "PAW"){
  moisture.0.10 = ((sum(mm.matrix.val[1,])/sum(mm.matrix.max[1,1:(num.col + 2)])) * (WC.sat[1]/10 - WC.ret[1]/10)) + (WC.ret[1]/10)
  moisture.10.20 = ((sum(mm.matrix.val[2,])/sum(mm.matrix.max[2,1:(num.col + 2)])) * (WC.sat[2]/10- WC.ret[2]/10)) + (WC.ret[2]/10)
  moisture.20.30 = ((sum(mm.matrix.val[3,])/sum(mm.matrix.max[3,1:(num.col + 2)])) * (WC.sat[3]/10 - WC.ret[3]/10 )) + (WC.ret[3]/10)
  moisture.30.40 =  ((sum(mm.matrix.val[4,])/sum(mm.matrix.max[4,1:(num.col + 2)])) * (WC.sat[4]/10 - WC.ret[4]/10)) + (WC.ret[4]/10)
  moisture.40.50 =  ((sum(mm.matrix.val[5,])/sum(mm.matrix.max[5,1:(num.col + 2)])) * (WC.sat[5]/10- WC.ret[5]/10)) + (WC.ret[5]/10)
  moisture.50.60 =  ((sum(mm.matrix.val[6,])/sum(mm.matrix.max[6,1:(num.col + 2)])) * (WC.sat[6]/10- WC.ret[6]/10)) + (WC.ret[6]/10)
  moisture.60.70 =  ((sum(mm.matrix.val[7,])/sum(mm.matrix.max[7,1:(num.col + 2)])) * (WC.sat[7]/10- WC.ret[7]/10)) + (WC.ret[7]/10)
  moisture.70.80 =  ((sum(mm.matrix.val[8,])/sum(mm.matrix.max[8,1:(num.col + 2)])) * (WC.sat[8]/10 - WC.ret[8]/10)) + (WC.ret[8]/10)
  moisture.80.90 =  ((sum(mm.matrix.val[9,])/sum(mm.matrix.max[9,1:(num.col + 2)])) * (WC.sat[9]/10 - WC.ret[9]/10)) + (WC.ret[9]/10)
  moisture.90.100 =  ((sum(mm.matrix.val[10,])/sum(mm.matrix.max[10,1:(num.col + 2)])) * (WC.sat[10]/10 - WC.ret[10]/10)) + (WC.ret[10]/10)
  
  moisture.0.10[moisture.0.10 <= WC.15[1]/10 ] = WC.15[1]/10
  moisture.0.10[moisture.0.10 >= WC.033[1]/10 ] = WC.033[1]/10
  moisture.10.20[moisture.10.20 <= WC.15[2]/10 ] = WC.15[2]/10
  moisture.10.20[moisture.10.20 >= WC.033[2]/10 ] = WC.033[2]/10
  moisture.20.30[moisture.20.30 <= WC.15[3]/10 ] = WC.15[3]/10
  moisture.20.30[moisture.20.30 >= WC.033[3]/10 ] = WC.033[3]/10
  moisture.30.40[moisture.30.40 <= WC.15[4]/10 ] = WC.15[4]/10
  moisture.30.40[moisture.30.40 >= WC.033[4]/10 ] = WC.033[4]/10
  moisture.40.50[moisture.40.50 <= WC.15[5]/10 ] = WC.15[5]/10
  moisture.40.50[moisture.40.50 >= WC.033[5]/10 ] = WC.033[5]/10
  moisture.50.60[moisture.50.60 <= WC.15[6]/10 ] = WC.15[6]/10
  moisture.50.60[moisture.50.60 >= WC.033[6]/10 ] = WC.033[6]/10
  moisture.60.70[moisture.60.70 <= WC.15[7]/10 ] = WC.15[7]/10
  moisture.60.70[moisture.60.70 >= WC.033[7]/10 ] = WC.033[7]/10
  moisture.70.80[moisture.70.80 <= WC.15[8]/10 ] = WC.15[8]/10
  moisture.70.80[moisture.70.80 >= WC.033[8]/10 ] = WC.033[8]/10
  moisture.80.90[moisture.80.90 <= WC.15[9]/10 ] = WC.15[9]/10
  moisture.80.90[moisture.80.90 >= WC.033[9]/10 ] = WC.033[9]/10
  moisture.90.100[moisture.90.100 <= WC.15[10]/10 ] = WC.15[10]/10
  moisture.90.100[moisture.90.100 >= WC.033[10]/10 ] = WC.033[10]/10
}


  # 2.6 Carry over moisture matrix to the next day
  mm <<- mm
  
  # 2.7 Assess the daily soil moisture condition
  return(new("newhalloutputspatial", day = d, prcp = prcp, ET = ET, mm.value = mm.matrix.val,
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
}

