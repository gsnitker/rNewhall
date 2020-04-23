rNewhall.pt.daily =  function(d){
  # Create a s4 class for rNewhall output, must be done here for it to work in parallel
  setClass("gg")
  setClass("newhalloutputspatial", slots = representation(day = "Date",
                                                          prcp = "numeric",
                                                          ET = "numeric",
                                                          vol.moisture.0.10 = "numeric",
                                                          vol.moisture.10.20 = "numeric",
                                                          vol.moisture.20.30 = "numeric",
                                                          vol.moisture.30.40 = "numeric",
                                                          vol.moisture.40.50 = "numeric",
                                                          vol.moisture.50.60 = "numeric",
                                                          vol.moisture.60.70 = "numeric",
                                                          vol.moisture.70.80 = "numeric",
                                                          vol.moisture.80.90 = "numeric",
                                                          vol.moisture.90.100 = "numeric"))
  
  #extract current day's met data
  current.met<<-subset(met.data.sub, DATE == d)
  
  # 2.1.1 extract daily precipitation
  prcp = current.met$RAIN

  
  # 2.1.2 calculate potential evapotranspiration using Thornthwaite (1948) equation
  PE = Thornthwaite.Daily(d, lat)

  # 2.1.3 # multiple by FAO58 crop coefficient
  ET = PE * kc[yday(d)] #
  
  
  # 2.1.4 Calculate effect of PE; called Net moisture activity (NMA) in Newhall and Berdanier (1996) and net potential
  # evapotranspiration (NPE) in Van Wambeke (2000). If NPE > 0, accretion will take place during this period; otherwise, 
  # water will be extracted from the profile
  NMA = (prcp - ET)
  
  
  # 2.2 Model changes in water content during each day (both accretion and depletion)
  # 2.2.1 First half of the month (15 days) add or remove water from profile based on LP
  # If NPE > 0, apply NPE/2 to fill available slots; if NPE < 0, apply NPE/2 to deplete filled slots
  if (NMA > 0) {
    total.available.NMA = (NMA)
    current.NMA = NMA
    for (i in 1:length(accretion.order)){
      accretion.pos = accretion.order[i]
      m.in.slot = mm[accretion.pos]
      m.slot = mm.max[accretion.pos]
      m.to.fill.slot = m.slot - m.in.slot
      if (current.NMA > m.to.fill.slot) {
        mm[accretion.pos] = m.in.slot + m.to.fill.slot
        current.NMA =  current.NMA - m.to.fill.slot 
      } else {
        mm[accretion.pos] = m.in.slot + current.NMA
        current.NMA =  current.NMA - current.NMA
      }
      if (current.NMA == 0) break # if there is no more water from preip, stop
      if (sum(mm) == sum(mm.max)) break # if the soil profile is full, stop
    }
  }
  if (NMA < 0) {
    total.available.NMA = abs(NMA)
    current.NMA = abs(NMA)
    for (i in 1:length(depletion.order)){
      depletion.pos = depletion.order[i]
      m.in.slot = mm[depletion.pos]
      energy.to.drain.slot = m.in.slot * depletion.req[i]
      if (mm[depletion.pos] == 0) next
      if (current.NMA > energy.to.drain.slot) {
        mm[depletion.pos] = 0
        current.NMA = current.NMA - energy.to.drain.slot
      } else {
        remaining.energy.proportion = current.NMA / energy.to.drain.slot
        mm[depletion.pos] = remaining.energy.proportion * m.in.slot
        current.NMA = current.NMA - current.NMA
      }
      if (current.NMA == 0) break
      if (sum(mm) == 0) break
    }
  }
  
  # 2.3 Evaluate the condition of the mm after adding and subtracting water
  mm.matrix.val = matrix(mm, nrow = num.rows, ncol = 11, byrow = T)
  
  # 2.4 Move water down trhough the profile each day. Starting with 10%  moving down from each layer every day
  # 1. calculate what 10% of teh given row is
  # 2. Move that water down the profile
  # 3. Subtract that water from the original row
  for (i in 1:num.rows){
    grav.water = sum(mm.matrix.val[i, ]) * per.infil # amount of water to move down the profile with infiltration
    grav.order = accretion.order[-(1:(i * row.depth))]# modifiy the accretion order to only fill the areas below teh current row with water
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
        mm.matrix.val = matrix(mm, nrow = num.rows, ncol = 11, byrow = T)
        if (grav.water== 0) break 
        if (sum(mm) == sum(mm.max)) break}}} 
  
  
  for (i in 1:num.rows){
    grav.water = sum(mm.matrix.val[i, ]) * per.infil # amount of water to move down the profile with infiltration
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
      mm.matrix.val = matrix(mm, nrow = num.rows, ncol = 11, byrow = T)
      if (grav.water == 0) break
      if (sum(mm) == 0) break
    }
  } 
  

  # 2.5 Assess the mositure in specified row 
  vol.moisture.0.10 = (sum(mm.matrix.val[1,])/sum(mm.matrix.max[1,1:11])) * ((WC.sat[1] - WC.15[1]) + WC.15[1])
  vol.moisture.10.20 = (sum(mm.matrix.val[2,])/sum(mm.matrix.max[2,1:11])) * ((WC.sat[2] - WC.15[2]) + WC.15[2])
  vol.moisture.20.30 = (sum(mm.matrix.val[3,])/sum(mm.matrix.max[3,1:11])) * ((WC.sat[3] - WC.15[3]) + WC.15[3])
  vol.moisture.30.40 = (sum(mm.matrix.val[4,])/sum(mm.matrix.max[4,1:11])) * ((WC.sat[4] - WC.15[4]) + WC.15[4])
  vol.moisture.40.50 = (sum(mm.matrix.val[5,])/sum(mm.matrix.max[5,1:11])) * ((WC.sat[5] - WC.15[5]) + WC.15[5])
  vol.moisture.50.60 = (sum(mm.matrix.val[6,])/sum(mm.matrix.max[6,1:11])) * ((WC.sat[6] - WC.15[6]) + WC.15[6])
  vol.moisture.60.70 = (sum(mm.matrix.val[7,])/sum(mm.matrix.max[7,1:11])) * ((WC.sat[7] - WC.15[7]) + WC.15[7])
  vol.moisture.70.80 = (sum(mm.matrix.val[8,])/sum(mm.matrix.max[8,1:11])) * ((WC.sat[8] - WC.15[8]) + WC.15[8])
  vol.moisture.80.90 = (sum(mm.matrix.val[9,])/sum(mm.matrix.max[9,1:11])) * ((WC.sat[9] - WC.15[9]) + WC.15[9])
  vol.moisture.90.100 = (sum(mm.matrix.val[10,])/sum(mm.matrix.max[10,1:11])) * ((WC.sat[10] - WC.15[10]) + WC.15[10])
  
  # 2.6 Carry over moisture matrix to the next day
  mm <<- mm
  
  # 2.7 Assess the daily soil moisture condition
  return(new("newhalloutputspatial", day = d, prcp = prcp, ET = ET,
             vol.moisture.0.10 = vol.moisture.0.10,
             vol.moisture.10.20 = vol.moisture.10.20,
             vol.moisture.20.30 = vol.moisture.20.30,
             vol.moisture.30.40 = vol.moisture.30.40,
             vol.moisture.40.50 = vol.moisture.40.50,
             vol.moisture.50.60 =  vol.moisture.50.60,
             vol.moisture.60.70 = vol.moisture.60.70,
             vol.moisture.70.80 = vol.moisture.70.80,
             vol.moisture.80.90 = vol.moisture.80.90,
             vol.moisture.90.100 = vol.moisture.90.100))
}

rNewhall.spinup.monthly =  function(m){
  
  #extract current day's met data
  current.met.monthly = subset(met.data.prev.monthly, MONTH == m)
  
  # 2.1.1 extract daily precipitation
  prcp = ((current.met.monthly$TAVG- 32) * (5/9))
  
  
  # 2.1.2 calculate potential evapotranspiration using Thornthwaite (1948) equation
  PE = thornthwaite(((met.data.sub.monthly$TAVG- 32) * (5/9)), lat)
  
  ET = PE[m]
  # 2.1.4 Calculate effect of PE; called Net moisture activity (NMA) in Newhall and Berdanier (1996) and net potential
  # evapotranspiration (NPE) in Van Wambeke (2000). If NPE > 0, accretion will take place during this period; otherwise, 
  # water will be extracted from the profile
  NMA = (prcp - ET)
  
  
  # 2.2 Model changes in water content during each day (both accretion and depletion)
  # 2.2.1 First half of the month (15 days) add or remove water from profile based on LP
  # If NPE > 0, apply NPE/2 to fill available slots; if NPE < 0, apply NPE/2 to deplete filled slots
  if (NMA > 0) {
    total.available.NMA = (NMA)
    current.NMA = NMA
    for (i in 1:length(accretion.order)){
      accretion.pos = accretion.order[i]
      m.in.slot = mm.spinup[accretion.pos]
      m.slot = mm.max[accretion.pos]
      m.to.fill.slot = m.slot - m.in.slot
      if (current.NMA > m.to.fill.slot) {
        mm.spinup[accretion.pos] = m.in.slot + m.to.fill.slot
        current.NMA =  current.NMA - m.to.fill.slot 
      } else {
        mm.spinup[accretion.pos] = m.in.slot + current.NMA
        current.NMA =  current.NMA - current.NMA
      }
      if (current.NMA == 0) break # if there is no more water from preip, stop
      if (sum(mm.spinup) == sum(mm.max)) break # if the soil profile is full, stop
    }
  }
  if (NMA < 0) {
    total.available.NMA = abs(NMA)
    current.NMA = abs(NMA)
    for (i in 1:length(depletion.order)){
      depletion.pos = depletion.order[i]
      m.in.slot = mm.spinup[depletion.pos]
      energy.to.drain.slot = m.in.slot * depletion.req[i]
      if (mm.spinup[depletion.pos] == 0) next
      if (current.NMA > energy.to.drain.slot) {
        mm.spinup[depletion.pos] = 0
        current.NMA = current.NMA - energy.to.drain.slot
      } else {
        remaining.energy.proportion = current.NMA / energy.to.drain.slot
        mm.spinup[depletion.pos] = remaining.energy.proportion * m.in.slot
        current.NMA = current.NMA - current.NMA
      }
      if (current.NMA == 0) break
      if (sum(mm.spinup) == 0) break
    }
  }
  

  # 2.6 Carry over moisture matrix to the next month
  mm.spinup <<- mm.spinup
}

