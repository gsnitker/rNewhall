# Calculate measured soil moisture data for comparaison
#measured.SM = subset(measured.SM , year(measured.SM$Date) == 2010)

met.data.sub$TR05[met.data.sub$TR05 == -996] = NA
met.data.sub$TR25[met.data.sub$TR25 == -996] = NA
met.data.sub$TR60[met.data.sub$TR60 ==  -996] = NA
met.data.sub$TR75[met.data.sub$TR75 ==  -996] = NA


# Create the variables needed to calculate soil hydraulic variables
convert.to.vol.SM = function(date, depth){
  data = subset(met.data.sub, DATE == date)# subset mesonet data to specific date
  prop = subset(soil.data.sub, Depth == as.numeric(depth))
  #Revised Schneider et al. equation
  MP = -0.119 * exp(2.861 * data[, paste( "TR" ,depth, sep = "")]) # matric potential (kPa)
  #Original Schneider et al. equation
  #MP = -0.717 * exp(1.788 * data[, paste( "TR" ,depth, sep = "")]) # matric potential (kPa)
  VWC = prop$Theta_r + (prop$Theta_s - prop$Theta_r) / (1 + (-prop$Alpha * MP)^prop$N)^(1-1./prop$N) #volumetric water content, cm^3/cm^3
  return(VWC)
}


results = data.frame()

for (i in list("05", "25", "60", "75")){ 
  results.temp = mapply(convert.to.vol.SM, date = date.range, depth = list(i))
  results = rbind(results.temp, results)
}

# clean up results
Vol_SM = as.data.frame(t(results))
Vol_SM = cbind(Vol_SM, as.Date(date.range))
Vol_SM = Vol_SM[,order(ncol(Vol_SM):1)]
colnames(Vol_SM) = c("Date", "SM05", "SM25", "SM60", "SM75")
row.names(Vol_SM) = c()
measured.SM = Vol_SM

if (output.type == "VWC"){
  measured.SM = measured.SM
}


if (output.type == "PAW"){
  measured.SM$SM05[measured.SM$SM05 <= WC.15.mesonet[1]/10 ] = WC.15.mesonet[1]/10
  measured.SM$SM05[measured.SM$SM05 >= WC.033.mesonet[1]/10 ] = WC.033.mesonet[1]/10
  measured.SM$SM25[measured.SM$SM25 <= WC.15.mesonet[2]/10 ] = WC.15.mesonet[2]/10
  measured.SM$SM25[measured.SM$SM25 >= WC.033.mesonet[2]/10 ] = WC.033.mesonet[2]/10
  measured.SM$SM60[measured.SM$SM60 <= WC.15.mesonet[4]/10 ] = WC.15.mesonet[4]/10
  measured.SM$SM60[measured.SM$SM60 >= WC.033.mesonet[4]/10 ] = WC.033.mesonet[4]/10
  measured.SM$SM75[measured.SM$SM75 <= WC.15.mesonet[5]/10 ] = WC.15.mesonet[5]/10
  measured.SM$SM75[measured.SM$SM75 >= WC.033.mesonet[5]/10 ] = WC.033.mesonet[5]/10
}
