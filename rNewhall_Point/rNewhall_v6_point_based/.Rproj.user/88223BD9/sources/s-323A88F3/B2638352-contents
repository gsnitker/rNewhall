# Updated daily Thorthwaite
Thornthwaite.Daily =  function(d, lat){
  # # 2 calculate potential evapotranspiration using Thornthwaite (1948) equation
  month = month(d)
  tmax = (current.met$TMAX - 32) * (5/9)
  tmin = (current.met$TMIN - 32) * (5/9)
  day.hours = daylength(lat, d)
  temp.ef =(3*tmax - tmin)* 0.5 * 0.69 # this is the temp effective calculation via Camargo et al. (1999) a corrcetion of 0.72 can also be used, but other author have suggested 0.69 is better (see Pereira and Pruitt 2004)
  temp = (tmax + tmin)/2#*(day.hours/(24-day.hours))
  #temp = tmin
  I.m = (((met.data.sub.monthly$TAVG- 32) * (5/9))/5)^1.514# need to account for negative temperatures
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
  PE = (raw.PE * k * l) / 10 # convert from mm to cm per day 
  return(PE)
}

