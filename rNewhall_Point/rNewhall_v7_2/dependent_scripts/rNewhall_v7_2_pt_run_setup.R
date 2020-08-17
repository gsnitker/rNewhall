options(warn=-1)
# create date range subsets for actual dataset
met.data.sub = subset(met.data,  DATE >= date.range[1] - memory.days & DATE <= date.range[length(date.range)])
# replace No data values with NAs for precip
met.data.sub$TMAX[met.data.sub$TMAX < -50] <- NA
met.data.sub$TMIN[met.data.sub$TMIN < -50] <- NA
met.data.sub$TAVG[met.data.sub$TAVG < -50] <- NA
# use linear interpolation to fill those values
met.data.sub$TMAX= na_interpolation(met.data.sub$TMAX, option = "linear", maxgap = Inf)
met.data.sub$TMIN= na_interpolation(met.data.sub$TMIN, option = "linear", maxgap = Inf)
met.data.sub$TAVG= na_interpolation(met.data.sub$TAVG, option = "linear", maxgap = Inf)
# set up clculation of monthly met values for thornthwaite
met.data.months = subset(met.data.sub,  YEAR ==  year(date.range[1]))
met.data.sub.monthly = aggregate(met.data.months, by=list(met.data.months$MONTH,met.data.months$YEAR),FUN=mean, na.rm=TRUE)

# subset modis data and create crop coefficient

MODIS.ET = subset(MODIS, calendar_date >= date.range[1] & calendar_date <= date.range[length(date.range)] & band == "ET_500m")
MODIS.PET = subset(MODIS, calendar_date >= date.range[1] & calendar_date <= date.range[length(date.range)] & band == "PET_500m")
MODIS.LAI.sub = subset(MODIS.LAI, calendar_date >= date.range[1] & calendar_date <= date.range[length(date.range)])


MODIS.ET.proc = MODIS.ET[,c("calendar_date","value")]
MODIS.ET.proc = fill(MODIS.ET.proc, value, .direction = "up")
Date.df = met.data.sub[,c("DATE","YEAR")]
colnames(MODIS.ET.proc) = c("DATE", "ET")
MODIS.ET.proc$DATE = as.Date(MODIS.ET.proc$DATE)
MODIS.ET.proc$ET = (MODIS.ET.proc$ET * 0.1)/8
MODIS.ET.temp = merge(x= Date.df, y= MODIS.ET.proc, by= 'DATE', all.x = T)
MODIS.ET.filled = fill(MODIS.ET.temp, ET)

MODIS.PET.proc = MODIS.PET[,c("calendar_date","value")]
MODIS.PET.proc = fill(MODIS.PET.proc, value, .direction = "up")
colnames(MODIS.PET.proc) = c("DATE", "PET")
MODIS.PET.proc$DATE = as.Date(MODIS.PET.proc$DATE)
MODIS.PET.proc$PET = (MODIS.PET.proc$PET * 0.1)/8
MODIS.PET.temp = merge(x= Date.df, y= MODIS.PET.proc, by= 'DATE', all.x = T)
MODIS.PET.filled = fill(MODIS.PET.temp, PET)

# plot(MODIS.PET.filled$PET, type = "l", main = "Spin-up MODIS PET and ET", xlab = "DOY", ylab = "mm/day")
# lines(MODIS.ET.filled$ET, col = "blue")
kc = MODIS.ET.filled$ET/MODIS.PET.filled$PET
kc <-kc [!is.na(kc )]
#plot(kc, type = "l")
# 
# # number of days for kc estimation 
# n.days = length(date.range) + 1
# # # created generalized model of kc frm MODIS values
# # #actual ET
# x<-yday(as.Date(MODIS.ET$calendar_date))
# y<-(MODIS.ET$value * 0.1)/8
# 
# df <- data.frame(x=x, y=y)
# tree <- rpart(y ~ x, data=df, control=rpart.control(minsplit=MODIS.phase.duration))
# 
# plot_tree <- function(tree, x, y) {
#   s <- seq(1, n.days, by=1)
#   #plot(x, y, main = "Actual ET")
#   #lines(s, predict(tree, data.frame(x=s)))
#   kc = predict(tree, data.frame(x=s))
#   return(kc)
# }
# Actual.ET = plot_tree(tree, x, y)
# #
# # created generalized model of kc frm MODIS values
# #Potential ET
# a<-yday(as.Date(MODIS.PET$calendar_date))
# b<-(MODIS.PET$value * 0.1)/8
# 
# df <- data.frame(a=a, b=b)
# tree <- rpart(b ~ a, data=df, control=rpart.control(minsplit=MODIS.phase.duration))
# 
# plot_tree <- function(tree, a, b) {
#   s <- seq(1, n.days , by=1)
#   #plot(x, y, main = "Potenial ET")
#   #lines(s, predict(tree, data.frame(x=s)))
#   kc = predict(tree, data.frame(x=s))
#   return(kc)
# }
# Potential.ET = plot_tree(tree, a, b)
# 
# 
# kc = Actual.ET /10
# kc = Actual.ET / Potential.ET
# plot(Potential.ET, type = "l")
# plot(Actual.ET, type = "l")
# plot(kc, type = "l")

#max for both is 219
# mid.duration = 55 #75
# early.season.dormant = 20 #89
# mid.ET.val = 1
# late.season.dormant = length(date.range) - mid.duration - early.season.dormant - 45 - 90 - 10
# kc = c(rep(0.20,early.season.dormant),rep(0.25, 10),seq(.25,mid.ET.val, length.out= 45), rep(mid.ET.val, mid.duration),seq(mid.ET.val,.25, length.out= 90) ,rep(0.20,late.season.dormant))
# length(kc)
#plot(kc, type = "l")

# created generalized model of interception from MODIS LAI
#LAI

MODIS.LAI.proc = MODIS.LAI.sub[,c("calendar_date","value")]
MODIS.LAI.proc = fill(MODIS.LAI.proc, value, .direction = "up")
Date.df = met.data.sub[,c("DATE","YEAR")]
colnames(MODIS.LAI.proc) = c("DATE", "LAI")
MODIS.LAI.proc$DATE = as.Date(MODIS.LAI.proc$DATE)
MODIS.LAI.proc$LAI = (MODIS.LAI.proc$LAI)/8
MODIS.LAI.temp = merge(x= Date.df, y= MODIS.LAI.proc, by= 'DATE', all.x = T)
MODIS.LAI.filled = fill(MODIS.LAI.temp, LAI)
LAI = MODIS.LAI.filled$LAI
LAI <-LAI[!is.na(LAI)]
#plot(LAI, type = "l")

# x<-yday(as.Date(MODIS.LAI$calendar_date))
# y<-(MODIS.LAI$value)/8
# 
# df <- data.frame(x=x, y=y)
# tree <- rpart(y ~ x, data=df, control=rpart.control(minsplit=MODIS.phase.duration))
# 
# plot_tree <- function(tree, x, y) {
#   s <- seq(1, n.days , by=1)
#   #plot(x, y, main = "LAI")
#   #lines(s, predict(tree, data.frame(x=s)))
#   kc = predict(tree, data.frame(x=s))
#   return(kc)
# }
# LAI = plot_tree(tree, x, y)
# apply Aston (1979) regression equation to calculate how much prcp is being intercepted due to vegetation (note this is a very generalized equation, but based on empirical study)
# % water intercepted = 0.0653:LAI
interception.rate = LAI * .0653

options(warn=0)