# subset met data
met.data.sub = subset(met.data, SITE==site & DATE >= spinup.range[1] & DATE <= spinup.range[length(spinup.range)])
met.data.months = subset(met.data, SITE==site & YEAR ==  year(spinup.range[1]))
met.data.sub.monthly = aggregate(met.data.months, by=list(met.data.months$MONTH,met.data.months$YEAR),FUN=mean, na.rm=TRUE)



# subset modis data and create crop coefficient

MODIS.ET = subset(MODIS, calendar_date >= spinup.range[1] & calendar_date <= spinup.range[length(spinup.range)] & band == "ET_500m")
MODIS.PET = subset(MODIS, calendar_date >= spinup.range[1] & calendar_date <= spinup.range[length(spinup.range)] & band == "PET_500m")


# number of days for kc estimation 
n.days = length(spinup.range) + 1
# created generalized model of kc frm MODIS values
#actual ET
x<-yday(as.Date(MODIS.ET$calendar_date))
y<-(MODIS.ET$value * 0.1)/8

df <- data.frame(x=x, y=y)
tree <- rpart(y ~ x, data=df, control=rpart.control(minsplit=5))

plot_tree <- function(tree, x, y) {
  s <- seq(1, n.days, by=1)
  plot(x, y, main = "Actual ET")
  lines(s, predict(tree, data.frame(x=s)))
  kc = predict(tree, data.frame(x=s))
  return(kc)
}
Actual.ET = plot_tree(tree, x, y)

# created generalized model of kc frm MODIS values
#Potential ET
x<-yday(as.Date(MODIS.PET$calendar_date))
y<-(MODIS.PET$value * 0.1)/8

df <- data.frame(x=x, y=y)
tree <- rpart(y ~ x, data=df, control=rpart.control(minsplit=5))

plot_tree <- function(tree, x, y) {
  s <- seq(1, n.days , by=1)
  plot(x, y, main = "Potenial ET")
  lines(s, predict(tree, data.frame(x=s)))
  kc = predict(tree, data.frame(x=s))
  return(kc)
}
Potential.ET = plot_tree(tree, x, y)

kc = Actual.ET / Potential.ET

#plot(kc, type = "l")