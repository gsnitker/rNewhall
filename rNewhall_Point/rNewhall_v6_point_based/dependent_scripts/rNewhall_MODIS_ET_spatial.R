#download modis
MODIS <- mt_subset(product = "MOD16A2",
                      lat = lat,
                      lon = long,
                      band = c("ET_500m", "PET_500m"),
                      start = date.range[1],
                      end = date.range[length(date.range)],
                      progress = T)

MODIS$value[MODIS$value == 32765 | MODIS$value == 32764  ] = NA
MODIS$value[MODIS$value == 32767 | MODIS$value == 32766  | MODIS$value == 32762 | MODIS$value == 32761] = NA #Might need to mak this into NA

save(MODIS, file = "./data/MODIS_ET_MARE.Rdata")

MODIS.ET = subset(MODIS, band == "ET_500m")
MODIS.PET = subset(MODIS, band == "PET_500m")

# created generalized model of kc frm MODIS values
#actual ET
x<-yday(as.Date(MODIS.ET$calendar_date))
y<-(MODIS.ET$value * 0.1)/8

df <- data.frame(x=x, y=y)
tree <- rpart(y ~ x, data=df, control=rpart.control(minsplit=10))

plot_tree <- function(tree, x, y) {
  s <- seq(1, 365, by=1)
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
tree <- rpart(y ~ x, data=df, control=rpart.control(minsplit=10))

plot_tree <- function(tree, x, y) {
  s <- seq(1, 365, by=1)
  plot(x, y, main = "Potenial ET")
  lines(s, predict(tree, data.frame(x=s)))
  kc = predict(tree, data.frame(x=s))
  return(kc)
}
Potential.ET = plot_tree(tree, x, y)

kc = Actual.ET / Potential.ET

plot(kc, type = "l")